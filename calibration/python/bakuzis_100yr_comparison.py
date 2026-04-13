#!/usr/bin/env python3
"""
Bakuzis Matrix: 100-Year FVS Default vs. Calibrated Comparison
==============================================================

Runs 100-year FVS projections for a factorial design of:
  4 species groups x 3 site classes x 3 initial densities = 36 scenarios

Each scenario is projected with both default and calibrated (v3) parameters
to evaluate long-term divergence, particularly the contributions of ClimateSI
and SDImax that are negligible at short (6.5-year FIA) horizons.

Comparison metrics:
  - BA trajectory over 100 years (5-year steps)
  - Mortality onset timing (year RDI first exceeds 0.55)
  - Volume at rotation age (50, 80, 100 years)
  - TPA trajectory

Output: CSV for figure generation + Bakuzis-style matrix figure (patchwork)

Usage:
  python bakuzis_100yr_comparison.py --variant ne
  python bakuzis_100yr_comparison.py --variant acd --all-variants

Author: A. Weiskittel
Date: 2026-04-09
"""

import argparse
import json
import logging
import os
import sqlite3
import sys
import tempfile
import time
from itertools import product
from pathlib import Path

import numpy as np
import pandas as pd

# Path setup
PROJECT_ROOT = os.environ.get(
    "FVS_PROJECT_ROOT",
    str(Path(__file__).resolve().parents[2])
)
sys.path.insert(0, PROJECT_ROOT)
sys.path.insert(0, os.path.join(PROJECT_ROOT, "deployment", "fvs2py"))
sys.path.insert(0, os.path.join(PROJECT_ROOT, "deployment", "microfvs"))

FVS_LIB_DIR = os.environ.get("FVS_LIB_DIR", "/usr/local/lib/fvs")
CONFIG_DIR = os.environ.get("FVS_CONFIG_DIR", os.path.join(PROJECT_ROOT, "config"))
OUTPUT_DIR = os.environ.get(
    "BAKUZIS_OUTPUT_DIR",
    os.path.join(PROJECT_ROOT, "calibration", "output", "bakuzis")
)

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
)
logger = logging.getLogger(__name__)

# ===========================================================================
# Bakuzis Matrix Design
# ===========================================================================

# Species groups: representative dominant species for Northeast/Acadian
SPECIES_GROUPS = {
    "Spruce-Fir": {
        "label": "Spruce-Fir",
        "species": [
            {"spcd": 12,  "pct": 40, "name": "Balsam fir"},
            {"spcd": 97,  "pct": 35, "name": "Red spruce"},
            {"spcd": 95,  "pct": 15, "name": "Black spruce"},
            {"spcd": 371, "pct": 10, "name": "Yellow birch"},
        ],
        "site_species": 12,
        "fortypcd": 121,  # Balsam fir
    },
    "Northern-Hardwood": {
        "label": "Northern Hardwood",
        "species": [
            {"spcd": 318, "pct": 35, "name": "Sugar maple"},
            {"spcd": 371, "pct": 25, "name": "Yellow birch"},
            {"spcd": 531, "pct": 20, "name": "American beech"},
            {"spcd": 241, "pct": 20, "name": "White ash"},
        ],
        "site_species": 318,
        "fortypcd": 801,  # Sugar maple-beech-yellow birch
    },
    "Pine": {
        "label": "White Pine",
        "species": [
            {"spcd": 129, "pct": 60, "name": "White pine"},
            {"spcd": 316, "pct": 15, "name": "Red maple"},
            {"spcd": 12,  "pct": 15, "name": "Balsam fir"},
            {"spcd": 261, "pct": 10, "name": "Eastern hemlock"},
        ],
        "site_species": 129,
        "fortypcd": 103,  # White pine
    },
    "Oak-Pine": {
        "label": "Oak-Pine",
        "species": [
            {"spcd": 833, "pct": 35, "name": "Red oak"},
            {"spcd": 129, "pct": 25, "name": "White pine"},
            {"spcd": 316, "pct": 20, "name": "Red maple"},
            {"spcd": 802, "pct": 20, "name": "White oak"},
        ],
        "site_species": 833,
        "fortypcd": 501,  # White-red-jack pine / oak
    },
}

# Site quality classes
SITE_CLASSES = {
    "Low":    {"SI": 45, "elev": 1800, "label": "Low (SI=45)"},
    "Medium": {"SI": 60, "elev": 800,  "label": "Medium (SI=60)"},
    "High":   {"SI": 75, "elev": 400,  "label": "High (SI=75)"},
}

# Initial density classes (BA in ft2/ac)
DENSITY_CLASSES = {
    "Low":    {"BA": 60,  "TPA": 150, "label": "Low (BA=60)"},
    "Medium": {"BA": 120, "TPA": 250, "label": "Medium (BA=120)"},
    "High":   {"BA": 180, "TPA": 400, "label": "High (BA=180)"},
}

# ===========================================================================
# Synthetic stand generator
# ===========================================================================

def generate_synthetic_stand(
    species_group: dict,
    site_class: dict,
    density_class: dict,
    stand_id: str,
    variant: str = "ne",
    inv_year: int = 2000,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Generate synthetic FVS standinit and treeinit tables.

    Creates a plausible tree list matching the target species composition,
    BA, and TPA. Uses inverse J-shaped diameter distribution.

    Returns:
        (stand_init_df, tree_init_df)
    """
    target_ba = density_class["BA"]
    target_tpa = density_class["TPA"]
    si = site_class["SI"]
    elev = site_class["elev"]
    species_list = species_group["species"]

    # Generate tree list with inverse-J diameter distribution
    trees = []
    tree_id = 1

    for sp_info in species_list:
        spcd = sp_info["spcd"]
        pct = sp_info["pct"] / 100.0
        sp_tpa = target_tpa * pct
        sp_ba = target_ba * pct

        # Solve for QMD from BA and TPA: QMD = sqrt(BA * 576 / (pi * TPA))
        if sp_tpa > 0:
            qmd = np.sqrt(sp_ba * 576 / (np.pi * sp_tpa))
        else:
            continue

        # Generate a diameter distribution around QMD
        n_trees = max(5, int(sp_tpa / 6.0))  # ~6 TPA per record
        diameters = np.random.lognormal(
            mean=np.log(qmd), sigma=0.35, size=n_trees
        )
        diameters = np.clip(diameters, 1.0, 40.0)

        # Compute TPA per tree record to match target
        tpa_each = sp_tpa / n_trees

        for dbh in diameters:
            # Rough height from species-generic H-D curve
            # Using simple allometry: H = 4.5 + b1 * (1 - exp(-b2 * DBH))^b3
            ht = 4.5 + (si * 1.1) * (1 - np.exp(-0.04 * dbh)) ** 1.2
            ht = max(10, min(ht, 120))

            # Crown ratio: decreases with competition
            cr = max(15, min(80, int(70 - 0.15 * target_ba + 0.3 * dbh)))

            trees.append({
                "stand_id": stand_id,
                "plot_id": 1,
                "tree_id": tree_id,
                "tree_count": round(tpa_each, 2),
                "species": spcd,
                "diameter": round(dbh, 1),
                "ht": round(ht, 0),
                "crratio": cr,
            })
            tree_id += 1

    tree_df = pd.DataFrame(trees)

    # Stand init
    stand_df = pd.DataFrame([{
        "stand_id": stand_id,
        "variant": variant.upper(),
        "inv_year": inv_year,
        "latitude": 45.0,
        "longitude": -69.0,
        "region": 9,
        "forest": 0,
        "district": 0,
        "basal_area_factor": 0.0,
        "inv_plot_size": 6.0,
        "brk_dbh": 5.0,
        "num_plots": 1,
        "age": 40,
        "aspect": 180,
        "slope": 15,
        "elevft": elev,
        "site_species": species_group["site_species"],
        "site_index": si,
        "state": 23,
        "county": 19,
        "forest_type": species_group["fortypcd"],
        "sam_wt": 1.0,
    }])

    return stand_df, tree_df


# ===========================================================================
# FVS runner (same pattern as PERSEUS script)
# ===========================================================================

KEYFILE_TEMPLATE = """STDIDENT
{stand_id}

DATABASE
DSNIN
{db_path}

DSNOUT
{db_path}

STANDSQL
SELECT *
FROM fvs_standinit
WHERE stand_id = '%StandID%'
ENDSQL

TREESQL
SELECT *
FROM fvs_treeinit
WHERE stand_id = '%StandID%'
ENDSQL

END

DATABASE
SUMMARY            2
TREELIDB           2         2
CARBREDB           2
END

CALBSTAT
TREELIST           0
ECHOSUM

TIMEINT            0         5
NUMCYCLE          20

{calibration_keywords}

PROCESS
STOP
"""


def run_scenario(stand_df, tree_df, stand_id, variant, config_version):
    """Run one FVS scenario and return summary DataFrame."""
    with tempfile.TemporaryDirectory() as tmpdir:
        db_path = os.path.join(tmpdir, "FVS_Data.db")
        conn = sqlite3.connect(db_path)
        stand_df.to_sql("fvs_standinit", conn, if_exists="replace", index=False)
        tree_df.to_sql("fvs_treeinit", conn, if_exists="replace", index=False)
        conn.close()

        cal_keywords = ""
        if config_version == "calibrated":
            try:
                from config.config_loader import FvsConfigLoader
                loader = FvsConfigLoader(
                    variant.lower(), version="calibrated", config_dir=CONFIG_DIR
                )
                cal_keywords = loader.generate_keywords(include_comments=False)
            except Exception as e:
                logger.warning(f"Could not load calibrated config: {e}")

        keyfile_content = KEYFILE_TEMPLATE.format(
            stand_id=stand_id,
            db_path=db_path,
            calibration_keywords=cal_keywords or "** DEFAULT PARAMETERS",
        )

        keyfile_path = os.path.join(tmpdir, f"{variant}_{stand_id}.key")
        with open(keyfile_path, "w") as f:
            f.write(keyfile_content)

        # Try shared library
        lib_path = os.path.join(FVS_LIB_DIR, f"FVS{variant.lower()}.so")
        if os.path.exists(lib_path):
            from fvs2py import FVS
            fvs = FVS(
                lib_path=lib_path,
                config_version=config_version,
                config_dir=CONFIG_DIR,
            )
            fvs.load_keyfile(keyfile_path)
            fvs.run()
            summary = fvs.summary
            if summary is not None:
                return summary.copy()
            return pd.DataFrame()

        # Fallback to subprocess
        import subprocess
        exe_path = f"/usr/local/bin/FVS{variant.lower()}"
        if os.path.exists(exe_path):
            result = subprocess.run(
                [exe_path, f"--keywordfile={os.path.basename(keyfile_path)}"],
                capture_output=True, cwd=tmpdir, timeout=120,
            )
            if os.path.exists(db_path):
                conn = sqlite3.connect(db_path)
                try:
                    summary = pd.read_sql_query("SELECT * FROM FVS_Summary2", conn)
                    return summary
                except Exception:
                    pass
                finally:
                    conn.close()

        logger.error(f"FVS not available for variant {variant}")
        return pd.DataFrame()


# ===========================================================================
# Main
# ===========================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Bakuzis matrix 100-year FVS comparison"
    )
    parser.add_argument(
        "--variant", type=str, default="ne",
        help="FVS variant to use"
    )
    parser.add_argument(
        "--all-variants", action="store_true",
        help="Run for both NE and ACD"
    )
    parser.add_argument(
        "--seed", type=int, default=42,
        help="Random seed for synthetic stand generation"
    )
    parser.add_argument(
        "--output-dir", type=str, default=OUTPUT_DIR,
        help="Output directory"
    )
    args = parser.parse_args()

    np.random.seed(args.seed)
    os.makedirs(args.output_dir, exist_ok=True)

    variants = ["ne", "acd"] if args.all_variants else [args.variant]

    all_results = []
    t0 = time.time()

    for variant in variants:
        logger.info(f"\n{'='*60}")
        logger.info(f"Running Bakuzis matrix for variant: {variant.upper()}")
        logger.info(f"{'='*60}")

        scenario_num = 0
        for sp_key, sp_group in SPECIES_GROUPS.items():
            for site_key, site_cls in SITE_CLASSES.items():
                for dens_key, dens_cls in DENSITY_CLASSES.items():
                    scenario_num += 1
                    stand_id = f"BK{scenario_num:02d}"

                    logger.info(
                        f"  Scenario {scenario_num}/36: "
                        f"{sp_key} / {site_key} / {dens_key}"
                    )

                    stand_df, tree_df = generate_synthetic_stand(
                        sp_group, site_cls, dens_cls,
                        stand_id, variant,
                    )

                    for config in [None, "calibrated"]:
                        config_label = config or "default"

                        try:
                            summary = run_scenario(
                                stand_df, tree_df, stand_id,
                                variant, config,
                            )

                            if summary is not None and not summary.empty:
                                summary = summary.copy()
                                summary["species_group"] = sp_key
                                summary["site_class"] = site_key
                                summary["density_class"] = dens_key
                                summary["variant"] = variant.upper()
                                summary["config"] = config_label
                                summary["scenario"] = scenario_num
                                all_results.append(summary)
                        except Exception as e:
                            logger.error(
                                f"  FAILED: {sp_key}/{site_key}/{dens_key}"
                                f"/{config_label}: {e}"
                            )

    elapsed = time.time() - t0

    if all_results:
        results_df = pd.concat(all_results, ignore_index=True)
        outfile = os.path.join(args.output_dir, "bakuzis_100yr_results.csv")
        results_df.to_csv(outfile, index=False)
        logger.info(f"\nResults: {len(results_df)} rows -> {outfile}")
        logger.info(f"Total time: {elapsed:.0f}s")

        # Print summary
        print("\n" + "=" * 80)
        print("Bakuzis Matrix Summary: BA at Year 50 and Year 100")
        print("=" * 80)

        if "year" in results_df.columns:
            yr_col = "year"
        elif "Year" in results_df.columns:
            yr_col = "Year"
        else:
            yr_col = results_df.columns[0]

        ba_col = "atba" if "atba" in results_df.columns else "ATBAftr"

        for yr in [2050, 2100]:
            subset = results_df.loc[results_df[yr_col] == yr]
            if subset.empty:
                continue
            pivot = subset.pivot_table(
                index=["species_group", "site_class", "density_class"],
                columns="config",
                values=ba_col,
            )
            print(f"\nBA (ft2/ac) at year {yr}:")
            print(pivot.to_string(float_format="{:.1f}".format))
    else:
        logger.warning("No results produced.")


if __name__ == "__main__":
    main()
