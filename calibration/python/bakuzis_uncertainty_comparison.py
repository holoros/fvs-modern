#!/usr/bin/env python3
"""
Bakuzis Matrix with Parametric Uncertainty: Default vs. Calibrated
===================================================================

Extends bakuzis_100yr_comparison.py to include posterior draw ensembles
for long term (100 year) projection uncertainty quantification.

For each of 36 scenarios (4 species groups x 3 sites x 3 densities) and
each variant, runs:
  - 1 default parameter projection
  - 1 calibrated MAP projection (posterior median)
  - N posterior draw projections (credible bands)

The posterior draws propagate parametric uncertainty from the Bayesian
calibration through to stand level BA, TPA, volume, and mortality
trajectories over 100 years. The ensemble spread represents the range
of plausible outcomes given the posterior.

This is complementary to perseus_uncertainty_projection.py: Perseus runs
plot level projections from actual FIA remeasurements, whereas this
script uses a synthetic factorial design to expose how uncertainty
scales with species, site quality, and initial density.

Output (long format CSV, one row per year x scenario x config x draw):
  year, scenario, species_group, site_class, density_class,
  variant, config, draw_id, age, tpa, atba, tcuft, mcuft, mort, ...

  config is one of: default, calibrated_map, posterior
  draw_id is NaN for default/calibrated_map, 0..N-1 for posterior

Usage:
  python bakuzis_uncertainty_comparison.py --variant ne --n-draws 50
  python bakuzis_uncertainty_comparison.py --all-variants --n-draws 100 --seed 42
  python bakuzis_uncertainty_comparison.py --variant ne --n-draws 50 \
      --batch-id 0 --batch-size 6   # for SLURM array jobs

Author: A. Weiskittel
Date: 2026-04-23
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import sqlite3
import sys
import tempfile
import time
from pathlib import Path
from typing import Optional

import numpy as np
import pandas as pd

# Import shared scaffolding from the point estimate version
SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))

from bakuzis_100yr_comparison import (  # noqa: E402
    DENSITY_CLASSES,
    KEYFILE_TEMPLATE,
    SITE_CLASSES,
    SPECIES_GROUPS,
    generate_synthetic_stand,
)

# ---------------------------------------------------------------------------
# Environment and path setup (no hardcoded Cardinal paths; all via env)
# ---------------------------------------------------------------------------

PROJECT_ROOT = os.environ.get(
    "FVS_PROJECT_ROOT",
    str(Path(__file__).resolve().parents[2]),
)
sys.path.insert(0, PROJECT_ROOT)
sys.path.insert(0, os.path.join(PROJECT_ROOT, "deployment", "fvs2py"))
sys.path.insert(0, os.path.join(PROJECT_ROOT, "deployment", "microfvs"))

FVS_LIB_DIR = os.environ.get("FVS_LIB_DIR", os.path.join(PROJECT_ROOT, "lib"))
CONFIG_DIR = os.environ.get("FVS_CONFIG_DIR", os.path.join(PROJECT_ROOT, "config"))
OUTPUT_DIR = os.environ.get(
    "BAKUZIS_OUTPUT_DIR",
    os.path.join(PROJECT_ROOT, "calibration", "output", "bakuzis"),
)

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
)
logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Scenario enumeration
# ---------------------------------------------------------------------------

def enumerate_scenarios() -> list[tuple[str, str, str, int]]:
    """Return the full factorial of scenarios as (species, site, density, id).

    Scenario ids are 1 based and stable across batches so that SLURM
    array jobs can address specific scenarios by id.
    """
    scenarios = []
    scenario_id = 0
    for sp_key in SPECIES_GROUPS:
        for site_key in SITE_CLASSES:
            for dens_key in DENSITY_CLASSES:
                scenario_id += 1
                scenarios.append((sp_key, site_key, dens_key, scenario_id))
    return scenarios


def slice_batch(
    scenarios: list[tuple[str, str, str, int]],
    batch_id: int,
    batch_size: int,
) -> list[tuple[str, str, str, int]]:
    """Return the batch_id'th contiguous slice of size batch_size."""
    if batch_size <= 0:
        return scenarios
    start = batch_id * batch_size
    end = start + batch_size
    return scenarios[start:end]


# ---------------------------------------------------------------------------
# FVS runner (extended for posterior draws)
# ---------------------------------------------------------------------------

def run_scenario(
    stand_df: pd.DataFrame,
    tree_df: pd.DataFrame,
    stand_id: str,
    variant: str,
    config_version: str,
    draw_keywords: Optional[str] = None,
) -> pd.DataFrame:
    """Run one FVS scenario and return the summary DataFrame.

    Args:
        stand_df, tree_df: synthetic stand tables.
        stand_id: unique scenario identifier (used in STDIDENT).
        variant: FVS variant code (lowercased for library name).
        config_version: 'default' | 'calibrated' | 'posterior'.
        draw_keywords: If config_version == 'posterior', pass the keyword
            block from UncertaintyEngine.generate_keywords_for_draw().
            Ignored for 'default' and 'calibrated'.
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        db_path = os.path.join(tmpdir, "FVS_Data.db")
        conn = sqlite3.connect(db_path)
        stand_df.to_sql("fvs_standinit", conn, if_exists="replace", index=False)
        tree_df.to_sql("fvs_treeinit", conn, if_exists="replace", index=False)
        conn.close()

        # Build the calibration keyword block
        cal_keywords = ""
        if config_version == "calibrated":
            try:
                from config.config_loader import FvsConfigLoader
                loader = FvsConfigLoader(
                    variant.lower(),
                    version="calibrated",
                    config_dir=CONFIG_DIR,
                )
                cal_keywords = loader.generate_keywords(include_comments=False)
            except Exception as e:
                logger.warning(f"Could not load calibrated config: {e}")
        elif config_version == "posterior" and draw_keywords:
            cal_keywords = draw_keywords

        keyfile_content = KEYFILE_TEMPLATE.format(
            stand_id=stand_id,
            db_path=db_path,
            calibration_keywords=cal_keywords or "** DEFAULT PARAMETERS",
        )

        keyfile_path = os.path.join(tmpdir, f"{variant}_{stand_id}.key")
        with open(keyfile_path, "w") as f:
            f.write(keyfile_content)

        # Shared library pathway (preferred)
        lib_path = os.path.join(FVS_LIB_DIR, f"FVS{variant.lower()}.so")
        if os.path.exists(lib_path):
            from fvs2py import FVS
            fvs = FVS(
                lib_path=lib_path,
                config_version=(
                    "calibrated" if config_version in ("calibrated", "posterior") else "default"
                ),
                config_dir=CONFIG_DIR,
            )
            fvs.load_keyfile(keyfile_path)
            fvs.run()
            summary = fvs.summary
            if summary is not None:
                return summary.copy()
            return pd.DataFrame()

        # Subprocess fallback
        import subprocess
        exe_path = f"/usr/local/bin/FVS{variant.lower()}"
        if os.path.exists(exe_path):
            subprocess.run(
                [exe_path, f"--keywordfile={os.path.basename(keyfile_path)}"],
                capture_output=True, cwd=tmpdir, timeout=180,
            )
            if os.path.exists(db_path):
                conn = sqlite3.connect(db_path)
                try:
                    return pd.read_sql_query("SELECT * FROM FVS_Summary2", conn)
                except Exception:
                    return pd.DataFrame()
                finally:
                    conn.close()

        logger.error(f"FVS not available for variant {variant}; expected {lib_path}")
        return pd.DataFrame()


# ---------------------------------------------------------------------------
# Main driver
# ---------------------------------------------------------------------------

def run_all(
    variant: str,
    n_draws: int,
    seed: int,
    batch_id: int,
    batch_size: int,
    output_dir: str,
) -> pd.DataFrame:
    """Run the full scenario x config x draw matrix for one variant.

    Returns a long DataFrame with columns identifying scenario, config,
    draw_id, and all FVS_Summary2 outputs.
    """
    np.random.seed(seed)

    # Initialize UncertaintyEngine once per variant
    engine = None
    default_config = None
    draw_indices: list[int] = []
    if n_draws > 0:
        try:
            from config.uncertainty import UncertaintyEngine
            from config.config_loader import FvsConfigLoader
            engine = UncertaintyEngine(
                variant.lower(), config_dir=CONFIG_DIR, seed=seed,
            )
            if not engine.draws_available:
                logger.warning(
                    f"No posterior draws file for variant {variant}; "
                    "running default + calibrated MAP only"
                )
                engine = None
            else:
                avail = engine.n_draws
                logger.info(f"Variant {variant}: {avail} posterior draws available")
                # Sample without replacement if n_draws <= avail, else with replacement
                if n_draws <= avail:
                    draw_indices = list(
                        np.random.default_rng(seed).choice(avail, size=n_draws, replace=False)
                    )
                else:
                    draw_indices = list(
                        np.random.default_rng(seed).choice(avail, size=n_draws, replace=True)
                    )
                # Load default config for the keyword generator
                default_loader = FvsConfigLoader(
                    variant.lower(), version="default", config_dir=CONFIG_DIR,
                )
                default_config = default_loader.config
        except Exception as e:
            logger.warning(f"UncertaintyEngine setup failed for {variant}: {e}")
            engine = None

    scenarios = slice_batch(enumerate_scenarios(), batch_id, batch_size)
    logger.info(
        f"Variant {variant}: {len(scenarios)} scenarios "
        f"(batch {batch_id} of size {batch_size})"
    )

    all_rows = []

    for sp_key, site_key, dens_key, scenario_num in scenarios:
        stand_id = f"BK{scenario_num:02d}"
        logger.info(
            f"  Scenario {scenario_num}: {sp_key} / {site_key} / {dens_key}"
        )

        stand_df, tree_df = generate_synthetic_stand(
            SPECIES_GROUPS[sp_key],
            SITE_CLASSES[site_key],
            DENSITY_CLASSES[dens_key],
            stand_id,
            variant,
        )

        # -- Default and calibrated MAP --------------------------------------
        for config_label, config_version in [
            ("default", "default"),
            ("calibrated_map", "calibrated"),
        ]:
            try:
                summary = run_scenario(
                    stand_df, tree_df, stand_id, variant, config_version,
                )
                if summary is not None and not summary.empty:
                    summary = summary.copy()
                    summary["species_group"] = sp_key
                    summary["site_class"] = site_key
                    summary["density_class"] = dens_key
                    summary["variant"] = variant.upper()
                    summary["config"] = config_label
                    summary["draw_id"] = np.nan
                    summary["scenario"] = scenario_num
                    all_rows.append(summary)
            except Exception as e:
                logger.error(f"    {config_label} failed: {e}")

        # -- Posterior draws -------------------------------------------------
        if engine is not None and default_config is not None:
            for i, draw_idx in enumerate(draw_indices):
                try:
                    draw = engine.get_draw(int(draw_idx))
                    draw_kw = engine.generate_keywords_for_draw(
                        draw, default_config, draw_idx=int(draw_idx),
                    )
                    summary = run_scenario(
                        stand_df, tree_df, stand_id, variant,
                        "posterior", draw_keywords=draw_kw,
                    )
                    if summary is not None and not summary.empty:
                        summary = summary.copy()
                        summary["species_group"] = sp_key
                        summary["site_class"] = site_key
                        summary["density_class"] = dens_key
                        summary["variant"] = variant.upper()
                        summary["config"] = "posterior"
                        summary["draw_id"] = int(draw_idx)
                        summary["scenario"] = scenario_num
                        all_rows.append(summary)
                except Exception as e:
                    logger.error(f"    draw {draw_idx} failed: {e}")

                # Progress log every 10 draws
                if (i + 1) % 10 == 0:
                    logger.info(f"    draws done: {i + 1}/{len(draw_indices)}")

    if not all_rows:
        return pd.DataFrame()

    df = pd.concat(all_rows, ignore_index=True)

    # Long form output file name
    suffix = f"_batch{batch_id:02d}" if batch_size > 0 else ""
    outfile = os.path.join(
        output_dir,
        f"bakuzis_uncertainty_{variant.lower()}_n{n_draws}{suffix}.csv",
    )
    os.makedirs(output_dir, exist_ok=True)
    df.to_csv(outfile, index=False)
    logger.info(f"Wrote {len(df)} rows to {outfile}")
    return df


def main():
    parser = argparse.ArgumentParser(
        description="Bakuzis matrix 100 year uncertainty comparison",
    )
    parser.add_argument("--variant", type=str, default="ne")
    parser.add_argument(
        "--all-variants", action="store_true",
        help="Run both NE and ACD (extend with more as needed)",
    )
    parser.add_argument(
        "--n-draws", type=int, default=50,
        help="Number of posterior draws per scenario (0 to skip posterior)",
    )
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument(
        "--batch-id", type=int, default=0,
        help="SLURM array index; 0 for single run",
    )
    parser.add_argument(
        "--batch-size", type=int, default=0,
        help="Scenarios per batch (0 means all 36)",
    )
    parser.add_argument("--output-dir", type=str, default=OUTPUT_DIR)
    args = parser.parse_args()

    variants = ["ne", "acd"] if args.all_variants else [args.variant]

    t0 = time.time()
    total_rows = 0
    for variant in variants:
        logger.info(f"\n{'=' * 60}")
        logger.info(f"Variant: {variant.upper()}")
        logger.info(f"{'=' * 60}")
        df = run_all(
            variant,
            n_draws=args.n_draws,
            seed=args.seed,
            batch_id=args.batch_id,
            batch_size=args.batch_size,
            output_dir=args.output_dir,
        )
        total_rows += len(df)

    elapsed = time.time() - t0
    logger.info(f"\nTotal rows: {total_rows} in {elapsed:.0f}s")


if __name__ == "__main__":
    main()
