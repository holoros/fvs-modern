#!/usr/bin/env python3
from __future__ import annotations
"""
PERSEUS 100-Year FVS Projection for Maine FIA Plots
====================================================

Runs FVS-ACD and FVS-NE (default and calibrated) for 3,632 Maine PERSEUS
plots over 100 years using fvs2py. Extracts NSBE total aboveground biomass
at each 10-year step. Post-processing aggregates tree-level biomass into
state-level megatonnes (MMT) for comparison with other PERSEUS models.

Pipeline:
  1. Read PERSEUS CSV to get FIRST_PLTCN for each plot
  2. Load FIA tree data for those PLT_CNs from the state SQLite database
  3. For each plot x variant x config combination:
     a. Populate FVS standinit + treeinit SQLite tables
     b. Generate keyword file (10 cycles x 10 years = 100 years)
     c. Run FVS via fvs2py shared library
     d. Extract tree lists at each cycle from TREELIDB
     e. Apply NSBE biomass equations per tree
     f. Compute per-plot AGB: sum(biomass_kg * TPA)
  4. Output: CSV with PLOT x YEAR x VARIANT x CONFIG x AGB

Designed for SLURM array jobs on Cardinal HPC.

Usage:
  python perseus_100yr_projection.py --batch-id 1 --batch-size 100
  python perseus_100yr_projection.py --all  # (single machine, all plots)

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
from pathlib import Path

import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# Environment and path setup
# ---------------------------------------------------------------------------

PROJECT_ROOT = os.environ.get(
    "FVS_PROJECT_ROOT",
    str(Path(__file__).resolve().parents[2])
)
sys.path.insert(0, PROJECT_ROOT)
sys.path.insert(0, os.path.join(PROJECT_ROOT, "deployment", "fvs2py"))
sys.path.insert(0, os.path.join(PROJECT_ROOT, "deployment", "microfvs"))

FIA_DIR = os.environ.get("FVS_FIA_DATA_DIR", os.path.expanduser("~/fia_data"))
FVS_LIB_DIR = os.environ.get("FVS_LIB_DIR", os.path.join(PROJECT_ROOT, "lib"))
NSBE_ROOT = os.environ.get("NSBE_ROOT", os.path.join(PROJECT_ROOT, "data", "NSBE"))
CONFIG_DIR = os.environ.get("FVS_CONFIG_DIR", os.path.join(PROJECT_ROOT, "config"))
OUTPUT_DIR = os.environ.get(
    "PERSEUS_OUTPUT_DIR",
    os.path.join(PROJECT_ROOT, "calibration", "output", "perseus")
)
PERSEUS_CSV = os.environ.get(
    "PERSEUS_CSV",
    os.path.join(PROJECT_ROOT, "calibration", "data", "perseus_plots.csv")
)

# FVS keyword template for 100-year no-treatment projection
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
COMPUTDB
END

CALBSTAT
TREELIST           0
ECHOSUM

TIMEINT            0{cycle_length:>10d}
NUMCYCLE{num_cycles:>10d}

{calibration_keywords}

PROCESS
STOP
"""

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
logger = logging.getLogger(__name__)


# ===========================================================================
# NSBE Biomass Equations (Python port of R script 20)
# ===========================================================================

class NSBECalculator:
    """NSBE/VTECO volume and biomass equations (Westfall et al. 2024).

    Implements the hierarchical coefficient lookup:
      SPCD_DIVISION > SPCD > JENKINS_SPGRPCD
    """

    def __init__(self, nsbe_root: str):
        self.nsbe_root = Path(nsbe_root)
        self._load_coefficients()

    def _load_coefficients(self):
        """Load NSBE coefficient files."""
        self.ref_species = pd.read_csv(
            self.nsbe_root / "REF_SPECIES.csv",
            usecols=["SPCD", "GENUS", "JENKINS_SPGRPCD",
                     "WOOD_SPGR_GREENVOL_DRYWT", "BARK_SPGR_GREENVOL_DRYWT"]
        )
        self.ref_species.set_index("SPCD", inplace=True)

        coef_dir = self.nsbe_root / "Coefs" / "combined"
        self.total_biomass_coefs = pd.read_csv(coef_dir / "total_biomass_coefs.csv")
        self.volib_coefs = pd.read_csv(coef_dir / "volib_coefs.csv")
        self.volob_coefs = pd.read_csv(coef_dir / "volob_coefs.csv")

        # Normalize SPCD column to clean string representation.
        # Pandas may read numeric SPCDs as floats (e.g., "12.0"); normalize
        # these to integer strings ("12") while preserving non-numeric entries
        # like "1_111" (division-level coefficients).
        for tbl in [self.total_biomass_coefs, self.volib_coefs, self.volob_coefs]:
            if "SPCD" in tbl.columns:
                tbl["SPCD"] = tbl["SPCD"].apply(self._normalize_spcd)

        logger.info(
            f"NSBE loaded: {len(self.ref_species)} species, "
            f"{len(self.total_biomass_coefs)} biomass coefs"
        )

    @staticmethod
    def _normalize_spcd(val) -> str:
        """Normalize SPCD to consistent string form.

        Converts '12.0' -> '12', keeps '1_111' as-is.
        """
        if pd.isna(val):
            return ""
        s = str(val).strip()
        try:
            return str(int(float(s)))
        except (ValueError, OverflowError):
            return s

    def _get_coefficient(self, coef_table: pd.DataFrame, spcd: int,
                         jenkins_grp: int | None = None) -> dict | None:
        """Hierarchical lookup: SPCD > JENKINS_SPGRPCD.

        Note: SPCD column is stored as strings in the CSV (due to entries
        like '1_111'), so we compare against str(spcd) for SPCD matching.
        We also check the float-string form (e.g., '12.0') because pandas
        may read numeric SPCD values as floats before casting to str.
        """
        # Try SPCD match (column is string dtype, so compare as string)
        spcd_str = str(spcd)
        spcd_float_str = str(float(spcd))  # e.g., "12.0" for robustness
        row = coef_table.loc[
            (coef_table["SPCD"] == spcd_str) |
            (coef_table["SPCD"] == spcd_float_str)
        ]
        if len(row) > 0:
            return row.iloc[0].to_dict()

        # Fallback to Jenkins group
        if jenkins_grp is not None:
            # Match Jenkins group as either int or float representation
            jgrp_mask = (
                (coef_table["JENKINS_SPGRPCD"] == jenkins_grp) |
                (coef_table["JENKINS_SPGRPCD"] == float(jenkins_grp))
            )
            row = coef_table.loc[jgrp_mask]
            if len(row) > 0:
                return row.iloc[0].to_dict()

        return None

    @staticmethod
    def _apply_equation(dbh: float, ht: float, form: int, coefs: dict) -> float:
        """Apply NSBE equation. DBH in inches, HT in feet. Returns POUNDS.

        NSBE equations were calibrated against FIA DRYBIO_AG which is stored
        in pounds in the FIA database.  The caller (compute_tree_biomass_kg)
        handles the lbs-to-kg conversion.
        """
        try:
            form = int(form)
        except (ValueError, TypeError):
            return np.nan

        if form == 3:
            a, b, c = coefs.get("a"), coefs.get("b"), coefs.get("c")
            if any(pd.isna(x) for x in [a, b, c]):
                return np.nan
            return a * (dbh ** b) * (ht ** c)

        elif form == 4:
            a0, b0, b1, c = (coefs.get("a0"), coefs.get("b0"),
                              coefs.get("b1"), coefs.get("c"))
            if any(pd.isna(x) for x in [a0, b0, b1, c]):
                return np.nan
            k = 15.0
            if dbh < k:
                return a0 * (dbh ** b0) * (ht ** c)
            else:
                return a0 * (k ** (b0 - b1)) * (dbh ** b1) * (ht ** c)

        elif form == 5:
            a, a1, b1, c1, c = (coefs.get("a"), coefs.get("a1"),
                                 coefs.get("b1"), coefs.get("c1"), coefs.get("c"))
            if any(pd.isna(x) for x in [a, a1, b1, c1, c]):
                return np.nan
            exp_dbh = a1 * (1 - np.exp(-b1 * dbh)) ** c1
            return a * (dbh ** exp_dbh) * (ht ** c)

        elif form == 50:
            a, b, c, b2 = (coefs.get("a"), coefs.get("b"),
                           coefs.get("c"), coefs.get("b2"))
            if any(pd.isna(x) for x in [a, b, c, b2]):
                return np.nan
            return a * (dbh ** b) * (ht ** c) * np.exp(-(b2 * dbh))

        return np.nan

    def compute_tree_biomass_kg(self, spcd: int, dbh: float, ht: float) -> float:
        """Compute total AGB for a single tree in kg.

        The NSBE equations were calibrated against FIA DRYBIO_AG, which is
        stored in POUNDS in the FIA database.  The raw equation output is
        therefore in pounds; we convert to kg here (divide by 2.20462).

        Args:
            spcd: FIA species code
            dbh: diameter at breast height (inches)
            ht: total height (feet)

        Returns:
            Total aboveground dry biomass in kg, or NaN if not computable
        """
        if pd.isna(dbh) or pd.isna(ht) or dbh < 1.0 or ht <= 0:
            return np.nan

        # Get Jenkins group for fallback
        jenkins_grp = None
        if spcd in self.ref_species.index:
            jenkins_grp = self.ref_species.loc[spcd, "JENKINS_SPGRPCD"]
            if isinstance(jenkins_grp, pd.Series):
                jenkins_grp = jenkins_grp.iloc[0]

        coef = self._get_coefficient(self.total_biomass_coefs, spcd, jenkins_grp)
        if coef is None:
            return np.nan

        form = coef.get("equation", np.nan)
        biomass_lbs = self._apply_equation(dbh, ht, form, coef)
        if np.isnan(biomass_lbs):
            return np.nan
        return biomass_lbs / 2.20462  # NSBE output is pounds; convert to kg


# ===========================================================================
# FIA Data Loader
# ===========================================================================

def load_fia_trees_for_plots(plt_cns: list[str], fia_dir: str,
                              state: str = "ME") -> pd.DataFrame:
    """Load FIA tree records for a set of PLT_CN values.

    Reads from the FIA SQLite or CSV files. Returns a DataFrame with
    columns needed for FVS initialization.

    Args:
        plt_cns: list of PLT_CN strings to fetch
        fia_dir: path to FIA data directory
        state: two-letter state abbreviation

    Returns:
        DataFrame with columns: PLT_CN, SUBP, TREE, SPCD, DIA, HT,
        ACTUALHT, CR, TPA_UNADJ, STATUSCD, plus stand-level fields
    """
    # Try SQLite database first (most common on Cardinal)
    db_candidates = [
        os.path.join(fia_dir, f"{state}_FIA.db"),
        os.path.join(fia_dir, state, f"{state}_FIA.db"),
        os.path.join(fia_dir, f"FIADB_{state}.db"),
        os.path.join(fia_dir, state, f"FIADB_{state}.db"),
    ]

    for db_path in db_candidates:
        if os.path.exists(db_path):
            return _load_from_sqlite(db_path, plt_cns)

    # Fallback to CSV
    csv_candidates = [
        os.path.join(fia_dir, state, f"{state}_TREE.csv"),
        os.path.join(fia_dir, f"{state}_TREE.csv"),
    ]
    for csv_path in csv_candidates:
        if os.path.exists(csv_path):
            return _load_from_csv(csv_path, plt_cns, fia_dir, state)

    raise FileNotFoundError(
        f"Cannot find FIA data for {state} in {fia_dir}. "
        f"Tried: {db_candidates + csv_candidates}"
    )


def _load_from_sqlite(db_path: str, plt_cns: list[str]) -> pd.DataFrame:
    """Load tree and plot data from FIA SQLite database."""
    logger.info(f"Loading FIA trees from SQLite: {db_path}")
    conn = sqlite3.connect(db_path)

    # Build parameterized query for PLT_CN list
    placeholders = ",".join(["?"] * len(plt_cns))

    tree_sql = f"""
    SELECT t.PLT_CN, t.SUBP, t.TREE, t.SPCD, t.DIA, t.HT, t.ACTUALHT,
           t.CR, t.TPA_UNADJ, t.STATUSCD, t.CN as TREE_CN
    FROM TREE t
    WHERE t.PLT_CN IN ({placeholders})
      AND t.STATUSCD = 1
      AND t.DIA >= 1.0
    """

    trees = pd.read_sql_query(tree_sql, conn, params=plt_cns)

    plot_sql = f"""
    SELECT p.CN as PLT_CN, p.INVYR, p.LAT, p.LON,
           p.ELEV, p.SLOPE, p.ASPECT,
           c.SICOND, c.SISP, c.FORTYPCD, c.STDAGE, c.BALIVE,
           c.CONDID, c.COND_STATUS_CD, c.OWNGRPCD,
           c.TPAGROW_UNADJ, c.TPAMORT_UNADJ
    FROM PLOT p
    LEFT JOIN COND c ON p.CN = c.PLT_CN AND c.CONDID = 1
    WHERE p.CN IN ({placeholders})
    """

    plots = pd.read_sql_query(plot_sql, conn, params=plt_cns)
    conn.close()

    # Merge tree and plot data
    merged = trees.merge(plots, on="PLT_CN", how="left")
    logger.info(f"Loaded {len(merged)} live trees across {len(plt_cns)} plots")
    return merged


def _load_from_csv(tree_csv: str, plt_cns: list[str],
                   fia_dir: str, state: str) -> pd.DataFrame:
    """Load tree data from FIA CSV files."""
    logger.info(f"Loading FIA trees from CSV: {tree_csv}")

    # Use chunked reading for large files
    # Normalize to integer strings for matching
    plt_cn_set = set(str(int(float(cn))) for cn in plt_cns)
    # Also build int set for numeric matching
    plt_cn_int_set = set(int(float(cn)) for cn in plt_cns)
    chunks = []
    for chunk in pd.read_csv(tree_csv, chunksize=100000, low_memory=False):
        # Match on either string or int representation
        mask = chunk["PLT_CN"].isin(plt_cn_int_set) | chunk["PLT_CN"].astype(str).isin(plt_cn_set)
        if mask.any():
            subset = chunk.loc[mask & (chunk["STATUSCD"] == 1) & (chunk["DIA"] >= 1.0)]
            chunks.append(subset)

    if not chunks:
        logger.warning("No trees found for the requested PLT_CNs")
        return pd.DataFrame()

    trees = pd.concat(chunks, ignore_index=True)

    # Load PLOT and COND for stand-level attributes
    plot_csv = tree_csv.replace("_TREE.csv", "_PLOT.csv")
    cond_csv = tree_csv.replace("_TREE.csv", "_COND.csv")

    if os.path.exists(plot_csv):
        plots = pd.read_csv(plot_csv, low_memory=False)
        plots.rename(columns={"CN": "PLT_CN"}, inplace=True)
        # Ensure PLT_CN is same type as trees for merge
        plots["PLT_CN"] = plots["PLT_CN"].astype(trees["PLT_CN"].dtype)
        plots = plots.loc[plots["PLT_CN"].isin(trees["PLT_CN"].unique())]
        # LAT/LON/ELEV come from PLOT; SLOPE/ASPECT come from COND
        plot_cols = ["PLT_CN"]
        for c in ["INVYR", "LAT", "LON", "ELEV"]:
            if c in plots.columns:
                plot_cols.append(c)
        trees = trees.merge(plots[plot_cols], on="PLT_CN", how="left")

    if os.path.exists(cond_csv):
        conds = pd.read_csv(cond_csv, low_memory=False)
        # Ensure PLT_CN is same type as trees for merge
        conds["PLT_CN"] = conds["PLT_CN"].astype(trees["PLT_CN"].dtype)
        conds = conds.loc[conds["PLT_CN"].isin(trees["PLT_CN"].unique()) & (conds["CONDID"] == 1)]
        # SLOPE, ASPECT, SICOND, SISP, etc. come from COND
        cond_cols = ["PLT_CN"]
        for c in ["SLOPE", "ASPECT", "SICOND", "SISP", "FORTYPCD", "STDAGE", "BALIVE"]:
            if c in conds.columns:
                cond_cols.append(c)
        trees = trees.merge(conds[cond_cols], on="PLT_CN", how="left")

    logger.info(f"Loaded {len(trees)} live trees from CSV")
    return trees


# ===========================================================================
# FVS Projection Runner
# ===========================================================================

def build_fvs_standinit(plot_data: dict, stand_id: str,
                         variant: str) -> pd.DataFrame:
    """Build an fvs_standinit table row from FIA plot data.

    Args:
        plot_data: dict of plot-level attributes from FIA
        stand_id: stand identifier string
        variant: FVS variant code (e.g., 'acd', 'ne')

    Returns:
        Single-row DataFrame matching fvs_standinit schema
    """
    inv_year = int(plot_data.get("INVYR", 2000))
    latitude = plot_data.get("LAT", 45.0)
    longitude = plot_data.get("LON", -69.0)
    elevation = plot_data.get("ELEV", 500)
    slope = plot_data.get("SLOPE", 10)
    aspect = plot_data.get("ASPECT", 180)
    site_index = plot_data.get("SICOND", 60)
    site_species = plot_data.get("SISP", 12)  # Default balsam fir
    forest_type = plot_data.get("FORTYPCD", 101)
    stand_age = plot_data.get("STDAGE", 50)
    state = 23  # Maine FIPS code
    county = plot_data.get("COUNTYCD", 19)

    # FIA plot design: 4 subplots, 24-foot radius = 0.0415 acres each
    # BAF = 0 (fixed area plot), plot size = 1/24 acre per subplot
    basal_area_factor = 0.0
    inv_plot_size = round(1.0 / (4 * 0.04154), 1)  # ~6.0 plots per acre
    brk_dbh = 5.0  # 5-inch breakpoint diameter

    row = {
        "stand_id": stand_id,
        "variant": variant.upper(),
        "inv_year": inv_year,
        "latitude": latitude,
        "longitude": longitude,
        "region": 9,
        "forest": 0,
        "district": 0,
        "basal_area_factor": basal_area_factor,
        "inv_plot_size": inv_plot_size,
        "brk_dbh": brk_dbh,
        "num_plots": 1,
        "age": stand_age if pd.notna(stand_age) else 50,
        "aspect": aspect if pd.notna(aspect) else 0,
        "slope": slope if pd.notna(slope) else 0,
        "elevft": elevation if pd.notna(elevation) else 500,
        "site_species": int(site_species) if pd.notna(site_species) else 12,
        "site_index": int(site_index) if pd.notna(site_index) else 60,
        "state": state,
        "county": int(county) if pd.notna(county) else 19,
        "forest_type": int(forest_type) if pd.notna(forest_type) else 101,
        "sam_wt": 1.0,
    }

    return pd.DataFrame([row])


def build_fvs_treeinit(trees: pd.DataFrame, stand_id: str) -> pd.DataFrame:
    """Build fvs_treeinit table from FIA tree records.

    Args:
        trees: DataFrame with FIA tree data (DIA, HT, CR, SPCD, TPA_UNADJ, SUBP, TREE)
        stand_id: stand identifier string

    Returns:
        DataFrame matching fvs_treeinit schema
    """
    if trees.empty:
        return pd.DataFrame()

    records = []
    for _, t in trees.iterrows():
        dbh = t.get("DIA", np.nan)
        ht = t.get("HT", 0) or t.get("ACTUALHT", 0)
        cr = t.get("CR", 0)
        spcd = int(t.get("SPCD", 0))
        tpa = t.get("TPA_UNADJ", 1.0)
        subp = int(t.get("SUBP", 1))
        tree_id = int(t.get("TREE", 0))

        if pd.isna(dbh) or dbh < 1.0:
            continue

        records.append({
            "stand_id": stand_id,
            "plot_id": subp,
            "tree_id": tree_id,
            "tree_count": tpa if pd.notna(tpa) else 6.018,
            "species": spcd,
            "diameter": round(dbh, 1),
            "ht": round(ht, 0) if pd.notna(ht) and ht > 0 else 0,
            "crratio": int(cr) if pd.notna(cr) and cr > 0 else 0,
        })

    return pd.DataFrame(records)


def run_fvs_projection(stand_init_df: pd.DataFrame,
                       tree_init_df: pd.DataFrame,
                       stand_id: str,
                       variant: str,
                       config_version: str | None = None,
                       num_cycles: int = 10,
                       cycle_length: int = 10) -> dict:
    """Run a single FVS projection and return summary + tree lists.

    Args:
        stand_init_df: standinit table (single row)
        tree_init_df: treeinit table (tree records)
        stand_id: stand identifier
        variant: FVS variant code
        config_version: None/'default' or 'calibrated'
        num_cycles: number of projection cycles
        cycle_length: years per cycle

    Returns:
        dict with keys:
          'summary': DataFrame of FVS summary table
          'treelists': dict of {year: DataFrame} with tree-level data
          'exit_code': FVS exit code
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        db_path = os.path.join(tmpdir, "FVS_Data.db")
        conn = sqlite3.connect(db_path)

        stand_init_df.to_sql("fvs_standinit", conn, if_exists="replace", index=False)
        tree_init_df.to_sql("fvs_treeinit", conn, if_exists="replace", index=False)
        conn.close()

        # Generate calibration keywords if needed
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
                logger.warning(f"Could not load calibrated config for {variant}: {e}")

        keyfile_content = KEYFILE_TEMPLATE.format(
            stand_id=stand_id,
            db_path=db_path,
            calibration_keywords=cal_keywords if cal_keywords else "** DEFAULT PARAMETERS",
            num_cycles=num_cycles,
            cycle_length=cycle_length,
        )

        keyfile_path = os.path.join(tmpdir, f"{variant}_{stand_id}.key")
        with open(keyfile_path, "w") as f:
            f.write(keyfile_content)

        # Prefer subprocess execution (more robust for batch jobs)
        exe_candidates = [
            os.path.join(FVS_LIB_DIR, f"FVS{variant.lower()}"),
            os.path.join(FVS_LIB_DIR, f"FVS{variant.upper()}"),
            f"/usr/local/bin/FVS{variant.lower()}",
        ]
        for exe_path in exe_candidates:
            if os.path.exists(exe_path) and os.access(exe_path, os.X_OK):
                return _run_via_subprocess(exe_path, keyfile_path,
                                           db_path, tmpdir)

        # Fallback to fvs2py shared library (ctypes)
        lib_path = os.path.join(FVS_LIB_DIR, f"FVS{variant.lower()}.so")
        if os.path.exists(lib_path):
            return _run_via_fvs2py(lib_path, keyfile_path, db_path,
                                   variant, config_version)

        # Neither available
        raise FileNotFoundError(
            f"Cannot find FVS executable or library for {variant}. "
            f"Tried: {exe_candidates + [lib_path]}"
        )


def _run_via_fvs2py(lib_path, keyfile_path, db_path,
                     variant, config_version):
    """Run FVS using fvs2py shared library."""
    from fvs2py import FVS

    fvs = FVS(
        lib_path=lib_path,
        config_version=config_version,
        config_dir=CONFIG_DIR,
    )
    fvs.load_keyfile(keyfile_path)
    fvs.run()

    summary = fvs.summary
    exit_code = fvs.exit_code

    # Extract tree lists from output database
    treelists = _extract_treelists(db_path)

    return {
        "summary": summary,
        "treelists": treelists,
        "exit_code": exit_code,
    }


def _run_via_subprocess(exe_path, keyfile_path, db_path, tmpdir):
    """Run FVS as a subprocess.

    FVS reads four file names interactively from stdin:
      1) keyword file name   (unit 15)
      2) tree data file name (unit 02) -- unused with DATABASE/DSNIN
      3) main output file    (unit 16)
      4) treelist output     (unit 03)
    """
    import subprocess
    keyname = os.path.basename(keyfile_path)
    # Provide all 4 inputs; tree data and treelist are unused with DATABASE mode
    fvs_input = f"{keyname}\nnone.tre\nfvs_run.out\nfvs_run.trl\n"
    result = subprocess.run(
        [exe_path],
        input=fvs_input,
        capture_output=True,
        text=True,
        cwd=tmpdir,
        timeout=300,
    )

    # STOP 10 (exit code 10) is normal termination for this build
    if result.returncode not in (0, 10):
        logger.warning(f"FVS subprocess returned code {result.returncode}")
        logger.warning(f"stderr: {result.stderr[:500]}")

    # Read summary from SQLite
    summary = None
    treelists = {}
    if os.path.exists(db_path):
        treelists = _extract_treelists(db_path)
        conn = sqlite3.connect(db_path)
        try:
            summary = pd.read_sql_query("SELECT * FROM FVS_Summary2", conn)
        except Exception:
            pass
        conn.close()

    return {
        "summary": summary,
        "treelists": treelists,
        "exit_code": result.returncode,
    }


def _extract_treelists(db_path: str) -> dict[int, pd.DataFrame]:
    """Extract tree lists at each cycle from FVS output database.

    Returns dict mapping year to DataFrame of tree records with columns:
    SPECIES, DBH, HT, TPA, etc.
    """
    treelists = {}
    if not os.path.exists(db_path):
        return treelists

    conn = sqlite3.connect(db_path)
    try:
        # FVS TREELIDB columns: Year, SpeciesFIA, DBH, Ht, TPA, etc.
        # No CutList column in this version; all rows are live-tree records.
        tl = pd.read_sql_query(
            "SELECT * FROM FVS_TreeList",
            conn
        )
        if not tl.empty:
            for year, group in tl.groupby("Year"):
                treelists[int(year)] = group.copy()
    except Exception as e:
        logger.debug(f"Could not read treelist from {db_path}: {e}")
    finally:
        conn.close()

    return treelists


# ===========================================================================
# Biomass Aggregation
# ===========================================================================

def compute_plot_agb(treelist: pd.DataFrame,
                     nsbe: NSBECalculator) -> float:
    """Compute total AGB for a single plot at one time step.

    Args:
        treelist: FVS tree list DataFrame for one year
        nsbe: NSBE calculator instance

    Returns:
        Total AGB in short tons per acre
    """
    if treelist.empty:
        return 0.0

    total_agb_tons = 0.0
    for _, tree in treelist.iterrows():
        # FVS TreeList uses SpeciesFIA (3-digit code as string)
        spcd_raw = tree.get("SpeciesFIA", tree.get("Species",
                   tree.get("SPCD", 0)))
        try:
            spcd = int(spcd_raw)
        except (ValueError, TypeError):
            continue
        dbh = tree.get("DBH", tree.get("Dbh", 0))
        ht = tree.get("Ht", tree.get("HT", tree.get("TotHt", 0)))
        tpa = tree.get("TPA", tree.get("Tpa", 1.0))

        if pd.isna(dbh) or dbh < 1.0 or pd.isna(tpa):
            continue

        # NSBE returns kg per tree; convert to short tons
        biomass_kg = nsbe.compute_tree_biomass_kg(spcd, dbh, ht)
        if not np.isnan(biomass_kg):
            biomass_tons = biomass_kg / 907.185  # kg to short tons
            total_agb_tons += biomass_tons * tpa

    return total_agb_tons


def compute_initial_agb(trees: pd.DataFrame,
                         nsbe: NSBECalculator) -> float:
    """Compute initial AGB from FIA tree records.

    Args:
        trees: FIA tree records for one plot
        nsbe: NSBE calculator instance

    Returns:
        Total AGB in short tons per acre
    """
    if trees.empty:
        return 0.0

    total = 0.0
    for _, t in trees.iterrows():
        spcd = int(t["SPCD"])
        dbh = t["DIA"]
        ht = t.get("HT", 0) or t.get("ACTUALHT", 0)
        tpa = t.get("TPA_UNADJ", 1.0)

        if pd.isna(dbh) or dbh < 1.0 or pd.isna(tpa):
            continue
        if pd.isna(ht) or ht <= 0:
            continue

        biomass_kg = nsbe.compute_tree_biomass_kg(spcd, dbh, ht)
        if not np.isnan(biomass_kg):
            total += (biomass_kg / 907.185) * tpa

    return total


# ===========================================================================
# Main Pipeline
# ===========================================================================

def process_plot(
    plot_row: dict,
    fia_trees: pd.DataFrame,
    nsbe: NSBECalculator,
    variants: list[str] = ["acd", "ne"],
    configs: list[str | None] = [None, "calibrated"],
) -> list[dict]:
    """Process one PERSEUS plot: run FVS for all variant x config combos.

    Returns list of result dicts (one per year x variant x config).
    """
    plot_id = plot_row["PLOT"]
    # Ensure PLT_CN is an integer string (strip trailing .0 from float repr)
    raw_cn = plot_row["FIRST_PLTCN"]
    plt_cn = str(int(float(raw_cn))) if pd.notna(raw_cn) else ""
    stand_id = f"P{int(float(plot_id))}"

    # Filter trees for this plot (both sides to integer string)
    fia_cn_str = fia_trees["PLT_CN"].apply(
        lambda x: str(int(float(x))) if pd.notna(x) else ""
    )
    plot_trees = fia_trees.loc[fia_cn_str == plt_cn]

    if plot_trees.empty:
        logger.warning(f"Plot {plot_id}: no trees found for PLT_CN={plt_cn}")
        return []

    # Extract plot-level attributes from first tree record
    plot_data = plot_trees.iloc[0].to_dict()
    plot_data["COUNTYCD"] = plot_row.get("COUNTYCD", 19)

    results = []

    # Compute initial AGB
    initial_agb = compute_initial_agb(plot_trees, nsbe)
    inv_year = int(plot_data.get("INVYR", plot_row.get("FIRST_INVYR", 2000)))

    for variant in variants:
        for config in configs:
            config_label = config if config else "default"

            # Build FVS init tables
            stand_df = build_fvs_standinit(plot_data, stand_id, variant)
            tree_df = build_fvs_treeinit(plot_trees, stand_id)

            if tree_df.empty:
                logger.warning(f"Plot {plot_id}/{variant}/{config_label}: no valid trees")
                continue

            # Add initial (year 0) result
            results.append({
                "PLOT": plot_id,
                "FIRST_PLTCN": plt_cn,
                "YEAR": inv_year,
                "PROJ_YEAR": 0,
                "VARIANT": variant.upper(),
                "CONFIG": config_label,
                "AGB_TONS_AC": round(initial_agb, 4),
            })

            try:
                fvs_result = run_fvs_projection(
                    stand_df, tree_df, stand_id, variant,
                    config_version=config,
                    num_cycles=20,
                    cycle_length=5,
                )

                if fvs_result["exit_code"] not in (0, 10):
                    logger.warning(
                        f"Plot {plot_id}/{variant}/{config_label}: "
                        f"FVS exit code {fvs_result['exit_code']}"
                    )

                # Extract AGB at each 10-year step
                for cycle_year, treelist in sorted(fvs_result["treelists"].items()):
                    proj_year = cycle_year - inv_year
                    # Skip cycle-0 treelist — we already have the
                    # manual initial record from FIA tree data above
                    if proj_year <= 0:
                        continue
                    agb = compute_plot_agb(treelist, nsbe)
                    results.append({
                        "PLOT": plot_id,
                        "FIRST_PLTCN": plt_cn,
                        "YEAR": cycle_year,
                        "PROJ_YEAR": proj_year,
                        "VARIANT": variant.upper(),
                        "CONFIG": config_label,
                        "AGB_TONS_AC": round(agb, 4),
                    })

                # If summary available but treelists sparse, use summary BA
                if fvs_result["summary"] is not None and not fvs_result["treelists"]:
                    logger.info(
                        f"Plot {plot_id}/{variant}/{config_label}: "
                        f"using summary table (no treelists)"
                    )
                    summ = fvs_result["summary"]
                    for _, row in summ.iterrows():
                        yr = row.get("year", row.get("Year", 0))
                        results.append({
                            "PLOT": plot_id,
                            "FIRST_PLTCN": plt_cn,
                            "YEAR": int(yr),
                            "PROJ_YEAR": int(yr) - inv_year,
                            "VARIANT": variant.upper(),
                            "CONFIG": config_label,
                            "AGB_TONS_AC": np.nan,  # No NSBE without tree list
                            "BA_SQFT_AC": row.get("atba", row.get("ATBAftr", np.nan)),
                            "TPA": row.get("tpa", row.get("TPAftr", np.nan)),
                        })

            except Exception as e:
                logger.error(
                    f"Plot {plot_id}/{variant}/{config_label} FAILED: {e}"
                )
                continue

    return results


def main():
    parser = argparse.ArgumentParser(
        description="PERSEUS 100-year FVS projection pipeline"
    )
    parser.add_argument(
        "--batch-id", type=int, default=None,
        help="SLURM array task ID (1-indexed)"
    )
    parser.add_argument(
        "--batch-size", type=int, default=100,
        help="Number of plots per batch"
    )
    parser.add_argument(
        "--all", action="store_true",
        help="Process all plots (single machine mode)"
    )
    parser.add_argument(
        "--perseus-csv", type=str, default=PERSEUS_CSV,
        help="Path to PERSEUS plot CSV"
    )
    parser.add_argument(
        "--variants", type=str, nargs="+", default=["acd", "ne"],
        help="FVS variants to run"
    )
    parser.add_argument(
        "--configs", type=str, nargs="+", default=["default", "calibrated"],
        help="Config versions to run"
    )
    parser.add_argument(
        "--output-dir", type=str, default=OUTPUT_DIR,
        help="Output directory"
    )
    args = parser.parse_args()

    # Handle config labels
    configs = []
    for c in args.configs:
        configs.append(None if c == "default" else c)

    # Load PERSEUS plot list
    perseus = pd.read_csv(args.perseus_csv)
    logger.info(f"Loaded {len(perseus)} PERSEUS plots from {args.perseus_csv}")

    # Determine batch slice
    if args.batch_id is not None:
        start = (args.batch_id - 1) * args.batch_size
        end = min(start + args.batch_size, len(perseus))
        perseus = perseus.iloc[start:end]
        logger.info(
            f"Batch {args.batch_id}: processing plots {start+1} to {end} "
            f"({len(perseus)} plots)"
        )
    elif not args.all:
        logger.error("Specify --batch-id or --all")
        sys.exit(1)

    if perseus.empty:
        logger.info("No plots in this batch. Exiting.")
        return

    # Initialize NSBE calculator
    nsbe = NSBECalculator(NSBE_ROOT)

    # Load FIA trees for all plots in this batch
    # Ensure integer string representation (no trailing .0)
    plt_cns = [str(int(float(x))) for x in perseus["FIRST_PLTCN"].dropna().unique()]
    logger.info(f"Loading FIA trees for {len(plt_cns)} unique PLT_CNs...")
    fia_trees = load_fia_trees_for_plots(plt_cns, FIA_DIR)
    logger.info(f"Loaded {len(fia_trees)} tree records")

    # Process each plot
    all_results = []
    t0 = time.time()

    for idx, (_, plot_row) in enumerate(perseus.iterrows()):
        plot_results = process_plot(
            plot_row.to_dict(),
            fia_trees,
            nsbe,
            variants=args.variants,
            configs=configs,
        )
        all_results.extend(plot_results)

        if (idx + 1) % 50 == 0:
            elapsed = time.time() - t0
            rate = (idx + 1) / elapsed * 60
            logger.info(
                f"Processed {idx+1}/{len(perseus)} plots "
                f"({rate:.1f} plots/min, {elapsed:.0f}s elapsed)"
            )

    # Save results
    os.makedirs(args.output_dir, exist_ok=True)
    results_df = pd.DataFrame(all_results)

    batch_suffix = f"_batch{args.batch_id}" if args.batch_id else ""
    outfile = os.path.join(
        args.output_dir,
        f"perseus_100yr_agb{batch_suffix}.csv"
    )
    results_df.to_csv(outfile, index=False)

    elapsed = time.time() - t0
    logger.info(
        f"Complete. {len(all_results)} records written to {outfile} "
        f"({elapsed:.0f}s total)"
    )

    # Print summary statistics
    if not results_df.empty:
        summary = results_df.groupby(["VARIANT", "CONFIG", "PROJ_YEAR"]).agg(
            mean_agb=("AGB_TONS_AC", "mean"),
            n_plots=("PLOT", "nunique"),
        ).reset_index()
        print("\n" + summary.to_string(index=False))


if __name__ == "__main__":
    main()
