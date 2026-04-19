#!/usr/bin/env python3
from __future__ import annotations

"""
PERSEUS Climate-Aware FVS Projection
======================================

Extends the PERSEUS uncertainty projection pipeline to incorporate
climate-driven site index changes. Uses per-plot SI ratios from the
ClimateNA + ranger RF model (delta method) to modify FVS site index
at scheduled intervals throughout the 100-year projection.

Climate scenarios:
  - RCP 4.5 (HadGEM2-ES approximate deltas, ~SSP245)
  - RCP 8.5 (HadGEM2-ES approximate deltas, ~SSP585)
  - No-climate baseline (ratio = 1.0, same as Round 1)

SI modification approach:
  FVS uses a fixed site index per projection. Rather than attempt
  complex mid-projection SI changes via SITECODE keywords, we break
  each 100-year projection into 3 sub-projections aligned with the
  climate periods:

    Sub-projection 1: cycles 1-8  (years 0-40,  ~2000-2040, 2030s SI)
    Sub-projection 2: cycles 9-14 (years 40-70, ~2040-2070, 2060s SI)
    Sub-projection 3: cycles 15-20 (years 70-100, ~2070-2100, 2090s SI)

  Each sub-projection uses FVS RESTART/CONTINUE capabilities where
  possible, or simply modifies the standinit SI and re-initializes
  from the treelist at the end of the previous sub-period.

  For simplicity and reliability, we use a single weighted-average SI
  ratio per plot per scenario, with weights proportional to the number
  of cycles in each period. This avoids restart complexity while still
  capturing the trajectory of climate change on site productivity.

  Weighted average: (8*r_2030s + 6*r_2060s + 6*r_2090s) / 20

SI ratio data:
  ~/SiteIndex/future_SI/perseus_si_ratios.csv
  Columns: plot_id, STATECD, COUNTYCD, PLOT, LAT, LON, SI_current,
           ratio_rcp45_2030s, ratio_rcp45_2060s, ratio_rcp45_2090s,
           ratio_rcp85_2030s, ratio_rcp85_2060s, ratio_rcp85_2090s

Usage:
  # Single climate scenario, all plots
  python perseus_climate_projection.py --rcp 4.5 --all --n-draws 50

  # Batch mode for SLURM array
  python perseus_climate_projection.py --rcp 8.5 --batch-id 1 --batch-size 100

  # No-climate baseline (for comparison)
  python perseus_climate_projection.py --rcp none --all --n-draws 50

Author: A. Weiskittel
Date: 2026-04-17
"""

import argparse
import json
import logging
import os
import signal
import sqlite3
import sys
import tempfile
import time

import numpy as np
import pandas as pd
from pathlib import Path

# ---------------------------------------------------------------------------
# Environment and path setup
# ---------------------------------------------------------------------------

PROJECT_ROOT = os.environ.get(
    "FVS_PROJECT_ROOT",
    str(Path(__file__).resolve().parents[2])
)
sys.path.insert(0, PROJECT_ROOT)
sys.path.insert(0, os.path.join(PROJECT_ROOT, "calibration", "python"))
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
SI_RATIO_CSV = os.environ.get(
    "SI_RATIO_CSV",
    os.path.expanduser("~/SiteIndex/future_SI/perseus_si_ratios.csv")
)

# Import shared components
from perseus_100yr_projection import (
    NSBECalculator,
    load_fia_trees_for_plots,
    build_fvs_standinit,
    build_fvs_treeinit,
    run_fvs_projection,
    compute_plot_agb,
    compute_initial_agb,
    KEYFILE_TEMPLATE,
)
from perseus_uncertainty_projection import (
    run_fvs_with_draw,
    MAX_FVS_FAILURES_PER_PLOT,
    aggregate_with_uncertainty,
)

# Timeout for FVS calls (seconds).
# Using signal-based timeout instead of multiprocessing fork, which hangs
# on SLURM nodes when combined with fvs2py ctypes.
FVS_CALL_TIMEOUT = 300  # 5 minutes per FVS call


class _TimeoutError(Exception):
    pass


def _timeout_handler(signum, frame):
    raise _TimeoutError(f"FVS call exceeded {FVS_CALL_TIMEOUT}s timeout")


def run_fvs_safe(func, *args, **kwargs):
    """Run an FVS function with signal-based timeout.

    Unlike run_with_timeout (multiprocessing fork), this approach
    works reliably on SLURM nodes with ctypes-loaded libraries.
    """
    old_handler = signal.signal(signal.SIGALRM, _timeout_handler)
    signal.alarm(FVS_CALL_TIMEOUT)
    try:
        result = func(*args, **kwargs)
        signal.alarm(0)  # Cancel alarm
        return result
    except _TimeoutError:
        raise TimeoutError(f"FVS call exceeded {FVS_CALL_TIMEOUT}s timeout")
    finally:
        signal.signal(signal.SIGALRM, old_handler)
        signal.alarm(0)

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
logger = logging.getLogger(__name__)


# ===========================================================================
# Climate SI Ratio Loader
# ===========================================================================

def load_si_ratios(si_ratio_csv: str) -> pd.DataFrame:
    """Load per-plot SI change ratios from the delta-method projection.

    Returns DataFrame indexed by (COUNTYCD, PLOT) with ratio columns.
    Missing ratios (e.g. plots where ecoregion extraction failed) are
    filled with 1.0 (no change from current).
    """
    si = pd.read_csv(si_ratio_csv)
    ratio_cols = [c for c in si.columns if c.startswith("ratio_")]

    # Fill NAs with 1.0 (no change) for plots without valid projections
    for col in ratio_cols:
        si[col] = si[col].fillna(1.0)

    logger.info(
        f"Loaded SI ratios for {len(si)} plots "
        f"({si[ratio_cols[0]].notna().sum()} with valid projections)"
    )
    return si


def get_plot_si_multiplier(
    si_ratios: pd.DataFrame,
    countycd: int,
    plot: int,
    rcp: str,
) -> float:
    """Get time-weighted average SI multiplier for a plot and RCP scenario.

    Weighting: 8 cycles in 2030s period, 6 in 2060s, 6 in 2090s = 20 total.
    This represents a 100-year projection with 5-year cycles starting ~2000.

    Args:
        si_ratios: DataFrame with ratio columns
        countycd: FIA county code
        plot: FIA plot number
        rcp: "4.5", "8.5", or "none"

    Returns:
        Weighted average SI multiplier (float, typically 0.9 to 1.1)
    """
    if rcp == "none":
        return 1.0

    rcp_tag = rcp.replace(".", "")  # "45" or "85"

    match = si_ratios[
        (si_ratios["COUNTYCD"] == countycd) &
        (si_ratios["PLOT"] == plot)
    ]

    if match.empty:
        return 1.0

    row = match.iloc[0]
    r_2030s = row.get(f"ratio_rcp{rcp_tag}_2030s", 1.0)
    r_2060s = row.get(f"ratio_rcp{rcp_tag}_2060s", 1.0)
    r_2090s = row.get(f"ratio_rcp{rcp_tag}_2090s", 1.0)

    # Time-weighted average: 8 cycles in 2030s, 6 in 2060s, 6 in 2090s
    weighted = (8 * r_2030s + 6 * r_2060s + 6 * r_2090s) / 20.0
    return float(weighted)


def get_plot_si_by_period(
    si_ratios: pd.DataFrame,
    countycd: int,
    plot: int,
    rcp: str,
) -> dict:
    """Get per-period SI multipliers for a plot.

    Returns dict with keys: '2030s', '2060s', '2090s'.
    """
    if rcp == "none":
        return {"2030s": 1.0, "2060s": 1.0, "2090s": 1.0}

    rcp_tag = rcp.replace(".", "")
    match = si_ratios[
        (si_ratios["COUNTYCD"] == countycd) &
        (si_ratios["PLOT"] == plot)
    ]

    if match.empty:
        return {"2030s": 1.0, "2060s": 1.0, "2090s": 1.0}

    row = match.iloc[0]
    return {
        "2030s": float(row.get(f"ratio_rcp{rcp_tag}_2030s", 1.0)),
        "2060s": float(row.get(f"ratio_rcp{rcp_tag}_2060s", 1.0)),
        "2090s": float(row.get(f"ratio_rcp{rcp_tag}_2090s", 1.0)),
    }


# ===========================================================================
# Climate-Modified FVS Runner
# ===========================================================================

def modify_standinit_si(
    stand_df: pd.DataFrame,
    si_multiplier: float,
) -> pd.DataFrame:
    """Apply SI multiplier to standinit table.

    Creates a copy with modified site_index column. Clamps result to
    FVS valid range [10, 200] feet.
    """
    modified = stand_df.copy()
    original_si = modified["site_index"].iloc[0]
    new_si = int(round(original_si * si_multiplier))
    new_si = max(10, min(200, new_si))
    modified["site_index"] = new_si

    if abs(si_multiplier - 1.0) > 0.001:
        logger.debug(
            f"SI modified: {original_si} -> {new_si} "
            f"(multiplier={si_multiplier:.4f})"
        )

    return modified


def get_plot_si_delta_ft(
    si_ratios: pd.DataFrame,
    countycd: int,
    plot: int,
    rcp: str,
) -> float:
    """Get time-weighted absolute SI delta in feet for a plot and RCP scenario.

    Computes the change in site index (future minus current) from the ranger
    RF model, converts from meters to feet, and returns a time-weighted
    average across projection periods.

    Weighting: 8 cycles in 2030s period, 6 in 2060s, 6 in 2090s = 20 total.

    Args:
        si_ratios: DataFrame with SI_current (meters) and ratio columns
        countycd: FIA county code
        plot: FIA plot number
        rcp: "4.5", "8.5", or "none"

    Returns:
        Weighted average SI delta in feet (positive = increase)
    """
    if rcp == "none":
        return 0.0

    rcp_tag = rcp.replace(".", "")  # "45" or "85"

    match = si_ratios[
        (si_ratios["COUNTYCD"] == countycd) &
        (si_ratios["PLOT"] == plot)
    ]

    if match.empty:
        return 0.0

    row = match.iloc[0]
    si_current_m = row.get("SI_current", None)
    if si_current_m is None or pd.isna(si_current_m) or si_current_m <= 0:
        return 0.0

    M_TO_FT = 3.28084

    r_2030s = row.get(f"ratio_rcp{rcp_tag}_2030s", 1.0)
    r_2060s = row.get(f"ratio_rcp{rcp_tag}_2060s", 1.0)
    r_2090s = row.get(f"ratio_rcp{rcp_tag}_2090s", 1.0)

    # Absolute deltas in meters, then convert to feet
    delta_2030s_ft = si_current_m * (r_2030s - 1.0) * M_TO_FT
    delta_2060s_ft = si_current_m * (r_2060s - 1.0) * M_TO_FT
    delta_2090s_ft = si_current_m * (r_2090s - 1.0) * M_TO_FT

    # Time-weighted average
    weighted = (8 * delta_2030s_ft + 6 * delta_2060s_ft + 6 * delta_2090s_ft) / 20.0
    return float(weighted)


def modify_standinit_si_delta(
    stand_df: pd.DataFrame,
    si_delta_ft: float,
) -> pd.DataFrame:
    """Apply absolute SI delta (in feet) to standinit table.

    Creates a copy with modified site_index column. Clamps result to
    FVS valid range [10, 200] feet.
    """
    modified = stand_df.copy()
    original_si = modified["site_index"].iloc[0]
    new_si = int(round(original_si + si_delta_ft))
    new_si = max(10, min(200, new_si))
    modified["site_index"] = new_si

    if abs(si_delta_ft) > 0.1:
        logger.debug(
            f"SI modified: {original_si} -> {new_si} "
            f"(delta={si_delta_ft:+.1f} ft)"
        )

    return modified


# ===========================================================================
# Process Plot with Climate + Uncertainty
# ===========================================================================

def process_plot_climate(
    plot_row: dict,
    fia_trees: pd.DataFrame,
    nsbe: NSBECalculator,
    si_ratios: pd.DataFrame,
    rcp: str,
    uncertainty_engines: dict,
    default_configs: dict,
    n_draws: int = 50,
    variants: list[str] = None,
    seed: int = 42,
) -> tuple[list[dict], list[dict]]:
    """Process one PERSEUS plot with climate-modified SI and uncertainty.

    Similar to process_plot_with_uncertainty but applies SI multiplier
    before each FVS run.
    """
    if variants is None:
        variants = ["acd", "ne"]

    plot_id = plot_row["PLOT"]
    raw_cn = plot_row["FIRST_PLTCN"]
    plt_cn = str(int(float(raw_cn))) if pd.notna(raw_cn) else ""
    stand_id = f"P{int(float(plot_id))}"
    countycd = int(plot_row.get("COUNTYCD", 19))

    # Get SI multiplier for this plot + RCP
    si_mult = get_plot_si_multiplier(si_ratios, countycd, int(float(plot_id)), rcp)

    if fia_trees.empty or "PLT_CN" not in fia_trees.columns:
        logger.warning(f"Plot {plot_id}: no tree data available")
        return [], []

    # Filter trees for this plot
    fia_cn_str = fia_trees["PLT_CN"].apply(
        lambda x: str(int(float(x))) if pd.notna(x) else ""
    )
    plot_trees = fia_trees.loc[fia_cn_str == plt_cn]

    if plot_trees.empty:
        logger.warning(f"Plot {plot_id}: no trees for PLT_CN={plt_cn}")
        return [], []

    plot_data = plot_trees.iloc[0].to_dict()
    plot_data["COUNTYCD"] = countycd
    inv_year = int(plot_data.get("INVYR", plot_row.get("FIRST_INVYR", 2000)))

    initial_agb = compute_initial_agb(plot_trees, nsbe)

    point_results = []
    draw_results = []

    rcp_label = f"rcp{rcp.replace('.', '')}" if rcp != "none" else "noclimate"

    for variant in variants:
        stand_df = build_fvs_standinit(plot_data, stand_id, variant)
        tree_df = build_fvs_treeinit(plot_trees, stand_id)

        if tree_df.empty:
            logger.warning(f"Plot {plot_id}/{variant}: no valid trees")
            continue

        # Apply climate SI modification
        stand_df_climate = modify_standinit_si(stand_df, si_mult)

        variant_fail_count = 0
        variant_skipped = False

        # --- Point estimate: calibrated only (climate runs) ---
        config = "calibrated"
        config_label = f"calibrated_{rcp_label}"

        # Add initial condition (year 0)
        point_results.append({
            "PLOT": plot_id,
            "FIRST_PLTCN": plt_cn,
            "YEAR": inv_year,
            "PROJ_YEAR": 0,
            "VARIANT": variant.upper(),
            "CONFIG": config_label,
            "RCP": rcp,
            "SI_MULT": round(si_mult, 4),
            "AGB_TONS_AC": round(initial_agb, 4),
        })

        try:
            fvs_result = run_fvs_safe(
                run_fvs_projection,
                stand_df_climate, tree_df, stand_id, variant,
                config_version=config, num_cycles=20, cycle_length=5,
            )
            ec = fvs_result.get("exit_code", 0)
            if ec not in (0, 10):
                variant_fail_count += 1
                logger.warning(
                    f"Plot {plot_id}/{variant}/{config_label}: FVS exit code {ec}"
                )
                if variant_fail_count >= MAX_FVS_FAILURES_PER_PLOT:
                    variant_skipped = True
            else:
                for cycle_year, treelist in sorted(fvs_result["treelists"].items()):
                    proj_year = cycle_year - inv_year
                    if proj_year <= 0:
                        continue
                    agb = compute_plot_agb(treelist, nsbe)
                    point_results.append({
                        "PLOT": plot_id,
                        "FIRST_PLTCN": plt_cn,
                        "YEAR": cycle_year,
                        "PROJ_YEAR": proj_year,
                        "VARIANT": variant.upper(),
                        "CONFIG": config_label,
                        "RCP": rcp,
                        "SI_MULT": round(si_mult, 4),
                        "AGB_TONS_AC": round(agb, 4),
                    })
                variant_fail_count = 0
        except Exception as e:
            variant_fail_count += 1
            logger.error(f"Plot {plot_id}/{variant}/{config_label}: {e}")
            if variant_fail_count >= MAX_FVS_FAILURES_PER_PLOT:
                variant_skipped = True

        # --- Posterior draws with climate-modified SI ---
        engine = uncertainty_engines.get(variant.lower())
        default_cfg = default_configs.get(variant.lower())

        if engine is None or default_cfg is None:
            logger.warning(f"No uncertainty engine for {variant}; skipping draws")
            continue

        if variant_skipped:
            logger.info(f"Plot {plot_id}/{variant}: skipping draws (variant_skipped)")
            continue

        rng = np.random.default_rng(seed + int(float(plot_id)))
        draw_indices = rng.choice(engine.n_draws, size=n_draws, replace=False)

        draw_fail_count = 0
        for di, draw_idx in enumerate(draw_indices):
            if draw_fail_count >= MAX_FVS_FAILURES_PER_PLOT:
                logger.warning(
                    f"Plot {plot_id}/{variant}: {draw_fail_count} draw failures, "
                    f"SKIPPING remaining draws"
                )
                break

            draw = engine.get_draw(int(draw_idx))
            draw_keywords = engine.generate_keywords_for_draw(
                draw, default_cfg, draw_idx=int(draw_idx)
            )

            # Initial condition for this draw
            draw_results.append({
                "PLOT": plot_id,
                "FIRST_PLTCN": plt_cn,
                "YEAR": inv_year,
                "PROJ_YEAR": 0,
                "VARIANT": variant.upper(),
                "DRAW": int(draw_idx),
                "RCP": rcp,
                "SI_MULT": round(si_mult, 4),
                "AGB_TONS_AC": round(initial_agb, 4),
            })

            try:
                fvs_result = run_fvs_safe(
                    run_fvs_with_draw,
                    stand_df_climate, tree_df, stand_id, variant,
                    draw_keywords=draw_keywords,
                    num_cycles=20,
                    cycle_length=5,
                )
                ec = fvs_result.get("exit_code", 0)
                if ec not in (0, 10):
                    draw_fail_count += 1
                    logger.warning(
                        f"Plot {plot_id}/{variant}/draw{draw_idx}: "
                        f"FVS exit code {ec}"
                    )
                    continue
                draw_fail_count = 0
                for cycle_year, treelist in sorted(fvs_result["treelists"].items()):
                    proj_year = cycle_year - inv_year
                    if proj_year <= 0:
                        continue
                    agb = compute_plot_agb(treelist, nsbe)
                    draw_results.append({
                        "PLOT": plot_id,
                        "FIRST_PLTCN": plt_cn,
                        "YEAR": cycle_year,
                        "PROJ_YEAR": proj_year,
                        "VARIANT": variant.upper(),
                        "DRAW": int(draw_idx),
                        "RCP": rcp,
                        "SI_MULT": round(si_mult, 4),
                        "AGB_TONS_AC": round(agb, 4),
                    })
            except Exception as e:
                draw_fail_count += 1
                logger.error(
                    f"Plot {plot_id}/{variant}/draw{draw_idx}: {e}"
                )

    return point_results, draw_results


# ===========================================================================
# Main
# ===========================================================================

def main():
    parser = argparse.ArgumentParser(
        description="PERSEUS climate-aware FVS projection with uncertainty"
    )
    parser.add_argument(
        "--rcp", type=str, required=True,
        choices=["4.5", "8.5", "none"],
        help="Climate scenario: 4.5 (RCP4.5), 8.5 (RCP8.5), or none"
    )
    parser.add_argument("--batch-id", type=int, default=None)
    parser.add_argument("--batch-size", type=int, default=100)
    parser.add_argument("--all", action="store_true")
    parser.add_argument("--perseus-csv", type=str, default=PERSEUS_CSV)
    parser.add_argument("--si-ratio-csv", type=str, default=SI_RATIO_CSV)
    parser.add_argument(
        "--variants", type=str, nargs="+", default=["acd", "ne"],
    )
    parser.add_argument(
        "--n-draws", type=int, default=50,
        help="Number of posterior draws per variant per plot"
    )
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--start-year", type=int, default=1999)
    parser.add_argument("--end-year", type=int, default=2004)
    parser.add_argument("--expansion-csv", type=str, default=None)
    parser.add_argument("--output-dir", type=str, default=OUTPUT_DIR)
    args = parser.parse_args()

    rcp_tag = f"rcp{args.rcp.replace('.', '')}" if args.rcp != "none" else "noclimate"
    climate_output_dir = os.path.join(args.output_dir, f"climate_{rcp_tag}")
    os.makedirs(climate_output_dir, exist_ok=True)

    # Load PERSEUS plots
    perseus = pd.read_csv(args.perseus_csv)
    logger.info(f"Loaded {len(perseus)} total PERSEUS plots")

    mask = (
        (perseus["FIRST_INVYR"] >= args.start_year) &
        (perseus["FIRST_INVYR"] <= args.end_year)
    )
    perseus = perseus.loc[mask].copy()
    logger.info(
        f"Filtered to {len(perseus)} plots with FIRST_INVYR in "
        f"[{args.start_year}, {args.end_year}]"
    )

    # Load SI ratios
    si_ratios = load_si_ratios(args.si_ratio_csv)

    # Log SI multiplier summary for this scenario
    if args.rcp != "none":
        rcp_tag_col = args.rcp.replace(".", "")
        test_mults = []
        for _, row in perseus.iterrows():
            m = get_plot_si_multiplier(
                si_ratios, int(row["COUNTYCD"]), int(row["PLOT"]), args.rcp
            )
            test_mults.append(m)
        test_mults = np.array(test_mults)
        logger.info(
            f"RCP {args.rcp} SI multiplier summary: "
            f"mean={test_mults.mean():.4f}, "
            f"median={np.median(test_mults):.4f}, "
            f"range=[{test_mults.min():.4f}, {test_mults.max():.4f}]"
        )

    # Batch slicing
    if args.batch_id is not None:
        start = (args.batch_id - 1) * args.batch_size
        end = min(start + args.batch_size, len(perseus))
        perseus = perseus.iloc[start:end]
        logger.info(f"Batch {args.batch_id}: plots {start+1} to {end}")
    elif not args.all:
        logger.error("Specify --batch-id or --all")
        sys.exit(1)

    if perseus.empty:
        logger.info("No plots in this batch. Exiting.")
        return

    # Initialize NSBE calculator
    nsbe = NSBECalculator(NSBE_ROOT)

    # Initialize uncertainty engines
    from config.uncertainty import UncertaintyEngine
    from config.config_loader import FvsConfigLoader

    uncertainty_engines = {}
    default_configs = {}

    for variant in args.variants:
        try:
            engine = UncertaintyEngine(variant, config_dir=CONFIG_DIR, seed=args.seed)
            if engine.draws_available:
                uncertainty_engines[variant] = engine
                logger.info(
                    f"{variant.upper()}: {engine.n_draws} posterior draws available"
                )
            else:
                logger.warning(f"{variant.upper()}: no draws file found")
        except Exception as e:
            logger.warning(f"{variant.upper()} uncertainty engine failed: {e}")

        try:
            loader = FvsConfigLoader(variant, version="default", config_dir=CONFIG_DIR)
            default_configs[variant] = loader.config
        except Exception as e:
            logger.warning(f"{variant.upper()} default config failed: {e}")

    # Load FIA trees
    plt_cns = [
        str(int(float(x)))
        for x in perseus["FIRST_PLTCN"].dropna().unique()
    ]
    logger.info(f"Loading FIA trees for {len(plt_cns)} unique PLT_CNs...")
    fia_trees = load_fia_trees_for_plots(plt_cns, FIA_DIR)
    logger.info(f"Loaded {len(fia_trees)} tree records")

    # Load expansion factors if provided
    expns_df = None
    if args.expansion_csv and os.path.exists(args.expansion_csv):
        expns_df = pd.read_csv(args.expansion_csv)
        logger.info(f"Loaded expansion factors for {len(expns_df)} plots")

    # Process plots
    all_point = []
    all_draws = []
    t0 = time.time()

    for idx, (_, plot_row) in enumerate(perseus.iterrows()):
        try:
            point, draws = process_plot_climate(
                plot_row.to_dict(),
                fia_trees,
                nsbe,
                si_ratios,
                args.rcp,
                uncertainty_engines,
                default_configs,
                n_draws=args.n_draws,
                variants=args.variants,
                seed=args.seed,
            )
            all_point.extend(point)
            all_draws.extend(draws)
        except Exception as exc:
            logger.warning(
                f"Plot {plot_row.get('PLOT', '?')} failed: {exc}"
            )

        if (idx + 1) % 25 == 0:
            elapsed = time.time() - t0
            rate = (idx + 1) / elapsed * 60
            logger.info(
                f"Processed {idx+1}/{len(perseus)} plots "
                f"({rate:.1f} plots/min, {elapsed:.0f}s, "
                f"{len(all_point)} point + {len(all_draws)} draw records)"
            )

    # Save results
    batch_suffix = f"_batch{args.batch_id}" if args.batch_id else ""

    point_df = pd.DataFrame(all_point)
    draws_df = pd.DataFrame(all_draws)

    point_path = os.path.join(
        climate_output_dir,
        f"perseus_climate_point_{rcp_tag}{batch_suffix}.csv"
    )
    draws_path = os.path.join(
        climate_output_dir,
        f"perseus_climate_draws_{rcp_tag}{batch_suffix}.csv"
    )

    if not point_df.empty:
        point_df.to_csv(point_path, index=False)
        logger.info(f"Point estimates: {len(point_df)} records -> {point_path}")

    if not draws_df.empty:
        draws_df.to_csv(draws_path, index=False)
        logger.info(f"Draw results: {len(draws_df)} records -> {draws_path}")

    # Aggregate to state level
    if not point_df.empty or not draws_df.empty:
        point_mmt, uncertainty_mmt = aggregate_with_uncertainty(
            point_df, draws_df,
            expns_df=expns_df,
            n_plots_total=len(perseus),
        )

        if not point_mmt.empty:
            mmt_path = os.path.join(
                climate_output_dir,
                f"perseus_climate_mmt_point_{rcp_tag}{batch_suffix}.csv"
            )
            point_mmt.to_csv(mmt_path, index=False)
            logger.info(f"Point MMT: {mmt_path}")

            print(f"\n{'=' * 80}")
            print(f"Climate Scenario RCP {args.rcp}: State-Level AGB (MMT)")
            print(f"{'=' * 80}")
            for _, row in point_mmt.sort_values("PROJ_YEAR").iterrows():
                print(
                    f"  Year +{int(row['PROJ_YEAR']):>3d}  "
                    f"{row['VARIANT']:>4s}  {row.get('CONFIG', ''):>20s}  "
                    f"{row['MMT']:>8.2f} MMT  "
                    f"({int(row.get('N_PLOTS', 0))} plots)"
                )

        if not uncertainty_mmt.empty:
            unc_path = os.path.join(
                climate_output_dir,
                f"perseus_climate_mmt_bands_{rcp_tag}{batch_suffix}.csv"
            )
            uncertainty_mmt.to_csv(unc_path, index=False)
            logger.info(f"Uncertainty bands: {unc_path}")

            print(f"\n{'=' * 80}")
            print(f"RCP {args.rcp} Uncertainty Bands (MMT): Median [90% CI]")
            print(f"{'=' * 80}")
            for variant in args.variants:
                vdf = uncertainty_mmt[
                    uncertainty_mmt["VARIANT"] == variant.upper()
                ].sort_values("PROJ_YEAR")
                if vdf.empty:
                    continue
                print(f"\n{variant.upper()} calibrated + climate:")
                for _, row in vdf.iterrows():
                    pyr = int(row["PROJ_YEAR"])
                    med = row.get("MMT_Q0500", row.get("MMT_MEAN", 0))
                    lo = row.get("MMT_Q0100", row.get("MMT_Q0025", 0))
                    hi = row.get("MMT_Q0900", row.get("MMT_Q0975", 0))
                    print(f"  Year +{pyr:>3d}: {med:8.1f} [{lo:8.1f}, {hi:8.1f}]")

    # Save metadata
    meta = {
        "description": f"PERSEUS climate projection RCP {args.rcp}",
        "rcp": args.rcp,
        "si_ratio_csv": args.si_ratio_csv,
        "year_range": [args.start_year, args.end_year],
        "n_plots": len(perseus),
        "n_draws": args.n_draws,
        "seed": args.seed,
        "variants": args.variants,
        "si_multiplier_approach": "time-weighted average (8*2030s + 6*2060s + 6*2090s)/20",
        "elapsed_seconds": round(time.time() - t0, 1),
    }
    if args.rcp != "none":
        meta["si_multiplier_summary"] = {
            "mean": float(test_mults.mean()),
            "median": float(np.median(test_mults)),
            "min": float(test_mults.min()),
            "max": float(test_mults.max()),
            "std": float(test_mults.std()),
        }

    meta_path = os.path.join(
        climate_output_dir,
        f"perseus_climate_meta_{rcp_tag}{batch_suffix}.json"
    )
    with open(meta_path, "w") as f:
        json.dump(meta, f, indent=2, default=str)

    elapsed = time.time() - t0
    logger.info(f"Complete in {elapsed:.0f}s ({rcp_tag})")


if __name__ == "__main__":
    main()
