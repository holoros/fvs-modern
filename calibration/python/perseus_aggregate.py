#!/usr/bin/env python3
"""
PERSEUS Post-Processing: Aggregate FVS Projections to State-Level MMT
=====================================================================

Reads per-batch CSVs from the PERSEUS projection pipeline and produces:
  1. Combined plot-level CSV: AGB per plot at each 10-year step
  2. State-level MMT: total Maine AGB in megatonnes (matching PERSEUS units)

Aggregation logic (per Aaron's specification):
  tree AGB (NSBE kg) x TPA -> per-acre AGB -> expand by forestland acres -> MMT

The PERSEUS CSV values are in megatonnes (MMT) for the whole state, where
each plot's per-acre AGB is expanded by the FIA plot expansion factor
(acres of forestland represented by the plot), then summed across all plots.

Usage:
  python perseus_aggregate.py
  python perseus_aggregate.py --output-dir /path/to/perseus/

Author: A. Weiskittel
Date: 2026-04-09
"""

import argparse
import glob
import os
import sys

import numpy as np
import pandas as pd


def main():
    parser = argparse.ArgumentParser(
        description="Aggregate PERSEUS batch results into state-level MMT"
    )
    parser.add_argument(
        "--output-dir", type=str,
        default=os.path.join(
            os.environ.get("FVS_PROJECT_ROOT", "."),
            "calibration", "output", "perseus"
        ),
        help="Directory with per-batch CSV files"
    )
    parser.add_argument(
        "--perseus-csv", type=str,
        default=os.path.join(
            os.environ.get("FVS_PROJECT_ROOT", "."),
            "calibration", "data", "perseus_plots.csv"
        ),
        help="Original PERSEUS plot list (for reference columns)"
    )
    parser.add_argument(
        "--expansion-csv", type=str, default=None,
        help="CSV with PLT_CN and expansion factor (acres represented). "
             "If not provided, uses FIA EXPNS from plot data."
    )
    args = parser.parse_args()

    # -----------------------------------------------------------------------
    # 1. Combine batch CSVs
    # -----------------------------------------------------------------------
    batch_files = sorted(glob.glob(
        os.path.join(args.output_dir, "perseus_100yr_agb_batch*.csv")
    ))

    if not batch_files:
        # Try single-run file
        single = os.path.join(args.output_dir, "perseus_100yr_agb.csv")
        if os.path.exists(single):
            batch_files = [single]
        else:
            print(f"ERROR: No result files found in {args.output_dir}")
            sys.exit(1)

    print(f"Combining {len(batch_files)} batch files...")
    dfs = [pd.read_csv(f) for f in batch_files]
    combined = pd.concat(dfs, ignore_index=True)
    combined.drop_duplicates(
        subset=["PLOT", "PROJ_YEAR", "VARIANT", "CONFIG"],
        inplace=True
    )

    # Save combined plot-level results
    combined_path = os.path.join(args.output_dir, "perseus_100yr_combined.csv")
    combined.to_csv(combined_path, index=False)
    print(f"Combined: {len(combined)} records, {combined['PLOT'].nunique()} plots")
    print(f"Saved to: {combined_path}")

    # -----------------------------------------------------------------------
    # 2. Load PERSEUS reference data for comparison
    # -----------------------------------------------------------------------
    if os.path.exists(args.perseus_csv):
        perseus = pd.read_csv(args.perseus_csv)
    else:
        perseus = None
        print("WARNING: PERSEUS reference CSV not found; skipping comparison")

    # -----------------------------------------------------------------------
    # 3. Compute state-level MMT
    #
    # Each FIA plot represents a certain number of acres of forestland.
    # The expansion factor (EXPNS) tells you how many acres that plot
    # represents in the population. To compute state total:
    #
    #   AGB_state = sum_over_plots( AGB_tons_per_acre * EXPNS_acres )
    #
    # Then convert short tons to metric tonnes (x 0.907185) and to
    # megatonnes (/ 1e6). Or equivalently:
    #
    #   MMT = sum(AGB_tons_ac * EXPNS) * 0.907185 / 1e6
    #
    # Carbon conversion: AGB * 0.5 = AGC (standard carbon fraction)
    # Output MMT column is AGC (matching PERSEUS units)
    #
    # If we don't have EXPNS, we estimate it from total Maine forestland
    # (~17.6 million acres) / number of sampled plots.
    # -----------------------------------------------------------------------

    CARBON_FRACTION = 0.5

    n_plots = combined["PLOT"].nunique()

    if args.expansion_csv and os.path.exists(args.expansion_csv):
        expns = pd.read_csv(args.expansion_csv)
        combined = combined.merge(
            expns[["PLOT", "EXPNS"]],
            on="PLOT", how="left"
        )
    else:
        # Approximate: Maine has ~17.6 million acres of forestland
        # Each plot in the FIA annual design represents roughly
        # 17,600,000 / n_plots acres
        maine_forest_acres = 17_600_000
        approx_expns = maine_forest_acres / n_plots
        combined["EXPNS"] = approx_expns
        print(
            f"Using approximate expansion factor: "
            f"{maine_forest_acres:,} acres / {n_plots} plots = "
            f"{approx_expns:,.0f} acres/plot"
        )

    # Compute MMT at each projection year for each variant x config
    # MMT is aboveground carbon (AGC); MMT_AGB is aboveground biomass
    mmt_results = (
        combined
        .groupby(["VARIANT", "CONFIG", "PROJ_YEAR"])
        .apply(
            lambda g: pd.Series({
                "MMT_AGB": (g["AGB_TONS_AC"] * g["EXPNS"]).sum()
                           * 0.907185 / 1e6,
                "MMT": (g["AGB_TONS_AC"] * g["EXPNS"]).sum()
                        * 0.907185 / 1e6 * CARBON_FRACTION,
                "MEAN_AGB_TONS_AC": g["AGB_TONS_AC"].mean(),
                "N_PLOTS": g["PLOT"].nunique(),
            }),
            include_groups=False,
        )
        .reset_index()
    )

    mmt_path = os.path.join(args.output_dir, "perseus_100yr_mmt.csv")
    mmt_results.to_csv(mmt_path, index=False)
    print(f"\nState-level MMT saved to: {mmt_path}")

    # -----------------------------------------------------------------------
    # 4. Summary table
    # -----------------------------------------------------------------------
    print("\n" + "=" * 80)
    print("PERSEUS 100-Year Projection: State-Level AGB (MMT)")
    print("=" * 80)

    pivot = mmt_results.pivot_table(
        index="PROJ_YEAR",
        columns=["VARIANT", "CONFIG"],
        values="MMT",
    )
    print(pivot.to_string(float_format="{:.2f}".format))

    # -----------------------------------------------------------------------
    # 5. Compare with observed PERSEUS cycles (if available)
    # -----------------------------------------------------------------------
    if perseus is not None:
        print("\n" + "-" * 80)
        print("Observed PERSEUS cycle estimates (MMT):")
        print("-" * 80)
        for col in ["CYCLE5_MMT", "CYCLE6_MMT", "CYCLE7_MMT", "CYCLE8_MMT", "2021"]:
            if col in perseus.columns:
                total = perseus[col].sum()
                print(f"  {col}: {total:.2f} MMT")

    print("\nDone.")


if __name__ == "__main__":
    main()
