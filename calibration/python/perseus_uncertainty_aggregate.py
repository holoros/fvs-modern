#!/usr/bin/env python3
"""
PERSEUS Post-Processing: Aggregate Uncertainty Projection Results
=================================================================

Combines per-batch output from perseus_uncertainty_projection.py into
state-level MMT estimates with credible intervals from posterior draws.

Produces:
  1. Combined plot-level point estimates (default + calibrated)
  2. Combined plot-level draw results
  3. State-level MMT time series for default + calibrated (point)
  4. State-level MMT with uncertainty bands (from draws)
  5. Comparison table matching PERSEUS format
  6. JSON summary for the manuscript

Usage:
  python perseus_uncertainty_aggregate.py --output-dir /path/to/uncertainty_output/

Author: A. Weiskittel
Date: 2026-04-11
"""

import argparse
import glob
import json
import os
import sys

import numpy as np
import pandas as pd


def combine_batches(output_dir: str, prefix: str) -> pd.DataFrame:
    """Combine batch CSV files into a single DataFrame."""
    pattern = os.path.join(output_dir, f"{prefix}_batch*.csv")
    files = sorted(glob.glob(pattern))

    # Also check for single-run file (no batch suffix)
    single = os.path.join(output_dir, f"{prefix}.csv")
    if not files and os.path.exists(single):
        files = [single]

    if not files:
        return pd.DataFrame()

    print(f"Combining {len(files)} files for {prefix}...")
    dfs = [pd.read_csv(f) for f in files]
    combined = pd.concat(dfs, ignore_index=True)

    # Remove duplicates
    key_cols = [c for c in ["PLOT", "PROJ_YEAR", "VARIANT", "CONFIG", "DRAW"]
                if c in combined.columns]
    combined.drop_duplicates(subset=key_cols, inplace=True)

    return combined


def compute_state_mmt(
    df: pd.DataFrame,
    group_cols: list[str],
    expns_df: pd.DataFrame | None = None,
    n_plots_total: int = 3586,
) -> pd.DataFrame:
    """Compute state-level MMT from plot-level AGB.

    MMT = sum(AGB_tons_ac * EXPNS_acres) * 0.907185 / 1e6
    """
    maine_forest_acres = 17_600_000

    if expns_df is not None and "EXPNS" not in df.columns:
        df = df.merge(expns_df[["PLOT", "EXPNS"]], on="PLOT", how="left")
        df["EXPNS"] = df["EXPNS"].fillna(maine_forest_acres / n_plots_total)
    elif "EXPNS" not in df.columns:
        df = df.copy()
        df["EXPNS"] = maine_forest_acres / n_plots_total

    mmt = (
        df
        .groupby(group_cols)
        .apply(
            lambda g: pd.Series({
                "MMT": (g["AGB_TONS_AC"] * g["EXPNS"]).sum() * 0.907185 / 1e6,
                "MEAN_AGB_TONS_AC": g["AGB_TONS_AC"].mean(),
                "N_PLOTS": g["PLOT"].nunique(),
            }),
            include_groups=False,
        )
        .reset_index()
    )
    return mmt


def summarize_draws(
    draw_mmt: pd.DataFrame,
    quantiles: list[float] | None = None,
) -> pd.DataFrame:
    """Summarize draw-level MMT into uncertainty bands."""
    if quantiles is None:
        quantiles = [0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975]

    summary = (
        draw_mmt
        .groupby(["VARIANT", "PROJ_YEAR"])
        .agg(
            MMT_MEAN=("MMT", "mean"),
            MMT_SD=("MMT", "std"),
            MMT_MIN=("MMT", "min"),
            MMT_MAX=("MMT", "max"),
            N_DRAWS=("DRAW", "nunique"),
            **{
                f"MMT_Q{int(q*1000):04d}": ("MMT", lambda x, q=q: x.quantile(q))
                for q in quantiles
            },
        )
        .reset_index()
    )

    # Add coefficient of variation
    summary["MMT_CV"] = summary["MMT_SD"] / summary["MMT_MEAN"]

    return summary


def main():
    parser = argparse.ArgumentParser(
        description="Aggregate PERSEUS uncertainty projection results"
    )
    parser.add_argument("--output-dir", type=str, required=True)
    parser.add_argument(
        "--expansion-csv", type=str, default=None,
        help="CSV with PLOT and EXPNS columns"
    )
    parser.add_argument(
        "--perseus-csv", type=str, default=None,
        help="Original PERSEUS plot list (for comparison)"
    )
    args = parser.parse_args()

    # -------------------------------------------------------------------
    # 1. Combine batch files
    # -------------------------------------------------------------------
    point_df = combine_batches(args.output_dir, "perseus_uncertainty_point")
    draws_df = combine_batches(args.output_dir, "perseus_uncertainty_draws")

    print(f"Point estimates: {len(point_df)} records, "
          f"{point_df['PLOT'].nunique() if not point_df.empty else 0} plots")
    print(f"Draw results: {len(draws_df)} records, "
          f"{draws_df['PLOT'].nunique() if not draws_df.empty else 0} plots")

    if point_df.empty and draws_df.empty:
        print("ERROR: No results found")
        sys.exit(1)

    # Save combined files
    if not point_df.empty:
        point_df.to_csv(
            os.path.join(args.output_dir, "perseus_unc_combined_point.csv"),
            index=False,
        )
    if not draws_df.empty:
        draws_df.to_csv(
            os.path.join(args.output_dir, "perseus_unc_combined_draws.csv"),
            index=False,
        )

    # Load expansion factors
    expns_df = None
    if args.expansion_csv and os.path.exists(args.expansion_csv):
        expns_df = pd.read_csv(args.expansion_csv)
        print(f"Loaded expansion factors for {len(expns_df)} plots")

    n_plots = max(
        point_df["PLOT"].nunique() if not point_df.empty else 1,
        draws_df["PLOT"].nunique() if not draws_df.empty else 1,
    )

    # -------------------------------------------------------------------
    # 2. Point estimate MMT
    # -------------------------------------------------------------------
    if not point_df.empty:
        point_mmt = compute_state_mmt(
            point_df,
            group_cols=["VARIANT", "CONFIG", "PROJ_YEAR"],
            expns_df=expns_df,
            n_plots_total=n_plots,
        )
        point_mmt.to_csv(
            os.path.join(args.output_dir, "mmt_point_estimates.csv"),
            index=False,
        )

        print("\n" + "=" * 80)
        print("Point Estimates: State-Level AGB (MMT)")
        print("=" * 80)
        pivot = point_mmt.pivot_table(
            index="PROJ_YEAR",
            columns=["VARIANT", "CONFIG"],
            values="MMT",
        )
        print(pivot.to_string(float_format="{:.2f}".format))

    # -------------------------------------------------------------------
    # 3. Draw-level MMT and uncertainty bands
    # -------------------------------------------------------------------
    if not draws_df.empty:
        draw_mmt = compute_state_mmt(
            draws_df,
            group_cols=["VARIANT", "DRAW", "PROJ_YEAR"],
            expns_df=expns_df,
            n_plots_total=n_plots,
        )

        draw_mmt.to_csv(
            os.path.join(args.output_dir, "mmt_per_draw.csv"),
            index=False,
        )

        uncertainty = summarize_draws(draw_mmt)
        uncertainty.to_csv(
            os.path.join(args.output_dir, "mmt_uncertainty_bands.csv"),
            index=False,
        )

        print("\n" + "=" * 80)
        print("Calibrated Uncertainty Bands (MMT): Median [90% CI]")
        print("=" * 80)
        for variant in sorted(uncertainty["VARIANT"].unique()):
            vdf = uncertainty[uncertainty["VARIANT"] == variant].sort_values("PROJ_YEAR")
            print(f"\n{variant} calibrated:")
            for _, row in vdf.iterrows():
                pyr = int(row["PROJ_YEAR"])
                med = row["MMT_Q0500"]
                lo = row["MMT_Q0050"]
                hi = row["MMT_Q0950"]
                sd = row["MMT_SD"]
                cv = row["MMT_CV"]
                print(
                    f"  Year +{pyr:>3d}: {med:8.1f} "
                    f"[{lo:8.1f}, {hi:8.1f}]  "
                    f"SD={sd:6.1f}  CV={cv:.3f}"
                )

    # -------------------------------------------------------------------
    # 4. Comparison table (PERSEUS format)
    # -------------------------------------------------------------------
    if not point_df.empty:
        # Build comparison with observed PERSEUS cycle estimates
        comparison_rows = []
        for variant in sorted(point_mmt["VARIANT"].unique()):
            for config in sorted(point_mmt.loc[
                point_mmt["VARIANT"] == variant, "CONFIG"
            ].unique()):
                vdf = point_mmt[
                    (point_mmt["VARIANT"] == variant) &
                    (point_mmt["CONFIG"] == config)
                ].sort_values("PROJ_YEAR")

                if vdf.empty:
                    continue

                row = {
                    "Model": f"FVS-{variant}" +
                             (" (cal)" if config == "calibrated" else ""),
                    "Lead": "Weiskittel",
                    "Area": "Maine",
                    "Metric": "MMT AG biomass",
                    "Scenario": "No Harvest",
                }

                # Add projection years as columns
                for _, r in vdf.iterrows():
                    pyr = int(r["PROJ_YEAR"])
                    if pyr % 5 == 0:
                        row[f"+{pyr}yr"] = round(r["MMT"], 2)

                # Compute CAGR from year 0 to year 100
                y0 = vdf.loc[vdf["PROJ_YEAR"] == 0, "MMT"]
                y100 = vdf.loc[vdf["PROJ_YEAR"] == 100, "MMT"]
                if not y0.empty and not y100.empty and y0.values[0] > 0:
                    cagr = (y100.values[0] / y0.values[0]) ** (1 / 100) - 1
                    row["CAGR_100yr"] = round(cagr, 5)

                comparison_rows.append(row)

        comparison = pd.DataFrame(comparison_rows)
        comparison.to_csv(
            os.path.join(args.output_dir, "perseus_comparison_table.csv"),
            index=False,
        )

    # -------------------------------------------------------------------
    # 5. JSON summary for manuscript integration
    # -------------------------------------------------------------------
    summary = {
        "description": (
            "PERSEUS 100yr projection with Bayesian uncertainty "
            "for Maine FIA plots (1999-2004 initial measurements)"
        ),
    }

    if not point_df.empty:
        summary["point_estimates"] = {}
        for variant in sorted(point_mmt["VARIANT"].unique()):
            for config in sorted(point_mmt.loc[
                point_mmt["VARIANT"] == variant, "CONFIG"
            ].unique()):
                vdf = point_mmt[
                    (point_mmt["VARIANT"] == variant) &
                    (point_mmt["CONFIG"] == config)
                ].sort_values("PROJ_YEAR")
                key = f"{variant}_{config}"
                summary["point_estimates"][key] = {
                    str(int(r["PROJ_YEAR"])): round(r["MMT"], 2)
                    for _, r in vdf.iterrows()
                }

    if not draws_df.empty:
        summary["uncertainty"] = {}
        for variant in sorted(uncertainty["VARIANT"].unique()):
            vdf = uncertainty[uncertainty["VARIANT"] == variant].sort_values("PROJ_YEAR")
            summary["uncertainty"][variant] = {
                str(int(r["PROJ_YEAR"])): {
                    "mean": round(r["MMT_MEAN"], 2),
                    "sd": round(r["MMT_SD"], 2),
                    "median": round(r["MMT_Q0500"], 2),
                    "ci90_lower": round(r["MMT_Q0050"], 2),
                    "ci90_upper": round(r["MMT_Q0950"], 2),
                    "ci95_lower": round(r["MMT_Q0025"], 2),
                    "ci95_upper": round(r["MMT_Q0975"], 2),
                }
                for _, r in vdf.iterrows()
            }

    with open(os.path.join(args.output_dir, "projection_summary.json"), "w") as f:
        json.dump(summary, f, indent=2)

    print("\nDone. Output files:")
    for f in sorted(os.listdir(args.output_dir)):
        if f.endswith((".csv", ".json")):
            fpath = os.path.join(args.output_dir, f)
            size = os.path.getsize(fpath)
            print(f"  {f} ({size:,} bytes)")


if __name__ == "__main__":
    main()
