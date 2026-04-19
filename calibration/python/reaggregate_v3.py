#!/usr/bin/env python3
"""Re-aggregate PERSEUS v2 batch data with corrected carbon conversion and dedup."""

import sys
import os

import pandas as pd
import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))
sys.path.insert(0, os.path.dirname(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "deployment", "fvs2py"))

from perseus_uncertainty_projection import aggregate_with_uncertainty, CARBON_FRACTION

output_dir = os.path.join(
    os.environ.get("FVS_PROJECT_ROOT", os.path.join(os.path.dirname(__file__), "..", "..")),
    "calibration", "output", "perseus"
)

scen_list = [
    "harvest_noclimate", "harvest_rcp45", "harvest_rcp85",
    "noharvest_noclimate", "noharvest_rcp45", "noharvest_rcp85",
]

all_mmt = []

for scen in scen_list:
    combined_path = os.path.join(output_dir, scen, f"perseus_{scen}_all.csv")
    if not os.path.exists(combined_path):
        print(f"  {scen}: combined CSV not found, skipping")
        continue

    df = pd.read_csv(combined_path)
    n_plots = df["PLOT"].nunique()
    print(f"  {scen}: {n_plots} plots, {len(df)} records")

    point_mmt, _ = aggregate_with_uncertainty(
        df, pd.DataFrame(), n_plots_total=n_plots
    )

    if not point_mmt.empty:
        point_mmt["SCENARIO"] = scen
        all_mmt.append(point_mmt)

        mmt_path = os.path.join(output_dir, scen, f"perseus_{scen}_mmt_v3.csv")
        point_mmt.to_csv(mmt_path, index=False)
        print(f"  {scen} MMT saved: {mmt_path}")

if all_mmt:
    combined_mmt = pd.concat(all_mmt, ignore_index=True)
    combined_path = os.path.join(output_dir, "perseus_harvest_factorial_mmt_v3.csv")
    combined_mmt.to_csv(combined_path, index=False)
    n_scen = combined_mmt["SCENARIO"].nunique()
    print(f"\nCombined factorial MMT v3: {combined_path}")
    print(f"{len(combined_mmt)} records across {n_scen} scenarios")
    print(f"Carbon fraction applied: {CARBON_FRACTION}")

    # Sanity check
    baseline = combined_mmt[
        (combined_mmt["CONFIG"] == "calibrated")
        & (combined_mmt["SCENARIO"] == "noharvest_noclimate")
        & (combined_mmt["VARIANT"] == "NE")
    ]
    print("\nCalibrated NE Baseline (No Harvest, No Climate):")
    print(f"{'Year':>5}  {'MMT AGC':>10}  {'MMT AGB':>10}  {'N_PLOTS':>8}")
    for _, row in baseline.sort_values("PROJ_YEAR").iterrows():
        yr = int(row["PROJ_YEAR"])
        agc = row["MMT"]
        agb = row.get("MMT_AGB", agc * 2)
        n = int(row["N_PLOTS"])
        print(f"{yr:>5}  {agc:>10.1f}  {agb:>10.1f}  {n:>8}")

    # PERSEUS reference comparison
    perseus_ref = {0: 400.7, 20: 554.4, 40: 718.9, 60: 910.3, 80: 1092.3, 100: 1268.4}
    print("\nComparison to PERSEUS reference (MMT AGC):")
    print(f"{'Year':>5}  {'Ours':>10}  {'PERSEUS':>10}  {'Ratio':>7}")
    for yr, ref in sorted(perseus_ref.items()):
        ours = baseline.loc[baseline["PROJ_YEAR"] == yr, "MMT"]
        if not ours.empty:
            ratio = ours.values[0] / ref
            print(f"{yr:>5}  {ours.values[0]:>10.1f}  {ref:>10.1f}  {ratio:>7.2f}")
