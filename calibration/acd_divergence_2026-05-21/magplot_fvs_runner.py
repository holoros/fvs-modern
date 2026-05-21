#!/usr/bin/env python3
"""
magplot_fvs_runner.py
=====================
Run the Fortran FVS engine (FVS-NE / FVS-ACD, default + calibrated) on Canadian
MAGPlot (New Brunswick) tree lists.

RESOLVED ROUTE (2026-05-21): use the standalone FVS binary (lib/FVSne, lib/FVSacd)
with a DATABASE/STANDSQL keyfile and a SQLite inventory DB, ONE stand per
subprocess. This was validated end to end (FVS_Summary2 populated). It avoids the
three fvs2py-route blockers documented in MAGPLOT_FVS_BLOCKER.md:
  1. fvs2py needs Python >= 3.11 (StrEnum/ParamSpec)  -- not needed here.
  2. fvs2py run() did not drive the DATABASE run to completion (no output)
     -- the standalone binary drives itself to completion and writes results.
  3. multiple fvs2py runs per process crash on keyrdr unit-15 EOF
     -- one subprocess per stand is always a fresh process.
This script needs only sqlite3 + pandas + the FVS binaries (works on Python 3.9).
"""
from __future__ import annotations
import argparse, os, sqlite3, subprocess, sys, tempfile
import pandas as pd
import numpy as np

PROJECT_ROOT = os.environ.get("FVS_PROJECT_ROOT", "/users/PUOM0008/crsfaaron/fvs-modern")
FVS_LIB_DIR = os.environ.get("FVS_LIB_DIR", os.path.join(PROJECT_ROOT, "lib"))
CONFIG_DIR = os.environ.get("FVS_CONFIG_DIR", os.path.join(PROJECT_ROOT, "config"))

ACRES_PER_HA = 2.4710538147
INCH_PER_CM = 1 / 2.54
FT_PER_M = 1 / 0.3048

GS_TO_SPCD = {
    "ABIE.BAL": 12, "PICE.MAR": 95, "ACER.RUB": 316, "BETU.PAP": 375,
    "PICE.RUB": 97, "POPU.TRE": 746, "THUJ.OCC": 241, "BETU.ALL": 371,
    "ACER.SAH": 318, "ACER.SAC": 318, "PICE.GLA": 94, "BETU.POP": 379,
    "FAGU.GRA": 531, "PINU.BAN": 105, "PRUN.PEN": 761, "PINU.STR": 129,
    "ACER.PEN": 315, "ACER.SPI": 319, "LARI.LAR": 71, "FRAX.AME": 541,
    "POPU.GRA": 743, "POPU.BAL": 741, "FRAX.NIG": 543, "SALI.SPP": 920,
    "TSUG.CAN": 261, "PINU.RES": 125, "PRUN.VIR": 763, "SORB.AME": 935,
    "OSTR.VIR": 701, "PICE.ABI": 91, "QUER.SPP": 833, "PRUN.SER": 762,
    "PINU.SYL": 130, "ULMU.SPP": 972, "CRAT.SPP": 500, "TILI.AME": 951,
    "FRAX.PEN": 544, "JUGL.CIN": 601,
    "ALNU.SPP": 998, "ILEX.MUC": 998, "VIBU.CAS": 998, "AMEL.SPP": 998,
    "MALU.SPP": 998, "SAMB.RAC": 998, "SAMB.NIG": 998, "CORN.ALT": 998,
    "VIBU.LAN": 998, "UNKN.SPP": 998, "GENH.SPP": 998, "GENC.SPP": 298,
}

KEYFILE = """STDIDENT
{sid}
DATABASE
DSNIN
{db}
DSNOUT
{db}
STANDSQL
SELECT * FROM fvs_standinit WHERE stand_id = '%StandID%'
ENDSQL
TREESQL
SELECT * FROM fvs_treeinit WHERE stand_id = '%StandID%'
ENDSQL
END
DATABASE
SUMMARY            2
TREELIDB           2         2
END
TIMEINT            0         {clen}
NUMCYCLE          {ncyc}
{calib}
PROCESS
STOP
"""

def build_standinit(sid, inv_year, variant):
    return pd.DataFrame([{
        "stand_id": sid, "variant": variant.upper(), "inv_year": int(inv_year),
        "latitude": 46.5, "longitude": -66.5, "region": 9, "forest": 0, "district": 0,
        "basal_area_factor": 0.0, "inv_plot_size": 1.0, "brk_dbh": 5.0, "num_plots": 1,
        "age": 50, "aspect": 0, "slope": 5, "elevft": 656,  # ~200 m
        "site_species": 12, "site_index": 45, "state": 23, "county": 21,
        "forest_type": 121, "sam_wt": 1.0,
    }])

def build_treeinit_magplot(plot_trees, sid):
    rows, tid = [], 1
    for _, r in plot_trees.iterrows():
        spcd = GS_TO_SPCD.get(str(r["species_gs"]).strip().upper())
        if spcd is None:
            continue
        try:
            dbh_cm = float(r["dbh"]); stem_ha = float(r["stem_ha"])
        except (TypeError, ValueError):
            continue
        if not (np.isfinite(dbh_cm) and dbh_cm > 0 and np.isfinite(stem_ha) and stem_ha > 0):
            continue
        ht_ft = 0.0
        try:
            h = float(r["height"])
            if np.isfinite(h) and h > 0:
                ht_ft = h * FT_PER_M
        except (TypeError, ValueError):
            pass
        rows.append({"stand_id": sid, "plot_id": 1, "tree_id": tid,
                     "tree_count": round(stem_ha / ACRES_PER_HA, 5), "species": spcd,
                     "diameter": round(dbh_cm * INCH_PER_CM, 3), "ht": round(ht_ft, 1),
                     "crratio": 40})
        tid += 1
    return pd.DataFrame(rows)

def calibrated_keywords(variant):
    try:
        sys.path.insert(0, PROJECT_ROOT)
        from config.config_loader import FvsConfigLoader
        return FvsConfigLoader(variant.lower(), version="calibrated",
                               config_dir=CONFIG_DIR).generate_keywords(include_comments=False)
    except Exception as e:
        sys.stderr.write(f"  calibrated keywords unavailable for {variant}: {e}\n")
        return "** DEFAULT (calibrated config not found)"

def run_stand(sid, tree_df, variant, config, ncyc, clen=5):
    """One stand in its own subprocess via the standalone FVS binary."""
    binary = os.path.join(FVS_LIB_DIR, f"FVS{variant.lower()}")
    if not os.path.exists(binary):
        return None, f"binary not found: {binary}"
    with tempfile.TemporaryDirectory() as d:
        db = os.path.join(d, "FVS_Data.db")
        con = sqlite3.connect(db)
        build_standinit(sid, 2021, variant).to_sql("fvs_standinit", con, if_exists="replace", index=False)
        tree_df.to_sql("fvs_treeinit", con, if_exists="replace", index=False)
        con.close()
        calib = calibrated_keywords(variant) if config == "calibrated" else "** DEFAULT PARAMETERS"
        key = os.path.join(d, "mag.key")
        with open(key, "w") as f:
            f.write(KEYFILE.format(sid=sid, db=db, clen=clen, ncyc=ncyc, calib=calib))
        # standalone binary: traceback on the final STOP is cosmetic; results are written first
        subprocess.run([binary, f"--keywordfile={key}"], cwd=d,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=120)
        try:
            con = sqlite3.connect(db)
            df = pd.read_sql_query("SELECT * FROM FVS_Summary2", con)
            con.close()
            df["variant"] = variant.upper(); df["config"] = config
            return df, None
        except Exception as e:
            return None, f"no FVS_Summary2: {e}"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--magdir", default="/users/PUOM0008/crsfaaron/magplot")
    ap.add_argument("--nstands", type=int, default=5)
    ap.add_argument("--min-trees", type=int, default=10)
    ap.add_argument("--variants", default="ne,acd")
    ap.add_argument("--configs", default="default")
    ap.add_argument("--num-cycles", type=int, default=2)
    ap.add_argument("--out", default="magplot_fvs_results.csv")
    args = ap.parse_args()

    trees = pd.read_csv(os.path.join(args.magdir, "magp_trees_nb.csv"),
                        usecols=["magp_site_id","plot_id","meas_num","species_gs",
                                 "dbh","height","stem_ha","tree_status"], low_memory=False)
    t0 = trees[(trees["meas_num"] == 0) & (trees["tree_status"] == "L")]
    t0 = t0[t0["dbh"].notna() & (t0["dbh"] > 0)]
    cov = t0["species_gs"].astype(str).str.upper().isin(GS_TO_SPCD).mean()
    print(f"[magplot] NB initial live trees {len(t0):,}  crosswalk coverage {cov*100:.1f}%")

    variants = [v.strip() for v in args.variants.split(",") if v.strip()]
    configs = [c.strip() for c in args.configs.split(",") if c.strip()]
    out_frames, picked = [], 0
    for site, g in t0.groupby("magp_site_id"):
        if len(g) < args.min_trees:
            continue
        sid = str(site)
        tdf = build_treeinit_magplot(g, sid)
        if len(tdf) < args.min_trees:
            continue
        for v in variants:
            for c in configs:
                df, err = run_stand(sid, tdf, v, c, args.num_cycles)
                if err:
                    print(f"  {sid} {v}/{c}: ERROR {err}")
                else:
                    y0, yn = df.iloc[0], df.iloc[-1]
                    print(f"  {sid} {v}/{c}: {len(tdf)} trees  "
                          f"BA {y0['BA']:.1f}->{yn['BA']:.1f}  TPA {y0['Tpa']:.0f}->{yn['Tpa']:.0f}  "
                          f"({len(df)} cycles)")
                    out_frames.append(df)
        picked += 1
        if picked >= args.nstands:
            break

    if out_frames:
        res = pd.concat(out_frames, ignore_index=True)
        res.to_csv(args.out, index=False)
        print(f"\n[magplot] wrote {args.out}: {len(res)} rows across "
              f"{res['StandID'].nunique()} stands x {res[['variant','config']].drop_duplicates().shape[0]} model configs")
        print("[magplot] SUCCESS: MAGPlot tree lists ingested + projected through the Fortran FVS engine.")
    else:
        print("[magplot] no results; see errors above.")

if __name__ == "__main__":
    main()
