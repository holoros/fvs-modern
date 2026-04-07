#!/usr/bin/env python3
"""
Extract ClimateSI and Emmerson SDIMAX raster values at FIA plot locations.
Creates a CSV lookup table: PLT_CN, LAT, LON, ClimateSI_ft, SDIMAX_imperial

ClimateSI raster: Lambert Azimuthal Equal Area, 1km, values in meters
  -> converted to feet (* 3.28084)
SDIMAX raster: WGS84, 30m, values in metric SDI (TPH at 25cm reference)
  -> converted to imperial SDI (TPA at 10in reference) via * 0.3945

Metric-to-imperial SDI conversion:
  SDI_imp = SDI_met * (1/2.47105) * (25/25.4)^1.605 = SDI_met * 0.3945
"""

import csv
import sys
import time
import numpy as np

try:
    import rasterio
except ImportError:
    print("ERROR: rasterio not available. Install with:")
    print("  pip3 install --user rasterio")
    sys.exit(1)

# Paths
FIA_PLOT_CSV = "/users/PUOM0008/crsfaaron/FIA/ENTIRE_PLOT.csv"
CLIMATE_SI_TIF = "/users/PUOM0008/crsfaaron/SiteIndex/ClimateNA_SI_m.tif"
SDIMAX_TIF = "/users/PUOM0008/crsfaaron/SiteIndex/TREEMAP_SDImax.tif"
OUTPUT_CSV = "/users/PUOM0008/crsfaaron/fvs-modern/calibration/data/plot_raster_lookup.csv"

M_TO_FT = 3.28084
SDI_MET_TO_IMP = 0.3945  # (1/2.47105) * (25/25.4)^1.605


def read_fia_plots(csv_path):
    """Read FIA plot locations (CN, LAT, LON) from ENTIRE_PLOT.csv."""
    plots = {}
    print(f"Reading FIA plots from {csv_path}...")
    with open(csv_path, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            cn = row.get('CN', '').strip()
            lat = row.get('LAT', '').strip()
            lon = row.get('LON', '').strip()
            if cn and lat and lon:
                try:
                    lat_f = float(lat)
                    lon_f = float(lon)
                    if -90 <= lat_f <= 90 and -180 <= lon_f <= 180:
                        plots[cn] = (lat_f, lon_f)
                except ValueError:
                    continue
    print(f"  Read {len(plots)} plots with valid coordinates")
    return plots


def extract_climate_si(plots, tif_path):
    """Extract ClimateSI values at plot locations, convert m -> ft."""
    print(f"\nExtracting ClimateSI from {tif_path}...")
    t0 = time.time()
    results = {}

    with rasterio.open(tif_path) as src:
        raster_crs = src.crs
        transform = src.transform
        data = src.read(1)  # Small raster (7688x7617), fits in memory
        nodata = src.nodata
        h, w = data.shape
        print(f"  Raster: {w}x{h}, CRS={raster_crs}")

        # Create transformer from WGS84 (lat/lon) to raster CRS (LAEA)
        try:
            from pyproj import Transformer, CRS
            raster_crs_proj = CRS.from_wkt(raster_crs.to_wkt())
            transformer = Transformer.from_crs("EPSG:4326", raster_crs_proj, always_xy=True)
        except Exception as e:
            print(f"  WARNING: CRS from WKT failed: {e}")
            print("  Using manual LAEA definition...")
            from pyproj import Transformer
            laea_crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
            transformer = Transformer.from_crs("EPSG:4326", laea_crs, always_xy=True)

        n_valid = 0
        n_nodata = 0
        n_oob = 0

        for cn, (lat, lon) in plots.items():
            x, y = transformer.transform(lon, lat)
            col = int((x - transform.c) / transform.a)
            row = int((y - transform.f) / transform.e)

            if 0 <= row < h and 0 <= col < w:
                val = data[row, col]
                if np.isfinite(val) and val > 0:
                    results[cn] = round(float(val) * M_TO_FT, 2)
                    n_valid += 1
                else:
                    n_nodata += 1
            else:
                n_oob += 1

        print(f"  Valid: {n_valid}, NoData: {n_nodata}, Out of bounds: {n_oob}")
        print(f"  Elapsed: {time.time() - t0:.1f}s")

    return results


def extract_sdimax(plots, tif_path):
    """Extract Emmerson SDIMAX (metric) at plot locations using rasterio.sample()."""
    print(f"\nExtracting SDIMAX from {tif_path}...")
    print(f"  Conversion: SDI_metric * {SDI_MET_TO_IMP} = SDI_imperial")
    t0 = time.time()
    results = {}

    with rasterio.open(tif_path) as src:
        nodata = src.nodata
        h, w = src.height, src.width
        print(f"  Raster: {w}x{h}, CRS={src.crs}")
        print(f"  NoData value: {nodata}")

        # Build coordinate array: rasterio.sample() expects (lon, lat) = (x, y)
        # since the raster CRS is WGS84 (EPSG:4979)
        cn_list = list(plots.keys())
        coords = [(plots[cn][1], plots[cn][0]) for cn in cn_list]  # (lon, lat)

        n_valid = 0
        n_nodata = 0

        # Process in batches to manage memory
        batch_size = 50000
        for batch_start in range(0, len(cn_list), batch_size):
            batch_end = min(batch_start + batch_size, len(cn_list))
            batch_coords = coords[batch_start:batch_end]
            batch_cns = cn_list[batch_start:batch_end]

            # rasterio.sample() returns generator of arrays
            for cn, vals in zip(batch_cns, src.sample(batch_coords)):
                val = vals[0]
                if np.isfinite(val) and val != nodata and val > 0 and val < 10000:
                    results[cn] = round(float(val) * SDI_MET_TO_IMP, 1)
                    n_valid += 1
                else:
                    n_nodata += 1

            if (batch_end) % 200000 == 0 or batch_end == len(cn_list):
                elapsed = time.time() - t0
                print(f"    Processed {batch_end} / {len(cn_list)} plots ({elapsed:.0f}s)")

        print(f"  Valid: {n_valid}, NoData: {n_nodata}")
        print(f"  Elapsed: {time.time() - t0:.1f}s")

    return results


def main():
    print("=" * 70)
    print("FIA Plot Raster Value Extraction")
    print("ClimateSI (m -> ft) + Emmerson SDIMAX (metric -> imperial)")
    print("=" * 70)

    plots = read_fia_plots(FIA_PLOT_CSV)

    climate_si = extract_climate_si(plots, CLIMATE_SI_TIF)
    sdimax = extract_sdimax(plots, SDIMAX_TIF)

    # Write combined lookup
    print(f"\nWriting lookup CSV to {OUTPUT_CSV}...")
    n_both = 0
    n_si_only = 0
    n_sdi_only = 0
    n_neither = 0

    with open(OUTPUT_CSV, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(['PLT_CN', 'LAT', 'LON', 'ClimateSI_ft', 'SDIMAX_imperial'])

        for cn, (lat, lon) in plots.items():
            si_val = climate_si.get(cn, '')
            sdi_val = sdimax.get(cn, '')

            if si_val and sdi_val:
                n_both += 1
            elif si_val:
                n_si_only += 1
            elif sdi_val:
                n_sdi_only += 1
            else:
                n_neither += 1

            if si_val or sdi_val:
                writer.writerow([cn, lat, lon, si_val, sdi_val])

    print(f"\n  Summary:")
    print(f"    Both values: {n_both}")
    print(f"    ClimateSI only: {n_si_only}")
    print(f"    SDIMAX only: {n_sdi_only}")
    print(f"    Neither: {n_neither}")
    print(f"    Total written: {n_both + n_si_only + n_sdi_only}")

    if climate_si:
        vals = list(climate_si.values())
        print(f"\n  ClimateSI (ft): mean={np.mean(vals):.1f}, median={np.median(vals):.1f}, "
              f"min={np.min(vals):.1f}, max={np.max(vals):.1f}")
    if sdimax:
        vals = list(sdimax.values())
        print(f"  SDIMAX (imperial): mean={np.mean(vals):.1f}, median={np.median(vals):.1f}, "
              f"min={np.min(vals):.1f}, max={np.max(vals):.1f}")

    print("\nDone!")


if __name__ == "__main__":
    main()
