#!/bin/bash
#SBATCH --job-name=bench_sf_all
#SBATCH --time=01:00:00
#SBATCH --mem=80G
#SBATCH --cpus-per-task=4
#SBATCH --account=PUOM0008
#SBATCH --output=/fs/scratch/PUOM0008/crsfaaron/fvs-conus/logs/bench_sf_all_%j.out
#SBATCH --error=/fs/scratch/PUOM0008/crsfaaron/fvs-conus/logs/bench_sf_all_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=aaron.weiskittel@maine.edu
set -uo pipefail
module load gcc/12.3.0 R/4.4.0
cd /users/PUOM0008/crsfaaron/fvs-modern
B=calibration/dev_sf_integration/benchmark_sf_vs_legA.R
D=calibration/data/conus_remeasurement_pairs_metric_cond_v2.rds
OUT=calibration/output/conus/sf_integration/benchmark
V1=calibration/traits/species_traits.rds
V2=calibration/traits/species_traits_v2.rds

echo "##### HG (v1 traits, no variety split) #####"
Rscript --vanilla $B --component=hg --bundle_prefix=hg_v5_prod --data=$D \
  --traits=$V1 --variant_json=config/calibrated/ne.json --variety_split=FALSE \
  --test_n=200000 --outdir=$OUT

echo "##### HCB (v2 traits, variety split) #####"
Rscript --vanilla $B --component=hcb --bundle_prefix=hcb_v2split --data=$D \
  --traits=$V2 --variant_json=config/calibrated/ne.json --variety_split=TRUE \
  --test_n=200000 --outdir=$OUT

echo "##### HT-DBH (v2 traits, variety split) #####"
Rscript --vanilla $B --component=htdbh --bundle_prefix=htdbh_v2split --data=$D \
  --traits=$V2 --variant_json=config/calibrated/ne.json --variety_split=TRUE \
  --test_n=200000 --outdir=$OUT
