#!/bin/bash
#SBATCH --job-name=unified_smoke
#SBATCH --time=01:00:00
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4
#SBATCH --account=PUOM0008
#SBATCH --output=/fs/scratch/PUOM0008/crsfaaron/fvs-conus/logs/unified_smoke_%j.out
#SBATCH --error=/fs/scratch/PUOM0008/crsfaaron/fvs-conus/logs/unified_smoke_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=aaron.weiskittel@maine.edu
# Smoke test of the unified models (species-free hierarchy + species random
# effect). Confirms the models compile and sample, and reports the species RE
# scale sigma_sp (is there species signal beyond traits?).
set -uo pipefail
module load gcc/12.3.0 R/4.4.0
cd /users/PUOM0008/crsfaaron/fvs-modern
O=calibration/output/conus/sf_integration/unified_smoke
V2=calibration/traits/species_traits_v2.rds

echo "########## HCB unified smoke ##########"
Rscript --vanilla calibration/R/33_fit_hcb_speciesfree.R \
  --stan_file=calibration/stan/hcb_unified.stan --traits=$V2 \
  --subsample=60000 --smoke --outdir=$O --outname=hcb_unified_smoke 2>&1 | tail -25
Rscript -e 'f<-readRDS("calibration/output/conus/sf_integration/unified_smoke/hcb_unified_smoke_fit.rds"); print(f$summary(variables=c("sigma_sp","sigma_L1","sigma_L2","sigma_L3","sigma_FT","phi"),"mean","sd","rhat"))' 2>&1 | tail -10

echo "########## HT-DBH unified smoke ##########"
Rscript --vanilla calibration/R/36_fit_htdbh_speciesfree.R \
  --stan_file=calibration/stan/htdbh_unified.stan --traits=$V2 \
  --subsample=60000 --smoke --outdir=$O --outname=htdbh_unified_smoke 2>&1 | tail -25
Rscript -e 'f<-readRDS("calibration/output/conus/sf_integration/unified_smoke/htdbh_unified_smoke_fit.rds"); print(f$summary(variables=c("sigma_sp","sigma","sigma_L1","sigma_L2","sigma_L3","sigma_FT"),"mean","sd","rhat"))' 2>&1 | tail -10
echo "DONE"
