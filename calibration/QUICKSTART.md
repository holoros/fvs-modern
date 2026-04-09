# FVS Calibration Pipeline: Quick Start Guide

## For the Impatient

### 1. One-time Setup

```bash
cd /path/to/Documents/Claude/fvs-modern
Rscript calibration/R/00_setup.R
```

Takes ~10 minutes. Installs packages and configures CmdStan.

### 2. Run Full Calibration on OSC

```bash
sbatch calibration/slurm/submit_calibration.sh
```

- Submits 25 parallel jobs (one per variant)
- Wall time: ~3 hours
- Check status: `squeue -u $USER`
- Monitor: `tail -f calibration/logs/array_*.log`

### 3. Check Results

After completion:

```bash
# Browse output
ls -lh calibration/output/variants/{variant}/

# View diagnostics
open calibration/output/variants/ca/diagnostics/DIAGNOSTICS_REPORT.md

# Check calibrated configs
ls -lh config/calibrated/
```

## For Local Testing

Test one variant locally:

```bash
bash calibration/slurm/run_variant.sh ca
```

Takes ~2 hours on a 4-core machine.

## Key Files

| File | Purpose |
|---|---|
| `R/00_setup.R` | Install packages, configure environment |
| `R/01_fetch_fia_data.R` | Download FIA data (run once) |
| `R/02_fit_diameter_growth.R` | Fit main Wykoff growth model |
| `R/03_fit_height_diameter.R` | Fit height-diameter curves |
| `R/04_fit_mortality.R` | Fit tree survival model |
| `R/05_fit_crown_ratio.R` | Fit crown ratio change model |
| `R/06_posterior_to_json.R` | Convert posteriors to FVS JSON |
| `R/07_diagnostics.R` | Generate validation plots |
| `stan/wykoff_dg.stan` | Stan model (main growth equation) |
| `slurm/submit_calibration.sh` | Submit all 25 jobs at once |
| `slurm/run_variant.sh` | Single-variant pipeline |

## What Gets Generated

Per variant:
- `output/variants/{variant}/diameter_growth_posterior.csv` - Posterior estimates
- `output/variants/{variant}/diameter_growth_samples.rds` - Full MCMC samples
- `config/calibrated/{variant}.json` - Calibrated FVS parameters
- `output/variants/{variant}/diagnostics/*.pdf` - Diagnostic plots
- `logs/variant_{variant}.log` - Execution log

## Troubleshooting

**Q: Where are the logs?**
A: `calibration/logs/` contains logs from each step and variant.

**Q: How do I use the calibrated configs?**
A: Copy to the fvs-modern config directory or load directly in your R scripts.

**Q: Can I stop and resume?**
A: Each variant is independent. Rerunning a variant will overwrite previous results.

**Q: How much disk space is needed?**
A: ~60 GB for FIA data, ~5 GB for all outputs and logs.

**Q: What if a job fails?**
A: Check `logs/variant_{variant}.log` for errors. Fix and resubmit:
```bash
bash calibration/slurm/run_variant.sh {variant}
```

## Next Steps

1. Read `README.md` for full documentation
2. Review `stan/wykoff_dg.stan` to understand the model
3. Check diagnostic plots in `output/variants/{variant}/diagnostics/`
4. Compare original vs calibrated parameters in JSON files
5. Integrate calibrated configs into fvs-modern simulations

## Questions?

See README.md for detailed documentation, parameter mapping, and interpretation guidance.
