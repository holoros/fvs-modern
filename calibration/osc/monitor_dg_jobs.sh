#!/bin/bash
# Monitor DG HMC jobs on Cardinal
# Usage: bash monitor_dg_jobs.sh

echo "=== FVS Diameter Growth Calibration Status ==="
echo "Timestamp: $(date)"
echo ""

# Count completed vs running
COMPLETED=$(ssh crsfaaron@cardinal.osc.edu "ls ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/output/variants/*/diameter_growth_summary.csv 2>/dev/null | wc -l" 2>/dev/null)
MAP_DONE=$(ssh crsfaaron@cardinal.osc.edu "ls ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/output/variants/*/diameter_growth_map.csv 2>/dev/null | wc -l" 2>/dev/null)

echo "MAP optimization: ${MAP_DONE}/25 variants"
echo "HMC sampling:     ${COMPLETED}/25 variants"
echo ""

# Job array status
echo "=== SLURM Job Status ==="
ssh crsfaaron@cardinal.osc.edu "sacct -j 7589313 --format=JobID%15,State%12,Elapsed%10,ExitCode%8 2>/dev/null | grep -E '^\s+7589313_[0-9]+\s'"
echo ""

# Check dependent jobs
echo "=== Dependent Jobs ==="
ssh crsfaaron@cardinal.osc.edu "sacct -j 7589314,7589315 --format=JobID%15,JobName%15,State%12,Elapsed%10 2>/dev/null | tail -3"
echo ""

# List newly completed variants
echo "=== Recently Completed HMC ==="
ssh crsfaaron@cardinal.osc.edu "ls -lt ${FVS_PROJECT_ROOT:-/path/to/fvs-modern}/calibration/output/variants/*/diameter_growth_summary.csv 2>/dev/null | head -10"
