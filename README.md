# fvs-modern

A community maintained fork of the USDA Forest Vegetation Simulator, modernized for contemporary computing environments and designed for ease of deployment, variant creation, and calibration.

**Status:** Active development, April 2026
**Origin:** Forked from [USDAForestService/ForestVegetationSimulator](https://github.com/USDAForestService/ForestVegetationSimulator) and [ForestVegetationSimulator-Interface](https://github.com/USDAForestService/ForestVegetationSimulator-Interface)
**Contact:** Aaron Weiskittel, University of Maine (aaron.weiskittel@maine.edu)

## Why this fork exists

FVS is the most widely used individual tree growth and yield model in North America, with a codebase tracing back to 1973. The USDA Forest Service maintains the official version, but the pace of computing modernization has not kept up with the needs of the research and applied forestry community. This fork addresses three specific gaps:

1. **Modern computing standards.** The original codebase is entirely Fortran 77 fixed form with COMMON block global state. This fork converts to free form Fortran 90+, replaces computed GOTOs with SELECT CASE, fixes known runtime bugs, and provides a foundation for eventual module migration.

2. **Turnkey deployment.** Getting FVS-Online running on a new server has historically required deep institutional knowledge. This fork provides Docker containers, AWS AMI builders, platform-specific install scripts (Linux, macOS, Windows/WSL), and automated SSL configuration.

3. **Upstream tracking and variant extensibility.** This fork automatically monitors the USFS GitHub repos for new releases, provides a one-command sync workflow, and includes scaffolding tools for creating new regional variants with calibration templates.

## Repository structure

```
fvs-modern/
    src-converted/              Full FVS codebase in free-form Fortran 90 (2,247 files)
        base/                   Core simulation engine (253 files)
        vbase/                  Virtual base includes (20 files)
        ne/, ie/, cr/, ...      22 US + 2 Canadian regional variants
        fire/, estb/, volume/   Extension modules

    deployment/
        docker/                 Dockerfile, compose, entrypoint (Ubuntu 24.04)
        aws/                    Packer HCL for AMI + EC2 user-data
        scripts/                Platform installers and maintenance tools
            deploy_laptop.sh        Fedora/RHEL deployment
            setup_macos.sh          macOS deployment (Homebrew)
            setup_wsl.sh            Windows/WSL2 deployment
            setup_ssl.sh            Let's Encrypt automation
            check_upstream.sh       USFS GitHub change detection
            sync_upstream.sh        Pull, patch, convert, rebuild, test
            add_variant.sh          New variant scaffolding
            run_regression_tests.sh Full test suite (66/67 passing)
        config/                 NAMESPACE files, DESCRIPTION, YAML config
        microfvs/               REST API integration (FastAPI, based on microFVS)
        fvs2py/                 Python ctypes wrapper (based on Vibrant Planet fvs2py)

    modernization/              Conversion tools and analysis reports
    variant-tools/              Templates for new regional variants
    docs/                       Verification reports, roadmap, update lifecycle
```

## Quick start

### Option A: Docker (recommended for servers)

```bash
# fvsOL Shiny interface only
cd deployment/docker
docker compose up --build
# FVS-Online available at http://localhost:3838

# Combined: fvsOL + REST API + fvs2py
docker compose -f deployment/microfvs/docker-compose.yml up --build
# FVS-Online at http://localhost:3838
# REST API at http://localhost:8000 (Swagger docs at /docs)
# Jupyter + fvs2py at http://localhost:8888
```

### Option B: Platform-specific install

```bash
# Linux (Fedora/RHEL/Ubuntu)
bash deployment/scripts/deploy_laptop.sh

# macOS
bash deployment/scripts/setup_macos.sh

# Windows (via WSL2)
bash deployment/scripts/setup_wsl.sh
```

### Option C: AWS

```bash
cd deployment/aws
packer build fvs-online.pkr.hcl
# Launch the AMI; FVS-Online auto-starts on port 3838
```

## Upstream synchronization

This fork tracks the USFS GitHub repositories and provides automated tooling for integration:

```bash
# Check for new upstream commits
./deployment/scripts/check_upstream.sh

# Full sync: pull, patch, convert, rebuild, test
./deployment/scripts/sync_upstream.sh --all
```

A GitHub Actions workflow (`.github/workflows/upstream-sync.yml`) runs weekly and opens a PR when new USFS commits are detected. See `docs/update_lifecycle.md` for the full process.

## Creating a new variant

```bash
./deployment/scripts/add_variant.sh --name me --base ne \
    --description "Acadian variant calibrated for Maine"
```

This scaffolds the directory structure, species mapping template, and calibration parameter files. For runtime calibration without recompilation, use FVS keywords (READCORR, MORTMULT, SITEINDEX) in keyword files.

## Bayesian calibration

The `calibration/` directory contains a complete Bayesian recalibration pipeline for all 25 FVS variants using FIA remeasurement data. Six model components are refitted per variant: diameter growth, height growth, height to diameter allometry, mortality, crown ratio, and stand density (SDImax).

Key features:

- Hierarchical Bayesian models fit via Stan ADVI, producing 500 posterior draws per variant
- Calibrated parameters stored in `config/calibrated/` as JSON (point estimates and full posteriors)
- PERSEUS validation framework comparing projected AGB against observed FIA remeasurements for Maine
- Uncertainty propagation through FVS projections using posterior draw sampling
- Reproducible HPC workflow via SLURM array jobs (see `calibration/slurm/`)

For details on the calibration pipeline, see `CALIBRATION.md`. For the Python projection scripts, see `calibration/python/README.md`.

## Regression test results

| Category | Result |
|----------|--------|
| rFVS library load (24 variants) | 24/24 PASS (22 US + 2 Canadian) |
| Standalone simulations | 41/42 PASS (1 known segfault in iet03) |
| rFVS API simulation | 1/1 PASS |
| **Total** | **66/67 (98.5%)** |

## Code modernization status

| Item | Status | Notes |
|------|--------|-------|
| Free form conversion (.f -> .f90) | Complete | 2,247 files |
| FORMAT bug fixes | Complete | 11 instances in vbase/initre.f |
| Computed GOTO -> SELECT CASE | 12/17 in base/vbase | 5 complex dispatchers deferred |
| fvsOL namespace cleanup | Complete | dplyr removed, selective plyr/colourpicker imports |
| COMMON -> module migration | Roadmapped | Phase 1 of modernization plan |

## Related projects

* [USDA FVS](https://github.com/USDAForestService/ForestVegetationSimulator) (original, Fortran 77)
* [Open-FVS](https://sourceforge.net/projects/open-fvs/) (older community fork on SourceForge)
* [microFVS](https://github.com/Vibrant-Planet-Open-Science/microfvs) (REST API, Vibrant Planet)
* [fvs2py](https://github.com/Vibrant-Planet-Open-Science/fvs2py) (Python ctypes wrapper, Vibrant Planet)
* [docker_fvs](https://github.com/Vibrant-Planet-Open-Science/docker_fvs) (Dockerized FVS builds, Vibrant Planet)

## License

This fork inherits the public domain status of the original USDA Forest Vegetation Simulator. See `LICENSE`.
