#!/bin/bash
# =============================================================================
# FVS-Online Container Entrypoint
# =============================================================================

set -euo pipefail

FVS_BIN="/srv/fvs/bin"
FVS_PROJECTS="/srv/fvs/projects"
PORT="${FVS_PORT:-3838}"
HOST="${FVS_HOST:-0.0.0.0}"

case "${1:-fvsonline}" in

    fvsonline)
        echo "============================================"
        echo "  FVS-Online"
        echo "  $(ls $FVS_BIN/FVS*.so | wc -l) variant libraries"
        echo "  Listening on http://${HOST}:${PORT}"
        echo "============================================"

        exec R --no-save --quiet -e "
            library(rFVS)
            library(fvsOL)
            options(shiny.port = ${PORT}L, shiny.host = '${HOST}')
            fvsOL(prjDir = '${FVS_PROJECTS}', fvsBin = '${FVS_BIN}')
        "
        ;;

    test)
        echo "Running FVS verification tests..."
        R --no-save --quiet -e "
            library(rFVS)

            # Test each available variant
            soFiles <- list.files('${FVS_BIN}', pattern = 'FVS.*\\\\.so')
            cat(sprintf('Found %d variant libraries.\n', length(soFiles)))

            passed <- 0
            failed <- 0
            for (soFile in soFiles) {
                variant <- sub('\\\\.so$', '', soFile)
                result <- tryCatch({
                    fvsLoad(variant, bin = '${FVS_BIN}')
                    dims <- fvsGetDims()
                    cat(sprintf('[PASS] %-8s maxspecies=%d maxtrees=%d\n',
                        variant, dims['maxspecies'], dims['maxtrees']))
                    TRUE
                }, error = function(e) {
                    cat(sprintf('[FAIL] %-8s %s\n', variant, e\$message))
                    FALSE
                })
                if (result) passed <- passed + 1 else failed <- failed + 1
            }

            cat(sprintf('\nResults: %d passed, %d failed out of %d\n',
                passed, failed, length(soFiles)))
            if (failed > 0) quit(status = 1)
        "
        ;;

    simulate)
        # Run a quick simulation as a smoke test
        echo "Running NE variant simulation..."
        cd /srv/fvs/tests/APIviaR
        R --no-save --quiet -e "
            library(rFVS)
            fvsLoad('FVSie', bin = '${FVS_BIN}')
            fvsSetCmdLine('--keywordfile=base.key')
            fvsRun(0, 0)
            summ <- fvsGetSummary()
            cat('\n=== IE Variant 100-Year Projection ===\n')
            print(summ[, c('Year','Tpa','ATBA','TCuFt','BdFt')])
            cat('\nSimulation PASSED.\n')
        "
        ;;

    shell|bash)
        exec /bin/bash
        ;;

    *)
        exec "$@"
        ;;
esac
