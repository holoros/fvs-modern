# =============================================================================
# launch_fvsonline.R
#
# Launcher script for FVS-Online. Called by systemd or run interactively.
# Reads configuration from fvsol_config.yml or environment variables.
# =============================================================================

# Load configuration
config_file <- Sys.getenv("FVSOL_CONFIG_FILE", "/srv/fvs/config/fvsol_config.yml")
if (file.exists(config_file)) {
  Sys.setenv(FVSOL_CONFIG_FILE = config_file)
}
source("/srv/fvs/config/fvsol_config.R")

# Load rFVS and fvsOL packages
library(rFVS)
library(fvsOL)

# Get settings from config
work_dir  <- cfg("paths", "work_dir")
fvs_bin   <- cfg("paths", "fvs_bin")
port      <- as.integer(cfg("shiny", "port"))

# Ensure directories exist
if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)

cat(sprintf("[%s] FVS-Online starting on port %d\n", Sys.time(), port))
cat(sprintf("  Work directory: %s\n", work_dir))
cat(sprintf("  FVS binaries:  %s\n", fvs_bin))

# Set working directory and launch
setwd(work_dir)
options(shiny.port = port, shiny.host = "127.0.0.1")

fvsOL(fvsBin = fvs_bin, shiny.trace = FALSE, logToConsole = TRUE)
