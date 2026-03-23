# =============================================================================
# launch_prjbldr.R
#
# Launcher script for FVS Project Builder. Called by systemd or run interactively.
# Reads configuration from fvsol_config.yml or environment variables.
# =============================================================================

# Load configuration
config_file <- Sys.getenv("FVSOL_CONFIG_FILE", "/srv/fvs/config/fvsol_config.yml")
if (file.exists(config_file)) {
  Sys.setenv(FVSOL_CONFIG_FILE = config_file)
}
source("/srv/fvs/config/fvsol_config.R")

library(shiny)

# Get settings from config
work_dir    <- cfg("paths", "work_dir")
fvs_bin     <- cfg("paths", "fvs_bin")
base_url    <- cfg("server", "base_url")
institution <- cfg("server", "institution")

cat(sprintf("[%s] FVS Project Builder starting on port 3839\n", Sys.time()))

# Override the hardcoded Virginia Tech paths with config values
options(shiny.port = 3839L, shiny.host = "127.0.0.1")

# Source the patched server and UI
# These are the drop-in replacements from deployment/patches/
source("/srv/fvs/patches/FVSPrjBldr_server.R")
source("/srv/fvs/interface/FVSPrjBldr/ui.R")

shinyApp(ui = FVSPrjBldr_ui, server = FVSPrjBldr_server)
