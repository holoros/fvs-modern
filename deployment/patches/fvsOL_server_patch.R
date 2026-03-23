# ==============================================================================
# fvsOL/R/server.R  --  PATCH for the fvsOL() entry function (lines 1-58)
# ==============================================================================
# This file shows the replacement for the top of fvsOL/R/server.R.
# Replace lines 1 through 58 of the original server.R with this block.
# The rest of server.R (line 59 onward: mkfvsStd, FVSOnlineUI, FVSOnlineServer)
# remains unchanged.
#
# Changes:
#   - Loads fvsol_config.R so cfg() is available throughout the app
#   - Upload size limit pulled from cfg("limits","max_upload_mb")
#   - Shiny trace pulled from cfg("shiny","trace")
#   - Launch browser pulled from cfg("shiny","launch_browser")
#   - fvsBin default falls back to cfg("paths","fvs_bin") when not supplied
# ==============================================================================

# The top of this file contains several objects loaded into the .GlobalEnv
# prior to the shinyApp call.

#' Run fvsOL (FVS OnLine/OnLocal).
#'
#' @param prjDir the name of the directory containing an fvsOL project.
#' @param runUUID the uuid of the run that should be opened when the system starts,
#'   if NULL or not found in the list of runs, it is ignored.
#' @param fvsBin the name of the directory containing the FVS load libraries for the platform.
#'   If NULL, reads from fvsol_config.yml via cfg("paths","fvs_bin").
#' @param shiny.trace turns on tracing for shiny, see shiny documentation
#' @param logToConsole controls if the log is output to the console or the log file,
#'   the default set by the interactive() function.
#' @return the shiny app.
#' @export
fvsOL <- function (prjDir=NULL,runUUID=NULL,fvsBin=NULL,shiny.trace=NULL,
                   logToConsole=interactive())
{
  require(stats)
  require(utils)

  # Load centralized configuration
  # Look for fvsol_config.R in the package inst/extdata or alongside this file
  config_candidates <- c(
    system.file("extdata", "fvsol_config.R", package = "fvsOL"),
    file.path(getwd(), "fvsol_config.R"),
    file.path(dirname(getwd()), "fvsol_config.R")
  )
  config_found <- FALSE
  for (cf in config_candidates) {
    if (nchar(cf) > 0 && file.exists(cf)) {
      source(cf, local = FALSE)
      config_found <- TRUE
      break
    }
  }
  if (!config_found) {
    message("[fvsOL] fvsol_config.R not found; using hardcoded defaults.")
    # Minimal fallback so cfg() exists even without the config loader
    cfg <- function(section, key) {
      defaults <- list(
        paths  = list(fvs_bin = "FVSbin"),
        limits = list(max_upload_mb = 10000),
        shiny  = list(trace = FALSE, launch_browser = TRUE)
      )
      val <- defaults[[section]][[key]]
      if (is.null(val)) stop(sprintf("Unknown config: %s/%s", section, key))
      val
    }
  }

  if (!is.null(prjDir) && dir.exists(prjDir)) setwd(prjDir)

  # Resolve FVS binary directory: explicit argument > config > local FVSbin
  if (is.null(fvsBin) || !dir.exists(fvsBin))
  {
    config_bin <- cfg("paths", "fvs_bin")
    if (dir.exists(config_bin)) {
      fvsBin <- config_bin
    } else if (dir.exists("FVSbin")) {
      fvsBin <- "FVSbin"
    } else {
      stop("fvsBin must be set: no valid binary directory found in argument, config, or local FVSbin/")
    }
  }
  fvsBin <<- fvsBin
  runUUID <<- runUUID
  logToConsole <<- logToConsole

  cat ("FVSOnline/OnLocal function fvsOL started.\n")

  addResourcePath("colourpicker-lib/js",
    system.file("www/shared/colourpicker/js", package="colourpicker"))
  addResourcePath("colourpicker-lib/css",
    system.file("www/shared/colourpicker/css",package="colourpicker"))
  addResourcePath("colourpicker-binding",
    system.file("srcjs",package="colourpicker"))
  addResourcePath("FVSlogo.png",
    system.file("extdata","www/FVSlogo.png",package="fvsOL"))
  addResourcePath("USDAFS.png",
    system.file("extdata","www/USDAFS.png",package="fvsOL"))
  addResourcePath("message-handler.js",
    system.file("extdata","www/message-handler.js",package="fvsOL"))
  if (!dir.exists ("www")) dir.create("www")
  addResourcePath("www",file.path(".","www"))
  addResourcePath("FVS_styles.css",
    system.file("extdata","www/FVS_styles.css", package="fvsOL"))

  # Resolve shiny trace: explicit argument > config
  if (is.null(shiny.trace)) shiny.trace <- cfg("shiny", "trace")

  # Set shiny options from config
  max_upload_bytes <- cfg("limits", "max_upload_mb") * 1024^2
  options(shiny.maxRequestSize = max_upload_bytes,
          shiny.trace = shiny.trace,
          rgl.inShiny = TRUE,
          rgl.useNULL = TRUE)

  data (prms)
  data (treeforms)

  cat ("Starting shinyApp.\n")

  shinyApp(FVSOnlineUI, FVSOnlineServer,
           options = list(launch.browser = cfg("shiny", "launch_browser")))
}

# === Everything below this line (mkfvsStd, FVSOnlineUI, FVSOnlineServer) ===
# === remains unchanged from the original server.R                         ===
