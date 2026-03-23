# ==============================================================================
# fvsol_config.R  --  Configuration loader for FVS-Online
# ==============================================================================
# Drop this file alongside server.R in both fvsOL and FVSPrjBldr directories.
# It reads fvsol_config.yml once and exposes a simple accessor: cfg("key").
#
# Lookup order for every value:
#   1. Environment variable  FVSOL_<SECTION>_<KEY>  (uppercase, underscores)
#      e.g.  FVSOL_SERVER_BASE_URL  overrides  server: base_url
#   2. YAML value from fvsol_config.yml
#   3. Built-in default (so the app still starts even without a YAML file)
#
# Usage:
#   source("fvsol_config.R")          # or source from package
#   cfg("server", "base_url")         # -> "https://..."
#   cfg("paths",  "work_dir")         # -> "/srv/fvs/projects"
#   cfg("projects", "retention_days") # -> 60
# ==============================================================================

if (!requireNamespace("yaml", quietly = TRUE)) {
  message("Installing yaml package (one-time setup)...")
  install.packages("yaml", repos = "https://cloud.r-project.org", quiet = TRUE)
}

# ---------------------------------------------------------------------------
# 1.  Built-in defaults (fallback when YAML is absent)
# ---------------------------------------------------------------------------
.fvsol_defaults <- list(
  server = list(
    base_url    = "https://localhost/FVSwork",
    institution = "FVS-Online",
    department  = ""
  ),
  paths = list(
    work_dir    = "/srv/fvs/projects",
    fvs_bin     = "/srv/fvs/bin",
    fvs_bin_dev = ""
  ),
  email = list(
    send_command   = "",
    subject_prefix = "FVSOnline project"
  ),
  projects = list(
    retention_days = 60
  ),
  limits = list(
    max_upload_mb = 10000
  ),
  logging = list(
    fvsol_log      = "FVSOnline.log",
    fvsol_log_prev = "FVSOnline.older.log",
    prjbldr_log    = "FVSPrjBldr.log"
  ),
  shiny = list(
    trace          = FALSE,
    launch_browser = TRUE
  )
)

# ---------------------------------------------------------------------------
# 2.  Load YAML file (search upward from working directory)
# ---------------------------------------------------------------------------
.fvsol_yaml <- list()

.fvsol_find_config <- function() {
  candidates <- c(
    Sys.getenv("FVSOL_CONFIG_FILE", ""),
    file.path(getwd(), "fvsol_config.yml"),
    file.path(dirname(getwd()), "fvsol_config.yml"),
    "/etc/fvsol/fvsol_config.yml"
  )
  for (f in candidates) {
    if (nchar(f) > 0 && file.exists(f)) return(f)
  }
  return(NULL)
}

.fvsol_config_path <- .fvsol_find_config()
if (!is.null(.fvsol_config_path)) {
  message("[fvsol_config] Loading: ", .fvsol_config_path)
  .fvsol_yaml <- yaml::yaml.load_file(.fvsol_config_path)
} else {
  message("[fvsol_config] No fvsol_config.yml found; using built-in defaults.")
  message("[fvsol_config] Set FVSOL_CONFIG_FILE or place fvsol_config.yml in the app directory.")
}

# ---------------------------------------------------------------------------
# 3.  Public accessor
# ---------------------------------------------------------------------------
#' Retrieve a configuration value.
#'
#' @param section  Top-level YAML key (e.g. "server", "paths", "email").
#' @param key      Second-level YAML key (e.g. "base_url", "work_dir").
#' @return Character or numeric value.
#' @examples
#'   cfg("paths", "work_dir")
#'   cfg("projects", "retention_days")
cfg <- function(section, key) {
  # 1. Environment variable override:  FVSOL_SERVER_BASE_URL
  env_name <- paste0("FVSOL_", toupper(section), "_", toupper(key))
  env_val  <- Sys.getenv(env_name, "")
  if (nchar(env_val) > 0) {
    # Coerce to numeric if the default is numeric
    default_val <- .fvsol_defaults[[section]][[key]]
    if (is.numeric(default_val)) return(as.numeric(env_val))
    if (is.logical(default_val)) return(as.logical(env_val))
    return(env_val)
  }

  # 2. YAML value
  yaml_val <- .fvsol_yaml[[section]][[key]]
  if (!is.null(yaml_val)) return(yaml_val)

  # 3. Built-in default
  default_val <- .fvsol_defaults[[section]][[key]]
  if (!is.null(default_val)) return(default_val)

  stop(sprintf("[fvsol_config] Unknown config key: %s / %s", section, key))
}

# ---------------------------------------------------------------------------
# 4.  Print summary on load (only if interactive or logging enabled)
# ---------------------------------------------------------------------------
if (interactive() || identical(Sys.getenv("FVSOL_LOG_CONFIG"), "TRUE")) {
  message("[fvsol_config] Active configuration:")
  message("  server.base_url     = ", cfg("server", "base_url"))
  message("  paths.work_dir      = ", cfg("paths", "work_dir"))
  message("  paths.fvs_bin       = ", cfg("paths", "fvs_bin"))
  message("  email.send_command  = ", cfg("email", "send_command"))
  message("  projects.retention  = ", cfg("projects", "retention_days"), " days")
  message("  limits.max_upload   = ", cfg("limits", "max_upload_mb"), " MB")
}
