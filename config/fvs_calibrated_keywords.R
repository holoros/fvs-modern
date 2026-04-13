#' FVS Calibrated Keywords Utility
#'
#' Provides functions to load Bayesian calibrated parameters from JSON config files
#' and generate FVS keywords for rFVS users, FVS-Online, and keyword file workflows.
#'
#' @docType package
#' @name fvs_calibrated_keywords
NULL

# Detect script directory for relative config path resolution
.get_script_dir <- function() {
  # First try to get the directory from sys.frame if sourced
  if (!interactive()) {
    frame <- sys.frame(1)
    if (!is.null(frame$ofile)) {
      return(dirname(frame$ofile))
    }
  }
  # Fallback: get working directory
  return(getwd())
}

#' Load FVS Configuration JSON
#'
#' Loads Bayesian calibrated or default parameters from a JSON config file.
#'
#' @param variant Character. Variant code (e.g., "ne", "nc", "se").
#' @param version Character. Which config to load: "default", "calibrated", or "draws".
#'   Default is "calibrated".
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#'
#' @return List with structure:
#'   - variant: character
#'   - categories: list containing growth, mortality, site_index, height_growth
#'
#' @examples
#' \dontrun{
#' config <- load_fvs_config("ne")
#' config_draws <- load_fvs_config("ne", version = "draws")
#' }
#'
#' @export
load_fvs_config <- function(variant, version = "calibrated", config_dir = NULL) {
  # Determine config directory
  if (is.null(config_dir)) {
    script_dir <- .get_script_dir()
    config_dir <- file.path(script_dir, "config")
  }

  # Build file path based on version
  if (version == "default") {
    json_path <- file.path(config_dir, paste0(variant, ".json"))
  } else if (version == "calibrated") {
    json_path <- file.path(config_dir, "calibrated", paste0(variant, ".json"))
  } else if (version == "draws") {
    json_path <- file.path(config_dir, "calibrated", paste0(variant, "_draws.json"))
  } else {
    stop("version must be 'default', 'calibrated', or 'draws'")
  }

  # Check file exists
  if (!file.exists(json_path)) {
    stop("Config file not found: ", json_path)
  }

  # Load and parse JSON
  tryCatch(
    {
      requireNamespace("jsonlite", quietly = TRUE) ||
        stop("jsonlite package required. Install with: install.packages('jsonlite')")
      config <- jsonlite::read_json(json_path)
      return(config)
    },
    error = function(e) {
      stop("Failed to load JSON from ", json_path, ": ", e$message)
    }
  )
}

#' Compute FVS Multipliers from Calibrated Parameters
#'
#' Calculates growth, mortality, height growth, and SDI max multipliers from
#' calibrated parameters. Multiplier math matches Python config_loader.py.
#'
#' @param variant Character. Variant code.
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#'
#' @return List with:
#'   - growth_mult: numeric vector, clipped to [0.1, 10.0]
#'   - mort_mult: numeric vector, clipped to [0.1, 10.0]
#'   - hg_mult: numeric vector, clipped to [0.1, 10.0]
#'   - sdi_max: numeric vector (direct values)
#'
#' @details
#' Multiplier formulas:
#'   - Growth: sqrt(exp(cal_B0 - def_B0)), clipped [0.1, 10.0]
#'   - Mortality: exp(cal_B0 - def_B0), clipped [0.1, 10.0]
#'   - Height growth: cal / default, clipped [0.1, 10.0]
#'   - SDI max: direct from site_index params (searches SDICON, R5SDI, R4SDI, FMSDI, SDIDEF)
#'
#' @examples
#' \dontrun{
#' mults <- compute_multipliers("ne")
#' }
#'
#' @export
compute_multipliers <- function(variant, config_dir = NULL) {
  # Load both default and calibrated configs
  def_cfg <- load_fvs_config(variant, version = "default", config_dir = config_dir)
  cal_cfg <- load_fvs_config(variant, version = "calibrated", config_dir = config_dir)

  # Extract B0 values from growth category
  def_growth_b0 <- unlist(def_cfg$categories$growth$B0)
  cal_growth_b0 <- unlist(cal_cfg$categories$growth$B0)

  # Growth multiplier: sqrt(exp(cal_B0 - def_B0))
  growth_mult <- sqrt(exp(cal_growth_b0 - def_growth_b0))
  growth_mult <- pmax(pmin(growth_mult, 10.0), 0.1)

  # Extract B0 values from mortality category (handle multiple naming conventions)
  mort_names <- c("MORT_B0", "B0", "MRT_B0")
  def_mort_b0 <- NULL
  cal_mort_b0 <- NULL

  for (name in mort_names) {
    if (name %in% names(def_cfg$categories$mortality)) {
      def_mort_b0 <- unlist(def_cfg$categories$mortality[[name]])
    }
    if (name %in% names(cal_cfg$categories$mortality)) {
      cal_mort_b0 <- unlist(cal_cfg$categories$mortality[[name]])
    }
    if (!is.null(def_mort_b0) && !is.null(cal_mort_b0)) break
  }

  if (is.null(def_mort_b0) || is.null(cal_mort_b0)) {
    stop("Could not find mortality B0 parameters in config")
  }

  # Mortality multiplier: exp(cal_B0 - def_B0)
  mort_mult <- exp(cal_mort_b0 - def_mort_b0)
  mort_mult <- pmax(pmin(mort_mult, 10.0), 0.1)

  # Extract height growth values (handle multiple naming conventions)
  hg_names <- c("HGLD", "HG_B0")
  def_hg <- NULL
  cal_hg <- NULL

  for (name in hg_names) {
    if (name %in% names(def_cfg$categories$height_growth)) {
      def_hg <- unlist(def_cfg$categories$height_growth[[name]])
    }
    if (name %in% names(cal_cfg$categories$height_growth)) {
      cal_hg <- unlist(cal_cfg$categories$height_growth[[name]])
    }
    if (!is.null(def_hg) && !is.null(cal_hg)) break
  }

  if (is.null(def_hg) || is.null(cal_hg)) {
    stop("Could not find height growth parameters in config")
  }

  # Height growth multiplier: cal / default
  hg_mult <- cal_hg / def_hg
  hg_mult <- pmax(pmin(hg_mult, 10.0), 0.1)

  # Extract SDI max from site_index (search in priority order)
  sdi_names <- c("SDICON", "R5SDI", "R4SDI", "FMSDI", "SDIDEF")
  sdi_max <- NULL

  for (name in sdi_names) {
    if (name %in% names(cal_cfg$categories$site_index)) {
      sdi_max <- unlist(cal_cfg$categories$site_index[[name]])
      break
    }
  }

  if (is.null(sdi_max)) {
    stop("Could not find SDI max parameters in config")
  }

  return(list(
    growth_mult = growth_mult,
    mort_mult = mort_mult,
    hg_mult = hg_mult,
    sdi_max = sdi_max
  ))
}

#' Generate FVS Keywords from Calibrated Parameters
#'
#' Creates FVS keyword strings (SDIMAX, MORTMULT, GROWMULT, HTGMULT) from
#' calibrated multipliers. Only includes keywords where multiplier differs
#' from 1.0 by more than 0.01.
#'
#' @param variant Character. Variant code.
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#' @param include_comments Logical. If TRUE, includes explanatory comment lines.
#'   Default is TRUE.
#'
#' @return Character string with FVS keywords, one per line.
#'
#' @details
#' Keyword formats must match exactly:
#'   - SDIMAX: `SDIMAX          {sp_index:10d}{value:10.1f}`
#'   - MORTMULT: `MORTMULT        {sp_index:10d}{mult:10.4f}       0.0     999.0`
#'   - GROWMULT: `GROWMULT        {sp_index:10d}{mult:10.4f}`
#'   - HTGMULT: `HTGMULT         {sp_index:10d}{mult:10.4f}`
#'
#' @examples
#' \dontrun{
#' keywords <- generate_fvs_keywords("ne")
#' cat(keywords)
#' }
#'
#' @export
generate_fvs_keywords <- function(variant, config_dir = NULL,
                                  include_comments = TRUE) {
  # Compute multipliers
  mults <- compute_multipliers(variant, config_dir = config_dir)

  lines <- character()

  # Add header comment if requested
  if (include_comments) {
    lines <- c(
      lines,
      paste("C Calibrated FVS keywords for variant:", variant),
      paste("C Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "C"
    )
  }

  # Number of species in vectors (assuming all vectors same length)
  n_species <- length(mults$growth_mult)

  # SDIMAX keywords
  sdi_keywords <- character()
  for (i in seq_along(mults$sdi_max)) {
    sdi_keywords <- c(
      sdi_keywords,
      sprintf(
        "SDIMAX          %10d%10.1f",
        i, mults$sdi_max[i]
      )
    )
  }

  # MORTMULT keywords (only if differs from 1.0 by > 0.01)
  mort_keywords <- character()
  for (i in seq_along(mults$mort_mult)) {
    if (abs(mults$mort_mult[i] - 1.0) > 0.01) {
      mort_keywords <- c(
        mort_keywords,
        sprintf(
          "MORTMULT        %10d%10.4f       0.0     999.0",
          i, mults$mort_mult[i]
        )
      )
    }
  }

  # GROWMULT keywords (only if differs from 1.0 by > 0.01)
  grow_keywords <- character()
  for (i in seq_along(mults$growth_mult)) {
    if (abs(mults$growth_mult[i] - 1.0) > 0.01) {
      grow_keywords <- c(
        grow_keywords,
        sprintf(
          "GROWMULT        %10d%10.4f",
          i, mults$growth_mult[i]
        )
      )
    }
  }

  # HTGMULT keywords (only if differs from 1.0 by > 0.01)
  htg_keywords <- character()
  for (i in seq_along(mults$hg_mult)) {
    if (abs(mults$hg_mult[i] - 1.0) > 0.01) {
      htg_keywords <- c(
        htg_keywords,
        sprintf(
          "HTGMULT         %10d%10.4f",
          i, mults$hg_mult[i]
        )
      )
    }
  }

  # Assemble final keyword string
  if (length(sdi_keywords) > 0) {
    lines <- c(lines, sdi_keywords)
  }
  if (length(mort_keywords) > 0) {
    if (include_comments) lines <- c(lines, "C Mortality multipliers")
    lines <- c(lines, mort_keywords)
  }
  if (length(grow_keywords) > 0) {
    if (include_comments) lines <- c(lines, "C Growth multipliers")
    lines <- c(lines, grow_keywords)
  }
  if (length(htg_keywords) > 0) {
    if (include_comments) lines <- c(lines, "C Height growth multipliers")
    lines <- c(lines, htg_keywords)
  }

  return(paste(lines, collapse = "\n"))
}

#' Inject Calibrated Keywords into FVS Keyfile
#'
#' Reads a .key file, inserts calibrated keywords before the PROCESS line,
#' and writes the result to a new file.
#'
#' @param keyfile_path Character. Path to input .key file.
#' @param variant Character. Variant code.
#' @param output_path Character. Path to output .key file. If NULL, overwrites
#'   the input file.
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#'
#' @return Invisibly returns the path to the output file.
#'
#' @details
#' Inserts keywords before the PROCESS line if found, otherwise appends at the end.
#'
#' @examples
#' \dontrun{
#' inject_keywords_into_keyfile("example.key", "ne", output_path = "example_cal.key")
#' }
#'
#' @export
inject_keywords_into_keyfile <- function(keyfile_path, variant,
                                         output_path = NULL, config_dir = NULL) {
  # Check input file exists
  if (!file.exists(keyfile_path)) {
    stop("Keyfile not found: ", keyfile_path)
  }

  # Set output path to input path if not specified
  if (is.null(output_path)) {
    output_path <- keyfile_path
  }

  # Read existing keyfile
  lines <- readLines(keyfile_path)

  # Generate calibrated keywords
  keywords <- generate_fvs_keywords(variant, config_dir = config_dir,
                                    include_comments = TRUE)

  # Find PROCESS line
  process_idx <- grep("^PROCESS", lines, ignore.case = TRUE)

  # Insert keywords before PROCESS line if found, otherwise append
  if (length(process_idx) > 0) {
    process_idx <- process_idx[1]
    new_lines <- c(
      lines[1:(process_idx - 1)],
      keywords,
      "",
      lines[process_idx:length(lines)]
    )
  } else {
    new_lines <- c(lines, "", keywords)
  }

  # Write output file
  writeLines(new_lines, output_path)

  return(invisible(output_path))
}

#' Sample One Posterior Draw from Draws JSON
#'
#' Loads the posterior draws JSON and extracts one draw, returning it as
#' a named list. Used for uncertainty propagation in ensemble runs.
#'
#' @param variant Character. Variant code.
#' @param draw_index Integer. Which draw to sample (1-indexed). If NULL,
#'   randomly samples one draw.
#' @param seed Integer. Random seed for reproducible sampling. Only used
#'   if draw_index is NULL.
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#'
#' @return List with structure matching one entry from draws JSON.
#'
#' @examples
#' \dontrun{
#' draw <- sample_posterior_draw("ne", draw_index = 1)
#' draw_random <- sample_posterior_draw("ne", seed = 42)
#' }
#'
#' @export
sample_posterior_draw <- function(variant, draw_index = NULL, seed = NULL,
                                  config_dir = NULL) {
  # Load draws config
  draws_cfg <- load_fvs_config(variant, version = "draws", config_dir = config_dir)

  # Extract draws array (could be at top level or in a 'draws' key)
  if ("draws" %in% names(draws_cfg)) {
    draws_list <- draws_cfg$draws
  } else if (is.list(draws_cfg) && length(draws_cfg) > 0) {
    # Assume the config itself is a list of draws
    draws_list <- draws_cfg
  } else {
    stop("Could not find draws array in ", variant, "_draws.json")
  }

  # Ensure it's a list
  if (!is.list(draws_list)) {
    stop("Draws array is not a list")
  }

  n_draws <- length(draws_list)

  # Select draw index
  if (is.null(draw_index)) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    draw_index <- sample(seq_len(n_draws), 1)
  } else {
    if (draw_index < 1 || draw_index > n_draws) {
      stop("draw_index out of range. Valid range: 1 to ", n_draws)
    }
  }

  return(draws_list[[draw_index]])
}

#' Generate FVS Keywords for One Posterior Draw
#'
#' Generates calibrated FVS keywords for a specific posterior draw.
#' Used for uncertainty propagation in ensemble runs.
#'
#' @param variant Character. Variant code.
#' @param draw_index Integer. Which draw to use. If NULL, randomly samples.
#' @param seed Integer. Random seed for reproducible sampling when draw_index is NULL.
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#'
#' @return Character string with FVS keywords.
#'
#' @examples
#' \dontrun{
#' keywords_draw1 <- generate_draw_keywords("ne", draw_index = 1)
#' keywords_random <- generate_draw_keywords("ne", seed = 42)
#' }
#'
#' @export
generate_draw_keywords <- function(variant, draw_index = NULL, seed = NULL,
                                   config_dir = NULL) {
  # Sample posterior draw
  draw <- sample_posterior_draw(variant, draw_index = draw_index, seed = seed,
                                config_dir = config_dir)

  # Compute multipliers from draw parameters
  # Assume draw has same structure as calibrated config
  if (is.null(config_dir)) {
    script_dir <- .get_script_dir()
    config_dir <- file.path(script_dir, "config")
  }

  # Load default config for reference
  def_cfg <- load_fvs_config(variant, version = "default", config_dir = config_dir)

  # Extract parameters from draw
  def_growth_b0 <- unlist(def_cfg$categories$growth$B0)
  draw_growth_b0 <- unlist(draw$categories$growth$B0)

  # Growth multiplier
  growth_mult <- sqrt(exp(draw_growth_b0 - def_growth_b0))
  growth_mult <- pmax(pmin(growth_mult, 10.0), 0.1)

  # Mortality
  mort_names <- c("MORT_B0", "B0", "MRT_B0")
  def_mort_b0 <- NULL
  draw_mort_b0 <- NULL

  for (name in mort_names) {
    if (name %in% names(def_cfg$categories$mortality)) {
      def_mort_b0 <- unlist(def_cfg$categories$mortality[[name]])
    }
    if (name %in% names(draw$categories$mortality)) {
      draw_mort_b0 <- unlist(draw$categories$mortality[[name]])
    }
    if (!is.null(def_mort_b0) && !is.null(draw_mort_b0)) break
  }

  if (is.null(def_mort_b0) || is.null(draw_mort_b0)) {
    stop("Could not find mortality B0 parameters in draw")
  }

  mort_mult <- exp(draw_mort_b0 - def_mort_b0)
  mort_mult <- pmax(pmin(mort_mult, 10.0), 0.1)

  # Height growth
  hg_names <- c("HGLD", "HG_B0")
  def_hg <- NULL
  draw_hg <- NULL

  for (name in hg_names) {
    if (name %in% names(def_cfg$categories$height_growth)) {
      def_hg <- unlist(def_cfg$categories$height_growth[[name]])
    }
    if (name %in% names(draw$categories$height_growth)) {
      draw_hg <- unlist(draw$categories$height_growth[[name]])
    }
    if (!is.null(def_hg) && !is.null(draw_hg)) break
  }

  if (is.null(def_hg) || is.null(draw_hg)) {
    stop("Could not find height growth parameters in draw")
  }

  hg_mult <- draw_hg / def_hg
  hg_mult <- pmax(pmin(hg_mult, 10.0), 0.1)

  # SDI max from draw
  sdi_names <- c("SDICON", "R5SDI", "R4SDI", "FMSDI", "SDIDEF")
  sdi_max <- NULL

  for (name in sdi_names) {
    if (name %in% names(draw$categories$site_index)) {
      sdi_max <- unlist(draw$categories$site_index[[name]])
      break
    }
  }

  if (is.null(sdi_max)) {
    stop("Could not find SDI max parameters in draw")
  }

  # Generate keywords from draw multipliers
  lines <- character()
  lines <- c(
    lines,
    paste("C Posterior draw keywords for variant:", variant),
    paste("C Draw index:", draw_index %||% "random"),
    paste("C Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "C"
  )

  # SDIMAX keywords
  for (i in seq_along(sdi_max)) {
    lines <- c(
      lines,
      sprintf("SDIMAX          %10d%10.1f", i, sdi_max[i])
    )
  }

  # MORTMULT keywords
  for (i in seq_along(mort_mult)) {
    if (abs(mort_mult[i] - 1.0) > 0.01) {
      lines <- c(
        lines,
        sprintf(
          "MORTMULT        %10d%10.4f       0.0     999.0",
          i, mort_mult[i]
        )
      )
    }
  }

  # GROWMULT keywords
  for (i in seq_along(growth_mult)) {
    if (abs(growth_mult[i] - 1.0) > 0.01) {
      lines <- c(
        lines,
        sprintf("GROWMULT        %10d%10.4f", i, growth_mult[i])
      )
    }
  }

  # HTGMULT keywords
  for (i in seq_along(hg_mult)) {
    if (abs(hg_mult[i] - 1.0) > 0.01) {
      lines <- c(
        lines,
        sprintf("HTGMULT         %10d%10.4f", i, hg_mult[i])
      )
    }
  }

  return(paste(lines, collapse = "\n"))
}

#' Run Uncertainty Ensemble
#'
#' Generates multiple modified keyfiles, each with a different posterior draw's
#' keywords injected. Used for uncertainty propagation via ensemble FVS runs.
#'
#' @param keyfile_path Character. Path to input .key file.
#' @param variant Character. Variant code.
#' @param n_draws Integer. Number of posterior draws to sample. Default is 50.
#' @param seed Integer. Random seed for reproducible sampling. Default is 42.
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#' @param output_dir Character. Directory to write output keyfiles. Default is ".".
#'
#' @return Data frame with columns:
#'   - draw_index: which draw was used
#'   - output_path: path to generated keyfile
#'
#' @details
#' Output filenames follow the pattern: `{basename}_{draw_number}.key`
#'
#' @examples
#' \dontrun{
#' results <- run_uncertainty_ensemble("example.key", "ne", n_draws = 100, seed = 42)
#' }
#'
#' @export
run_uncertainty_ensemble <- function(keyfile_path, variant, n_draws = 50,
                                     seed = 42, config_dir = NULL,
                                     output_dir = ".") {
  # Check input file exists
  if (!file.exists(keyfile_path)) {
    stop("Keyfile not found: ", keyfile_path)
  }

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Load draws to check how many are available
  draws_cfg <- load_fvs_config(variant, version = "draws", config_dir = config_dir)

  if ("draws" %in% names(draws_cfg)) {
    draws_list <- draws_cfg$draws
  } else {
    draws_list <- draws_cfg
  }

  n_available <- length(draws_list)

  if (n_draws > n_available) {
    warning(
      "Requested ", n_draws, " draws but only ", n_available,
      " available. Using all available draws."
    )
    n_draws <- n_available
  }

  # Seed for reproducible sampling
  set.seed(seed)

  # Sample draw indices without replacement
  draw_indices <- sample(seq_len(n_available), n_draws, replace = FALSE)

  # Generate output keyfiles
  results <- data.frame(
    draw_index = integer(),
    output_path = character(),
    stringsAsFactors = FALSE
  )

  # Get base filename
  base_name <- tools::file_path_sans_ext(basename(keyfile_path))
  ext <- tools::file_ext(keyfile_path)

  for (i in seq_along(draw_indices)) {
    draw_idx <- draw_indices[i]

    # Generate keywords for this draw
    keywords <- generate_draw_keywords(variant, draw_index = draw_idx,
                                       config_dir = config_dir)

    # Read existing keyfile
    lines <- readLines(keyfile_path)

    # Find PROCESS line
    process_idx <- grep("^PROCESS", lines, ignore.case = TRUE)

    # Insert keywords before PROCESS line
    if (length(process_idx) > 0) {
      process_idx <- process_idx[1]
      new_lines <- c(
        lines[1:(process_idx - 1)],
        keywords,
        "",
        lines[process_idx:length(lines)]
      )
    } else {
      new_lines <- c(lines, "", keywords)
    }

    # Write output keyfile
    output_filename <- sprintf("%s_%03d.%s", base_name, i, ext)
    output_path <- file.path(output_dir, output_filename)
    writeLines(new_lines, output_path)

    # Record result
    results <- rbind(results, data.frame(
      draw_index = draw_idx,
      output_path = output_path,
      stringsAsFactors = FALSE
    ))
  }

  return(results)
}

# Utility function for NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ============================================================================
# Usage Examples (uncommented for testing)
# ============================================================================

# Examples:
#
# # Load default and calibrated configs
# config_default <- load_fvs_config("ne", version = "default")
# config_calibrated <- load_fvs_config("ne", version = "calibrated")
#
# # Compute multipliers
# mults <- compute_multipliers("ne")
# str(mults)
#
# # Generate keywords
# keywords <- generate_fvs_keywords("ne", include_comments = TRUE)
# cat(keywords)
#
# # Inject keywords into a keyfile
# inject_keywords_into_keyfile("example.key", "ne", output_path = "example_cal.key")
#
# # Sample posterior draws
# draw <- sample_posterior_draw("ne", draw_index = 1)
# draw_random <- sample_posterior_draw("ne", seed = 123)
#
# # Generate keywords for a specific draw
# draw_keywords <- generate_draw_keywords("ne", draw_index = 5)
# cat(draw_keywords)
#
# # Run uncertainty ensemble
# results <- run_uncertainty_ensemble("example.key", "ne", n_draws = 50, seed = 42,
#                                     output_dir = "ensemble_runs")
# print(results)
