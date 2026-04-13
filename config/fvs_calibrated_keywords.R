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

# Utility function for NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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
#' Returns NULL for any component where required parameters are unavailable.
#'
#' @param variant Character. Variant code.
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#'
#' @return List with available components (may be partial):
#'   - growth_mult: numeric vector, clipped to [0.1, 10.0] (or NULL)
#'   - mort_mult: numeric vector, clipped to [0.1, 10.0] (or NULL)
#'   - hg_mult: numeric vector, clipped to [0.1, 10.0] (or NULL)
#'   - sdi_max: numeric vector (direct values, or NULL)
#'   - bamax: numeric vector (direct values, or NULL)
#'
#' @details
#' Multiplier formulas:
#'   - Growth: sqrt(exp(cal_B0 - def_B0)), clipped [0.1, 10.0]
#'   - Mortality: exp(cal_B0 - def_B0), clipped [0.1, 10.0]
#'   - Height growth: cal / default, clipped [0.1, 10.0]
#'   - SDI max: direct from calibrated site_index (SDICON, R5SDI, R4SDI, etc.)
#'   - BAMAX: direct from calibrated site_index (BAMAXA, BAMAX1, BAMAX)
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

  result <- list()

  # ---- Growth multiplier: sqrt(exp(cal_B0 - def_B0)) ----
  tryCatch({
    def_growth <- def_cfg$categories$growth
    cal_growth <- cal_cfg$categories$growth
    if (!is.null(def_growth) && !is.null(cal_growth)) {
      # Try B0 first, then B1 as fallback
      for (bname in c("B0", "B1", "WEIBB1")) {
        if (bname %in% names(cal_growth) && bname %in% names(def_growth)) {
          cal_b0 <- unlist(cal_growth[[bname]])
          def_b0 <- unlist(def_growth[[bname]])
          n <- min(length(cal_b0), length(def_b0))
          if (n > 0) {
            mult <- ifelse(
              def_b0[1:n] != 0,
              sqrt(pmax(pmin(exp(cal_b0[1:n] - def_b0[1:n]), 10.0), 0.1)),
              1.0
            )
            result$growth_mult <- mult
            break
          }
        }
      }
    }
  }, error = function(e) {
    message("Note: Could not compute growth multipliers: ", e$message)
  })

  # ---- Mortality multiplier: exp(cal_B0 - def_B0) ----
  tryCatch({
    def_mort <- def_cfg$categories$mortality
    cal_mort <- cal_cfg$categories$mortality
    if (!is.null(def_mort) && is.list(def_mort) && length(def_mort) > 0 &&
        !is.null(cal_mort) && is.list(cal_mort) && length(cal_mort) > 0) {
      for (bname in c("MORT_B0", "B0", "MRT_B0")) {
        if (bname %in% names(cal_mort) && bname %in% names(def_mort)) {
          cal_b0 <- unlist(cal_mort[[bname]])
          def_b0 <- unlist(def_mort[[bname]])
          n <- min(length(cal_b0), length(def_b0))
          if (n > 0) {
            odds <- ifelse(
              def_b0[1:n] != 0,
              exp(cal_b0[1:n] - def_b0[1:n]),
              1.0
            )
            result$mort_mult <- pmax(pmin(odds, 10.0), 0.1)
            break
          }
        }
      }
    }
  }, error = function(e) {
    message("Note: Could not compute mortality multipliers: ", e$message)
  })

  # ---- Height growth multiplier: cal / default ----
  tryCatch({
    for (cat_name in c("height_diameter", "height_growth", "growth")) {
      cal_cat <- cal_cfg$categories[[cat_name]]
      def_cat <- def_cfg$categories[[cat_name]]
      if (!is.null(cal_cat) && is.list(cal_cat) && !is.null(def_cat) && is.list(def_cat)) {
        for (bname in c("HD_A", "HGLD", "HG_B0")) {
          if (bname %in% names(cal_cat) && bname %in% names(def_cat)) {
            cal_vals <- unlist(cal_cat[[bname]])
            def_vals <- unlist(def_cat[[bname]])
            n <- min(length(cal_vals), length(def_vals))
            if (n > 0) {
              mult <- ifelse(
                def_vals[1:n] != 0,
                cal_vals[1:n] / def_vals[1:n],
                1.0
              )
              result$hg_mult <- pmax(pmin(mult, 10.0), 0.1)
              break
            }
          }
        }
      }
      if (!is.null(result$hg_mult)) break
    }
  }, error = function(e) {
    message("Note: Could not compute height growth multipliers: ", e$message)
  })

  # ---- SDI max: direct from calibrated config ----
  tryCatch({
    site_cats <- cal_cfg$categories$site_index %||% list()
    other_cats <- cal_cfg$categories$other %||% list()
    all_cats <- c(site_cats, other_cats)
    for (name in c("SDICON", "R5SDI", "R4SDI", "FMSDI", "SDIDEF")) {
      if (name %in% names(all_cats)) {
        result$sdi_max <- unlist(all_cats[[name]])
        break
      }
    }
  }, error = function(e) {
    message("Note: Could not extract SDI max: ", e$message)
  })

  # ---- BAMAX: direct from calibrated config ----
  tryCatch({
    site_cats <- cal_cfg$categories$site_index %||% list()
    other_cats <- cal_cfg$categories$other %||% list()
    all_cats <- c(site_cats, other_cats)
    for (name in c("BAMAXA", "BAMAX1", "BAMAX")) {
      if (name %in% names(all_cats)) {
        result$bamax <- unlist(all_cats[[name]])
        break
      }
    }
  }, error = function(e) {
    message("Note: Could not extract BAMAX: ", e$message)
  })

  if (length(result) == 0) {
    warning("No calibration parameters could be computed for variant '", variant, "'")
  }

  return(result)
}

#' Generate FVS Keywords from Calibrated Parameters
#'
#' Creates FVS keyword strings (SDIMAX, BAMAX, MORTMULT, GROWMULT, HTGMULT) from
#' calibrated multipliers. Only includes keywords where multiplier differs
#' from 1.0 by more than 0.01. Gracefully skips unavailable components.
#'
#' @param variant Character. Variant code.
#' @param config_dir Character. Path to config directory. If NULL, defaults to
#'   config/ relative to script location.
#' @param include_comments Logical. If TRUE, includes explanatory comment lines.
#'   Default is TRUE.
#'
#' @return Character string with FVS keywords, one per line.
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
  # Compute multipliers (returns only available components)
  mults <- compute_multipliers(variant, config_dir = config_dir)

  lines <- character()

  # Add header comment if requested
  if (include_comments) {
    lines <- c(
      lines,
      paste("!! Bayesian calibrated parameters for variant", toupper(variant)),
      "!!"
    )
  }

  # SDIMAX keywords
  if (!is.null(mults$sdi_max)) {
    if (include_comments) lines <- c(lines, "!! Species specific SDI maximums")
    for (i in seq_along(mults$sdi_max)) {
      val <- mults$sdi_max[i]
      if (!is.na(val) && is.numeric(val) && val > 0) {
        lines <- c(lines, sprintf("SDIMAX          %10d%10.1f", i, val))
      }
    }
  }

  # BAMAX keywords
  if (!is.null(mults$bamax)) {
    if (include_comments) lines <- c(lines, "!! Maximum basal area")
    for (i in seq_along(mults$bamax)) {
      val <- mults$bamax[i]
      if (!is.na(val) && is.numeric(val) && val > 0) {
        lines <- c(lines, sprintf("BAMAX           %10d%10.1f", i, val))
      }
    }
  }

  # MORTMULT keywords (only if differs from 1.0 by > 0.01)
  if (!is.null(mults$mort_mult)) {
    if (include_comments) lines <- c(lines, "!! Mortality multipliers")
    for (i in seq_along(mults$mort_mult)) {
      if (abs(mults$mort_mult[i] - 1.0) > 0.01) {
        lines <- c(
          lines,
          sprintf("MORTMULT        %10d%10.4f       0.0     999.0", i, mults$mort_mult[i])
        )
      }
    }
  }

  # GROWMULT keywords (only if differs from 1.0 by > 0.01)
  if (!is.null(mults$growth_mult)) {
    if (include_comments) lines <- c(lines, "!! Growth multipliers")
    for (i in seq_along(mults$growth_mult)) {
      if (abs(mults$growth_mult[i] - 1.0) > 0.01) {
        lines <- c(
          lines,
          sprintf("GROWMULT        %10d%10.4f", i, mults$growth_mult[i])
        )
      }
    }
  }

  # HTGMULT keywords (only if differs from 1.0 by > 0.01)
  if (!is.null(mults$hg_mult)) {
    if (include_comments) lines <- c(lines, "!! Height growth multipliers")
    for (i in seq_along(mults$hg_mult)) {
      if (abs(mults$hg_mult[i] - 1.0) > 0.01) {
        lines <- c(
          lines,
          sprintf("HTGMULT         %10d%10.4f", i, mults$hg_mult[i])
        )
      }
    }
  }

  if (length(lines) <= 2 && include_comments) {
    warning("No calibration keywords generated for variant '", variant,
            "'. Config may lack comparable default/calibrated parameters.")
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
#'   Each entry is a named list of components (diameter_growth, mortality, etc.),
#'   each containing parameter name/value pairs from the posterior.
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

  # The draws JSON has structure:
  #   { variant, n_draws, draws: { component: [{param: val, ...}, ...], ... } }
  # Navigate into the draws sub object
  if ("draws" %in% names(draws_cfg)) {
    draws_data <- draws_cfg$draws
  } else {
    draws_data <- draws_cfg
  }

  component_names <- names(draws_data)
  if (length(component_names) == 0) {
    stop("No components found in ", variant, "_draws.json")
  }

  # Determine number of draws from first component
  first_comp <- draws_data[[component_names[1]]]
  n_draws <- length(first_comp)

  # Select draw index
  if (is.null(draw_index)) {
    if (!is.null(seed)) set.seed(seed)
    draw_index <- sample(seq_len(n_draws), 1)
  } else {
    if (draw_index < 1 || draw_index > n_draws) {
      stop("draw_index out of range. Valid range: 1 to ", n_draws)
    }
  }

  # Extract the selected draw from each component
  draw <- list()
  for (comp in component_names) {
    draw[[comp]] <- draws_data[[comp]][[draw_index]]
  }

  attr(draw, "draw_index") <- draw_index
  return(draw)
}

#' Generate FVS Keywords for One Posterior Draw
#'
#' Generates calibrated FVS keywords for a specific posterior draw using
#' the hierarchical Stan/brms parameterization. Handles two styles:
#'   - Stan non centered: mu_b0 + sigma_b0 * z_b0[i] (diameter growth)
#'   - brms random effects: b_Intercept + r_SPCD[code,Intercept] (mortality, SDI)
#'
#' FIA species codes in random effects are mapped to FVS species indices
#' using the FIAJSP array from the variant config.
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
  actual_idx <- attr(draw, "draw_index") %||% draw_index %||% "random"

  # Load default config for reference
  if (is.null(config_dir)) {
    script_dir <- .get_script_dir()
    config_dir <- file.path(script_dir, "config")
  }
  def_cfg <- load_fvs_config(variant, version = "default", config_dir = config_dir)

  # Build FIA to FVS species index mapping
  sp_defs <- def_cfg$categories$species_definitions
  fiajsp <- unlist(sp_defs$FIAJSP)
  maxsp <- length(fiajsp)
  fia_to_fvs <- setNames(seq_along(fiajsp), as.character(as.integer(fiajsp)))

  lines <- c(
    paste("!! Posterior draw", actual_idx, "for variant", toupper(variant)),
    "!!"
  )

  # ---- SDI max from stand_density (brms: b_Intercept + r_SPCD[code,Intercept]) ----
  if ("stand_density" %in% names(draw)) {
    sd_params <- draw$stand_density
    fixed <- sd_params$b_Intercept
    if (!is.null(fixed)) {
      fixed <- as.numeric(fixed)
      sdi_vals <- rep(exp(fixed), maxsp)  # default: all species get fixed effect

      # Add species random effects
      r_pattern <- "^r_SPCD\\[(\\d+),Intercept\\]$"
      r_keys <- grep(r_pattern, names(sd_params), value = TRUE)
      for (key in r_keys) {
        fia_code <- sub(r_pattern, "\\1", key)
        fvs_idx <- fia_to_fvs[fia_code]
        if (!is.na(fvs_idx)) {
          re_val <- as.numeric(sd_params[[key]])
          sdi_vals[as.integer(fvs_idx)] <- exp(fixed + re_val)
        }
      }

      # Clip to reasonable range
      sdi_vals <- pmax(pmin(sdi_vals, 2000), 100)
      lines <- c(lines, "!! SDI max (posterior draw)")
      for (i in seq_along(sdi_vals)) {
        lines <- c(lines, sprintf("SDIMAX          %10d%10.1f", i, sdi_vals[i]))
      }
    }
  }

  # ---- Growth multiplier from diameter_growth (Stan: mu_b0 + sigma_b0 * z_b0[i]) ----
  if ("diameter_growth" %in% names(draw)) {
    dg_params <- draw$diameter_growth
    mu_b0 <- as.numeric(dg_params$mu_b0)
    sigma_b0 <- as.numeric(dg_params$sigma_b0)

    if (!is.null(mu_b0) && !is.null(sigma_b0)) {
      # Reconstruct species B0 from non centered parameterization
      cal_b0 <- rep(mu_b0, maxsp)
      z_pattern <- "^z_b0\\[(\\d+)\\]$"
      z_keys <- grep(z_pattern, names(dg_params), value = TRUE)
      for (key in z_keys) {
        idx <- as.integer(sub(z_pattern, "\\1", key))
        if (idx <= maxsp) {
          cal_b0[idx] <- mu_b0 + sigma_b0 * as.numeric(dg_params[[key]])
        }
      }

      # Get default B0 or B1 for comparison
      def_growth <- def_cfg$categories$growth
      def_b0 <- NULL
      for (bname in c("B0", "B1", "WEIBB1")) {
        if (bname %in% names(def_growth)) {
          def_b0 <- unlist(def_growth[[bname]])
          break
        }
      }

      if (!is.null(def_b0)) {
        n <- min(length(cal_b0), length(def_b0))
        mult <- ifelse(
          def_b0[1:n] != 0,
          sqrt(pmax(pmin(exp(cal_b0[1:n] - def_b0[1:n]), 10.0), 0.1)),
          1.0
        )
        grow_kw <- character()
        for (i in seq_len(n)) {
          if (abs(mult[i] - 1.0) > 0.01) {
            grow_kw <- c(grow_kw, sprintf("GROWMULT        %10d%10.4f", i, mult[i]))
          }
        }
        if (length(grow_kw) > 0) {
          lines <- c(lines, "!! Growth multipliers (posterior draw)", grow_kw)
        }
      }
    }
  }

  # ---- Mortality multiplier from brms (b_Intercept + r_SPCD[code,Intercept]) ----
  if ("mortality" %in% names(draw)) {
    mort_params <- draw$mortality
    mort_fixed <- as.numeric(mort_params$b_Intercept)

    if (!is.null(mort_fixed)) {
      # Check if default config has mortality reference
      def_mort <- def_cfg$categories$mortality
      if (!is.null(def_mort) && is.list(def_mort) && length(def_mort) > 0) {
        def_mort_b0 <- NULL
        for (bname in c("MORT_B0", "B0", "MRT_B0")) {
          if (bname %in% names(def_mort)) {
            def_mort_b0 <- unlist(def_mort[[bname]])
            break
          }
        }

        if (!is.null(def_mort_b0)) {
          cal_b0 <- rep(mort_fixed, maxsp)
          r_pattern <- "^r_SPCD\\[(\\d+),Intercept\\]$"
          r_keys <- grep(r_pattern, names(mort_params), value = TRUE)
          for (key in r_keys) {
            fia_code <- sub(r_pattern, "\\1", key)
            fvs_idx <- fia_to_fvs[fia_code]
            if (!is.na(fvs_idx)) {
              re_val <- as.numeric(mort_params[[key]])
              cal_b0[as.integer(fvs_idx)] <- mort_fixed + re_val
            }
          }

          n <- min(length(cal_b0), length(def_mort_b0))
          odds <- ifelse(
            def_mort_b0[1:n] != 0,
            exp(cal_b0[1:n] - def_mort_b0[1:n]),
            1.0
          )
          odds <- pmax(pmin(odds, 10.0), 0.1)
          mort_kw <- character()
          for (i in seq_len(n)) {
            if (abs(odds[i] - 1.0) > 0.01) {
              mort_kw <- c(mort_kw, sprintf(
                "MORTMULT        %10d%10.4f       0.0     999.0", i, odds[i]
              ))
            }
          }
          if (length(mort_kw) > 0) {
            lines <- c(lines, "!! Mortality multipliers (posterior draw)", mort_kw)
          }
        }
      }
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

  # Load draws to determine available count
  draws_cfg <- load_fvs_config(variant, version = "draws", config_dir = config_dir)
  draws_data <- if ("draws" %in% names(draws_cfg)) draws_cfg$draws else draws_cfg
  n_available <- length(draws_data[[names(draws_data)[1]]])

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

  # Read template keyfile once
  template_lines <- readLines(keyfile_path)
  process_idx <- grep("^PROCESS", template_lines, ignore.case = TRUE)

  # Get base filename
  base_name <- tools::file_path_sans_ext(basename(keyfile_path))
  ext <- tools::file_ext(keyfile_path)

  results <- data.frame(
    draw_index = integer(),
    output_path = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(draw_indices)) {
    draw_idx <- draw_indices[i]

    # Generate keywords for this draw
    keywords <- generate_draw_keywords(variant, draw_index = draw_idx,
                                       config_dir = config_dir)

    # Insert keywords before PROCESS line
    if (length(process_idx) > 0) {
      pidx <- process_idx[1]
      new_lines <- c(
        template_lines[1:(pidx - 1)],
        keywords,
        "",
        template_lines[pidx:length(template_lines)]
      )
    } else {
      new_lines <- c(template_lines, "", keywords)
    }

    # Write output keyfile
    output_filename <- sprintf("%s_%03d.%s", base_name, i, ext)
    output_path <- file.path(output_dir, output_filename)
    writeLines(new_lines, output_path)

    results <- rbind(results, data.frame(
      draw_index = draw_idx,
      output_path = output_path,
      stringsAsFactors = FALSE
    ))

    if (i %% 10 == 0 || i == 1) {
      message(sprintf("Generated %d/%d ensemble keyfiles", i, n_draws))
    }
  }

  return(results)
}
