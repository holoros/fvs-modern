# ==============================================================================
# fvsOL - Calibrated Parameter Support Patch
# ==============================================================================
#
# This patch adds Bayesian calibrated parameter support to FVS-Online.
#
# INTEGRATION INSTRUCTIONS:
# ========================
# 1. Source this file in the main FVS-Online server.R after loading fvsol_config.R:
#
#    source("fvs_calibration_patch.R")
#
# 2. Add the UI component to the FVS-Online UI definition:
#
#    In FVSOnlineUI(), add this where variant/parameter selection UI lives:
#
#    fvsCalibrationUI("calibration_panel")
#
# 3. Add the server component to the FVS-Online server function:
#
#    In FVSOnlineServer(), add this in the appropriate observer:
#
#    calibrated_keyfile <- fvsCalibrationServer("calibration_panel",
#                                               reactive(input$variant_choice),
#                                               reactive(input$keyfile_path))
#
# 4. Before writing the keyfile for FVS execution, inject calibrated keywords:
#
#    final_keyfile <- calibrated_keyfile()
#    if (!is.null(final_keyfile)) {
#      # Use final_keyfile instead of user's original keyfile
#    }
#
# DEPENDENCIES:
# ==============
# - Requires jsonlite package for reading calibrated parameter JSON files
# - Requires python3 executable if using config/export_keywords.py export method
# - Calibrated parameters expected at: config/calibrated_params/{variant}.json
#
# ==============================================================================

# ============================================================================
# UI Component: fvsCalibrationUI
# ============================================================================
#' Create Calibrated Parameter Selection UI
#'
#' This function creates a UI panel for selecting between default FVS parameters
#' and Bayesian calibrated parameters, with options for uncertainty quantification.
#'
#' @param id The namespace ID for this module
#'
#' @return A tagList containing the UI elements for calibration parameter selection
#'
#' @export
fvsCalibrationUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Parameter Set Selection"),
    shiny::selectInput(
      ns("param_set"),
      label = "Parameter Set",
      choices = c(
        "Default (original FVS)" = "default",
        "Bayesian Calibrated" = "bayesian"
      ),
      selected = "default"
    ),

    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'bayesian'", ns("param_set")),

      shiny::checkboxInput(
        ns("include_uncertainty"),
        label = "Include uncertainty estimates",
        value = FALSE
      ),

      shiny::conditionalPanel(
        condition = sprintf("input['%s']", ns("include_uncertainty")),

        shiny::numericInput(
          ns("num_draws"),
          label = "Number of posterior draws",
          value = 50,
          min = 10,
          max = 500,
          step = 10
        )
      ),

      shiny::numericInput(
        ns("random_seed"),
        label = "Random seed",
        value = 42,
        min = 0,
        max = .Machine$integer.max,
        step = 1
      )
    )
  )
}


# ============================================================================
# Server Component: fvsCalibrationServer
# ============================================================================
#' Apply Calibrated Parameters to FVS Keyfile
#'
#' This function modifies an FVS keyfile to incorporate Bayesian calibrated
#' parameters when requested. It handles reading calibrated parameter files,
#' generating FVS keywords, and optionally creating multiple keyfiles for
#' posterior uncertainty sampling.
#'
#' @param id The namespace ID (must match the UI)
#' @param variant_reactive A reactive expression that returns the selected FVS variant
#' @param keyfile_reactive A reactive expression that returns the path to the original keyfile
#' @param calibration_config Optional list with:
#'   - config_dir: directory containing calibrated parameter JSON files
#'   - export_script: path to config/export_keywords.py script
#'   Default config_dir is "config/calibrated_params"
#'
#' @return A reactive expression that returns:
#'   - NULL if using default parameters
#'   - List with $keyfile (path to calibrated keyfile) and $draws (number of draws)
#'     if using calibrated parameters
#'
#' @details
#' The function looks for calibrated parameters in:
#'   config/calibrated_params/{variant}.json
#'
#' Expected JSON structure:
#' {
#'   "keywords": ["KEYWORD1 params", "KEYWORD2 params"],
#'   "posterior_samples": [[draw1], [draw2], ...],
#'   "parameter_names": ["param1", "param2", ...]
#' }
#'
#' @export
fvsCalibrationServer <- function(id, variant_reactive, keyfile_reactive,
                                 calibration_config = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # Set defaults for calibration config
    if (is.null(calibration_config)) {
      calibration_config <- list(
        config_dir = "config/calibrated_params",
        export_script = "config/export_keywords.py"
      )
    }

    # Reactive expression that applies calibration and returns modified keyfile path
    calibrated_keyfile <- shiny::reactive({

      # If using default parameters, return NULL
      if (input$param_set == "default") {
        return(NULL)
      }

      # Get the original keyfile content
      keyfile_path <- keyfile_reactive()
      if (is.null(keyfile_path) || !file.exists(keyfile_path)) {
        warning("Original keyfile not found: ", keyfile_path)
        return(NULL)
      }

      keyfile_content <- readLines(keyfile_path)
      variant <- variant_reactive()

      # Read or generate calibrated keywords
      keywords_to_inject <- .fvs_get_calibrated_keywords(
        variant = variant,
        config_dir = calibration_config$config_dir,
        export_script = calibration_config$export_script
      )

      if (is.null(keywords_to_inject)) {
        warning("Failed to load calibrated parameters for variant: ", variant)
        return(NULL)
      }

      # Find the PROCESS line in the keyfile
      process_line <- which(grepl("^PROCESS", keyfile_content, ignore.case = TRUE))

      if (length(process_line) == 0) {
        warning("PROCESS keyword not found in keyfile")
        return(NULL)
      }

      # Inject calibration keywords before PROCESS
      insert_pos <- process_line[1]
      new_content <- c(
        keyfile_content[1:(insert_pos - 1)],
        keywords_to_inject,
        keyfile_content[insert_pos:length(keyfile_content)]
      )

      # Write the modified keyfile
      calibrated_keyfile_path <- .fvs_create_temp_keyfile(
        content = new_content,
        variant = variant,
        draw_num = NULL
      )

      cat("[fvsCalibration] Created calibrated keyfile: ", calibrated_keyfile_path, "\n")

      # Return info for uncertainty quantification
      list(
        keyfile = calibrated_keyfile_path,
        draws = if (input$include_uncertainty) input$num_draws else 1,
        seed = input$random_seed,
        variant = variant
      )
    })

    return(calibrated_keyfile)
  })
}


# ============================================================================
# Helper Functions
# ============================================================================

#' Get Calibrated Keywords for a Variant
#'
#' Attempts to load calibrated keywords from JSON, falling back to Python export.
#'
#' @keywords internal
.fvs_get_calibrated_keywords <- function(variant, config_dir, export_script) {

  # Try to read pre-built calibrated parameters JSON
  param_file <- file.path(config_dir, paste0(variant, ".json"))

  if (file.exists(param_file)) {
    tryCatch({
      require(jsonlite, quietly = TRUE)
      params <- jsonlite::fromJSON(param_file)

      if (!is.null(params$keywords) && length(params$keywords) > 0) {
        # Return keywords as character vector
        cat("[fvsCalibration] Loaded calibrated parameters from: ", param_file, "\n")
        return(params$keywords)
      }
    }, error = function(e) {
      cat("[fvsCalibration] Error reading JSON file: ", param_file, "\n")
      cat("  Error: ", conditionMessage(e), "\n")
    })
  }

  # Fallback: try to generate keywords using Python script
  if (file.exists(export_script)) {
    tryCatch({
      output_file <- tempfile(fileext = ".txt")
      cmd <- sprintf("python3 %s --variant %s --output %s",
                     export_script, variant, output_file)

      result <- system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)

      if (result == 0 && file.exists(output_file)) {
        keywords <- readLines(output_file)
        cat("[fvsCalibration] Generated keywords via Python script\n")
        unlink(output_file)
        return(keywords)
      } else {
        cat("[fvsCalibration] Python script failed with code: ", result, "\n")
      }
    }, error = function(e) {
      cat("[fvsCalibration] Error running Python script: ", conditionMessage(e), "\n")
    })
  }

  return(NULL)
}


#' Create a Temporary Keyfile with Calibrated Parameters
#'
#' Writes the modified keyfile content to a temporary location.
#'
#' @keywords internal
.fvs_create_temp_keyfile <- function(content, variant, draw_num = NULL) {

  # Create filename for temporary keyfile
  if (is.null(draw_num)) {
    filename <- sprintf("keyfile_%s_calibrated.key", variant)
  } else {
    filename <- sprintf("keyfile_%s_calibrated_draw%03d.key", variant, draw_num)
  }

  # Use session temp directory if available
  temp_path <- file.path(tempdir(), filename)

  writeLines(content, con = temp_path)

  return(temp_path)
}


#' Create Multiple Keyfiles for Posterior Uncertainty Sampling
#'
#' Generates multiple keyfiles, each with sampled posterior parameters,
#' for running FVS across the posterior distribution.
#'
#' @param variant The FVS variant
#' @param num_draws Number of posterior draws to generate
#' @param random_seed Random seed for reproducibility
#' @param original_keyfile Path to the original keyfile
#' @param config_dir Directory containing calibrated parameter files
#'
#' @return A list of paths to the generated keyfiles
#'
#' @keywords internal
.fvs_create_posterior_keyfiles <- function(variant, num_draws, random_seed,
                                          original_keyfile, config_dir) {

  if (!file.exists(original_keyfile)) {
    stop("Original keyfile not found: ", original_keyfile)
  }

  set.seed(random_seed)
  keyfile_content <- readLines(original_keyfile)
  param_file <- file.path(config_dir, paste0(variant, ".json"))

  if (!file.exists(param_file)) {
    warning("Calibrated parameters not found: ", param_file)
    return(NULL)
  }

  tryCatch({
    require(jsonlite, quietly = TRUE)
    params <- jsonlite::fromJSON(param_file)

    if (is.null(params$posterior_samples) || nrow(params$posterior_samples) == 0) {
      warning("No posterior samples found in: ", param_file)
      return(NULL)
    }

    # Sample posterior draws
    num_available <- nrow(params$posterior_samples)
    num_to_draw <- min(num_draws, num_available)

    sampled_indices <- sample(1:num_available, size = num_to_draw, replace = TRUE)

    # Create keyfiles for each draw
    keyfiles <- character(num_to_draw)

    for (i in 1:num_to_draw) {
      draw_idx <- sampled_indices[i]
      draw_params <- params$posterior_samples[draw_idx, ]

      # Generate keywords for this draw
      draw_keywords <- .fvs_generate_draw_keywords(
        param_names = params$parameter_names,
        param_values = draw_params
      )

      # Find PROCESS line
      process_line <- which(grepl("^PROCESS", keyfile_content, ignore.case = TRUE))
      if (length(process_line) > 0) {
        insert_pos <- process_line[1]
        new_content <- c(
          keyfile_content[1:(insert_pos - 1)],
          draw_keywords,
          keyfile_content[insert_pos:length(keyfile_content)]
        )
      } else {
        new_content <- c(draw_keywords, keyfile_content)
      }

      keyfiles[i] <- .fvs_create_temp_keyfile(
        content = new_content,
        variant = variant,
        draw_num = i
      )
    }

    cat("[fvsCalibration] Created ", num_to_draw, " posterior keyfiles\n")
    return(keyfiles)

  }, error = function(e) {
    cat("[fvsCalibration] Error creating posterior keyfiles: ",
        conditionMessage(e), "\n")
    return(NULL)
  })
}


#' Generate FVS Keywords from Posterior Draw Parameters
#'
#' Converts sampled posterior parameters into FVS keyword format.
#'
#' @keywords internal
.fvs_generate_draw_keywords <- function(param_names, param_values) {

  # This function should be customized based on your FVS variant's
  # keyword structure. This is a placeholder template.
  #
  # Example: if parameters correspond to growth model coefficients,
  # you might generate ECHOFILE or PARMFILE keywords here.

  keywords <- character(0)

  # Add parameters as comments (customize based on variant)
  keywords <- c(
    keywords,
    "* ================================================================",
    "* Bayesian Calibrated Parameters",
    paste0("* Draw parameters: ", paste(param_values, collapse = ", "))
  )

  # TODO: Add actual FVS keywords based on param_names and param_values
  # For example:
  # if ("growth_intercept" %in% param_names) {
  #   keywords <- c(keywords, "ECHOFILE")
  #   keywords <- c(keywords, paste("  ", param_names, param_values))
  # }

  return(keywords)
}

# ==============================================================================
# End of fvs_calibration_patch.R
# ==============================================================================
