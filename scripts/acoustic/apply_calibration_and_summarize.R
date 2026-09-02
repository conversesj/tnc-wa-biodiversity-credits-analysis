# apply_calibration_and_summarize.R ###############################################################
# Apply species-specific calibration thresholds to acoustic detections for Clearwater, Ellsworth,
# and Hoh. For each species, use the model specified in the calibration table:
#   "source" -> Confidence_source
#   "target" -> Confidence_target
# Missing scores for the specified model are treated as 0.
#
# Detections are separated into:
#   1) detections passing the calibrated threshold
#   2) detections requiring manual review (Inf/manual)
#   3) avian detections not represented in the calibration table
#
# A plot-by-species summary is generated from threshold-passing detections.
#
# CONFIG:
preserves <- c(
  Clearwater = "E:/TNC Plot Data/Clearwater",
  Ellsworth  = "E:/TNC Plot Data/Ellsworth",
  Hoh        = "E:/TNC Plot Data/Hoh"
)
#
# INPUT:
cal_file <- "E:/TNC Plot Data/Jacuzzi_OESF_calibration.csv"
#
# OUTPUT:
# Calibration_output/01_threshold_pass
# Calibration_output/02_INF_manual
# Calibration_output/03_not_in_calibration
# E:/TNC Plot Data/PASS_species_by_plot.xlsx
###################################################################################################

library(readr)
library(readxl)
library(writexl)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)


# Read species-specific calibration table
cal <- read_csv(cal_file, show_col_types = FALSE)

# Standardize fields used for matching
cal$sci_key <- str_to_lower(str_trim(cal$scientific_name))
cal$com_key <- str_to_lower(str_trim(cal$common_name))
cal$model <- str_to_lower(str_trim(cal$model))
cal$threshold <- as.numeric(cal$threshold)


# Define obvious abiotic / non-avian labels to exclude
nonavian_pattern <- regex(
  "^(abiotic|insect|anuran)(_|\\b)",
  ignore_case = TRUE
)


# Process each preserve independently
for (preserve in names(preserves)) {
  
  in_dir <- preserves[[preserve]]
  
  # Create output directories
  out_root <- file.path(in_dir, "Calibration_output")
  out_pass <- file.path(out_root, "01_threshold_pass")
  out_manual <- file.path(out_root, "02_INF_manual")
  out_new <- file.path(out_root, "03_not_in_calibration")
  
  dir.create(out_pass, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_manual, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_new, recursive = TRUE, showWarnings = FALSE)
  
  # Find prediction files
  files <- list.files(
    in_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  for (f in files) {
    
    x <- read_excel(f)
    
    # Standardize known alternate column names
    if ("Scientific.name" %in% names(x)) {
      names(x)[names(x) == "Scientific.name"] <- "Scientific name"
    }
    
    if ("Common.name" %in% names(x)) {
      names(x)[names(x) == "Common.name"] <- "Common name"
    }
    
    # Match detections to calibration table using scientific name first
    sci_key <- str_to_lower(str_trim(as.character(x$`Scientific name`)))
    com_key <- str_to_lower(str_trim(as.character(x$`Common name`)))
    
    idx_sci <- match(sci_key, cal$sci_key)
    idx_com <- match(com_key, cal$com_key)
    
    idx <- ifelse(!is.na(idx_sci), idx_sci, idx_com)
    
    # Attach calibration information
    x$cal_model <- cal$model[idx]
    x$cal_threshold <- cal$threshold[idx]
    x$cal_method <- cal$method[idx]
    
    matched <- !is.na(idx)
    
    # Select confidence score from the model specified in the calibration table
    source_score <- suppressWarnings(as.numeric(x$Confidence_source))
    target_score <- suppressWarnings(as.numeric(x$Confidence_target))
    
    # Treat missing scores as 0
    source_score[is.na(source_score)] <- 0
    target_score[is.na(target_score)] <- 0
    
    x$cal_score_used <- ifelse(
      x$cal_model == "source",
      source_score,
      ifelse(
        x$cal_model == "target",
        target_score,
        NA_real_
      )
    )
    
    # Identify obvious non-avian detections
    sci_text <- ifelse(
      is.na(x$`Scientific name`),
      "",
      as.character(x$`Scientific name`)
    )
    
    com_text <- ifelse(
      is.na(x$`Common name`),
      "",
      as.character(x$`Common name`)
    )
    
    lab_text <- ifelse(
      is.na(x$Label),
      "",
      as.character(x$Label)
    )
    
    nonavian <- str_detect(sci_text, nonavian_pattern) |
      str_detect(com_text, nonavian_pattern) |
      str_detect(lab_text, nonavian_pattern)
    
    # Identify species requiring manual review
    manual <- matched &
      !nonavian &
      (
        is.infinite(x$cal_threshold) |
          x$cal_method == "manual" |
          x$cal_model == "manual"
      )
    
    # Retain detections meeting species-specific calibrated thresholds
    pass <- matched &
      !nonavian &
      !manual &
      is.finite(x$cal_threshold) &
      x$cal_score_used >= x$cal_threshold
    
    # Identify avian species not represented in the calibration table
    not_in_cal <- !matched &
      !nonavian &
      (sci_key != "" | com_key != "")
    
    # Write one output file for each input file
    filename <- basename(f)
    
    write_xlsx(
      x[pass, ],
      file.path(out_pass, filename)
    )
    
    write_xlsx(
      x[manual, ],
      file.path(out_manual, filename)
    )
    
    write_xlsx(
      x[not_in_cal, ],
      file.path(out_new, filename)
    )
    
    # Summary
    below_threshold <- matched &
      !nonavian &
      !manual &
      is.finite(x$cal_threshold) &
      x$cal_score_used < x$cal_threshold
    
    cat(
      filename,
      "| PASS:", sum(pass),
      "| INF/manual:", sum(manual),
      "| Not calibration:", sum(not_in_cal),
      "| Below threshold:", sum(below_threshold),
      "| Non-avian excluded:", sum(nonavian),
      "\n"
    )
  }
}

# Summarize threshold-passing detections by plot and species
pass_folders <- c(
  Clearwater = "E:/TNC Plot Data/Clearwater/Calibration_output/01_threshold_pass",
  Ellsworth = "E:/TNC Plot Data/Ellsworth/Calibration_output/01_threshold_pass",
  Hoh = "E:/TNC Plot Data/Hoh/Calibration_output/01_threshold_pass"
)


make_species_matrix <- function(folder) {
  
  # Find threshold-passing files
  files <- list.files(
    folder,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Retain all plots, including plots with zero passing detections
  all_plots <- tibble(
    Plot = basename(files) %>%
      str_remove("_?merged_results_with_filename\\.xlsx$")
  )
  
  # Count detections for each species within each plot
  species_counts <- map_dfr(files, function(f) {
    x <- read_excel(f)
    plot_id <- basename(f) %>%
      str_remove("_?merged_results_with_filename\\.xlsx$")
    
    if (nrow(x) == 0) {
      return(
        tibble(
          Plot = character(),
          `Common name` = character()
        )
      )
    }
    
    # Allow known alternate common-name column formats
    if ("Common name" %in% names(x)) {
      common_name <- x$`Common name`
    } else if ("Common.name" %in% names(x)) {
      common_name <- x$Common.name
    } else {
      return(
        tibble(
          Plot = character(),
          `Common name` = character()
        )
      )
    }
    
    tibble(
      Plot = plot_id,
      `Common name` = as.character(common_name)
    )
  }) %>%
    filter(
      !is.na(`Common name`),
      str_trim(`Common name`) != ""
    ) %>%
    count(
      Plot,
      `Common name`,
      name = "Detection_count"
    )
  
  # Reshape to plot-by-species matrix
  species_matrix <- species_counts %>%
    pivot_wider(
      names_from = `Common name`,
      values_from = Detection_count,
      values_fill = 0
    )
  
  all_plots %>%
    left_join(species_matrix, by = "Plot") %>%
    mutate(
      across(-Plot, ~ replace_na(.x, 0))
    ) %>%
    arrange(Plot)
}


# Generate one worksheet per preserve
result <- list(
  Clearwater = make_species_matrix(pass_folders["Clearwater"]),
  Ellsworth = make_species_matrix(pass_folders["Ellsworth"]),
  Hoh = make_species_matrix(pass_folders["Hoh"])
)


# Save 
write_xlsx(
  result,
  "E:/TNC Plot Data/PASS_species_by_plot.xlsx"
)
