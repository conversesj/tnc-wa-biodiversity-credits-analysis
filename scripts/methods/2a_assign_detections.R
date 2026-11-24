# 2a_assign_detections.R =======================================================
# Apply thresholds to prediction data and report species without thresholds (time and location of each prediction)
#
## INPUT:
path_predictions_cache = "data/cache/1a_load_classifier_predictions/predictions.rds"
path_calibration_table = "data/Jacuzzi_OESF_calibration.csv"
## OUTPUT:
out_cache_dir = "data/cache/2a_assign_detections"
# ==============================================================================

source("scripts/methods/0_config.R")

message("Loading predictions from ", path_predictions_cache)
predictions = readRDS(path_predictions_cache)

message("Loading classifier calibration table from ", path_calibration_table)
calibration = read.csv(path_calibration_table)

message("Retaining only predictions from the optimal submodel (i.e. default BirdNET for 'source' or custom OESF for 'target)")
predictions_optimized = predictions %>%
  left_join(calibration %>% distinct(common_name, model), by = "common_name", relationship = "many-to-one") %>%
  mutate(
    confidence = if_else(model %in% "target", confidence_target, confidence_source)
  ) %>%
  select(-confidence_source, -confidence_target) %>%
  filter(!is.na(confidence))

# Overwrite any validated predictions with observed truth
{
  in_path_validations = file.path(in_path_gdrive, "Data/Classifier validations")
  message("Finding all csv files under ", in_path_validations)
  files = list.files(in_path_validations, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  
  message("Collating all validations into a single tibble")
  bar = progress_bar$new(format = "[:bar] :percent :elapsedfull (ETA :eta)", total = length(files), clear = FALSE)
  validations = lapply(files, function(f) {
    file_data = read_csv(f, show_col_types = FALSE, progress = FALSE) %>% clean_names()
    bar$tick()
    return(file_data)
  }) %>% bind_rows()
  
  # Drop unknown (NA) validations
  validations = validations %>% mutate(truth = as.numeric(truth)) %>% filter(!is.na(truth))
  
  # Overwrite confidence value of present examples to 1.0 and absent to 0.0
  stopifnot(all(validations$truth %in% c(0, 1)))
  validations = validations %>% mutate(confidence = if_else(truth == 1, 1.0, confidence))
  validations = validations %>% mutate(confidence = if_else(truth == 0, 0.0, confidence))
  
  # Align columns and factor levels with predictions
  validations_clean = validations %>%
    select(-truth, -notes) %>%
    mutate(across(c(scientific_name, reserve, site, aru), ~ factor(.x, levels = levels(predictions_optimized[[cur_column()]])))) %>%
    select(-datetime)
  
  keys = c("source_file", "start_s", "common_name")
  stopifnot(
    # matching factors
    !anyNA(select(validations_clean, scientific_name, reserve, site, aru)),
    # one validation per detection
    !anyDuplicated(select(validations_clean, all_of(keys))),
    # each validation has only one matching prediction
    nrow(anti_join(validations_clean, predictions_optimized, by = keys)) == 0,
    nrow(semi_join(predictions_optimized, validations_clean, by = keys)) == nrow(validations_clean)
  )
  
  # Overwrite matching observations
  predictions_optimized_validated = predictions_optimized %>% rows_update(validations_clean %>% select(all_of(keys), confidence), by = keys)
  
  # Inspect which values are overwritten
  waldo::compare(predictions_optimized, predictions_optimized_validated)
  
  # Drop verified nondetections
  predictions_optimized_validated = predictions_optimized_validated %>% filter(confidence > 0.0)
  
  # Overwrite predictions_optimized with validated detections
  predictions_optimized = predictions_optimized_validated
}

message("Assign detections by retaining only predictions at or above species-specific thresholds (manual and uncalibrated species untouched)")
detections = predictions_optimized %>%
  left_join(calibration %>% select(common_name, threshold, method), by = "common_name", relationship = "many-to-one") %>%
  filter(
    is.na(threshold) |          # no calibration available (retain)
      method %in% "manual" |    # manual review required  (retain)
      confidence >= threshold   # otherwise, retain only if at/above threshold
  ) %>%
  select(-threshold, -method)

str(detections)

if (!dir.exists(out_cache_dir)) dir.create(out_cache_dir, recursive = TRUE)
path_detections_cache = file.path(out_cache_dir, "detections.rds")
message("Caching results to ", path_detections_cache)
saveRDS(detections, path_detections_cache)
message(crayon::green("Cached", path_detections_cache))
