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

message("Dropping classes not of interest (e.g. 'abiotic rain')")
predictions = predictions %>% filter(!common_name %in% c(
  "abiotic rain", "abiotic aircraft", "abiotic wind", "abiotic logging", "abiotic vehicle", "biotic insect", "biotic anuran", NA
))

message("Loading classifier calibration table from ", path_calibration_table)
calibration = read_csv(path_calibration_table)

message("Retaining only predictions from the optimal submodel (i.e. default BirdNET for 'source' or custom OESF for 'target)")
predictions_optimized = predictions %>%
  left_join(calibration %>% distinct(common_name, model), by = "common_name", relationship = "many-to-one") %>%
  mutate(
    confidence = if_else(model %in% "target", confidence_target, confidence_source)
  ) %>%
  select(-confidence_source, -confidence_target) %>%
  filter(!is.na(confidence))

message(crayon::yellow("Predictions for the classes below require require manual validation:"))
predictions_optimized %>%
  left_join(calibration %>% select(common_name, method), by = "common_name", relationship = "many-to-one") %>%
  filter(is.na(method) | method == "manual") %>%
  count(common_name, sort = TRUE) %>% print(n = Inf)

class_to_inspect = "mourning dove"
message(crayon::yellow("Reporting predictions for specific class '", class_to_inspect, "'"))
predictions_optimized %>% filter(common_name == class_to_inspect) %>% print(n = Inf)

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
