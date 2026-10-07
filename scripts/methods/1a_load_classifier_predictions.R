# 1a_load_classifier_predictions.R =============================================
# Load and collate all raw xlsx classifier prediction data into a single tibble
#
## CONFIG:
# NOTE: You must manually set your local Google Drive path below
in_path_gdrive = "/Users/gioj/Library/CloudStorage/GoogleDrive-giojacuzzi@gmail.com/.shortcut-targets-by-id/1ALEa2jYUKRvSzUO8e_mEXSZLJhUu_8St/TNC Biodiversity Monitoring - publication effort"
## OUTPUT:
out_cache_dir = "data/cache/1a_load_classifier_predictions"
# ==============================================================================

source("scripts/methods/0_config.R")

if (!file.exists(in_path_gdrive)) stop("Missing path ", in_path_gdrive, ", did you set `in_path_gdrive`?")

in_path_predictions = file.path(in_path_gdrive, "Data/Classifier predictions")
message("Finding all xlsx files under ", in_path_predictions)
files = list.files(in_path_predictions, pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)

message("Collating all predictions into a single tibble")
bar = progress_bar$new(format = "[:bar] :percent :elapsedfull (ETA :eta)", total = length(files), clear = FALSE)
predictions = lapply(files, function(f) {
  file_data = read_excel(f, progress = FALSE) %>% clean_names() %>% mutate(
    across(everything(), as.character),
    reserve = tolower(basename(dirname(f))),
    xlsx_file = basename(f)
  )
  bar$tick()
  return(file_data)
}) %>% bind_rows()

message("Standardizing data case and type")
predictions = predictions %>%
  mutate(
    across(c(common_name, scientific_name), tolower),
    across(c(common_name, scientific_name, reserve), factor),
    across(c(confidence_source, confidence_target), as.numeric)
  )

str(predictions)

if (!dir.exists(out_cache_dir)) dir.create(out_cache_dir, recursive = TRUE)
path_predictions_cache = file.path(out_cache_dir, "predictions.rds")
message("Caching results to ", path_predictions_cache)
saveRDS(predictions, path_predictions_cache)
message(crayon::green("Cached", path_predictions_cache))
