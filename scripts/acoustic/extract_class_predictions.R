# extract_class_predictions.R ######################################################################
# Extract predictions above a confidence threshold for a specific class. These can be used with the
# audio data to derive species- and model-specific segments via the GUI from Jacuzzi and Olden 2025.
#
# CONFIG:
focal_classes = c("Olive-sided Flycatcher", "Hairy Woodpecker")
threshold = 0.99
#
# OUTPUT:
path_out = "data/debug/helper_get_class_predictions/" # TODO: Change filepath to point to an output directory you want to create
#
# INPUT:
path_prediction_data = "data/cache/1_pam/2_agg_raw_predictions/NEW_prediction_data.feather" # TODO: Change filepath to point to your prediction data
# path_prediction_data = "filename.xlsx"
####################################################################################################

library(dplyr)
library(readxl)
library(readr)
library(purrr)
library(crayon)

message("Loading all prediction data")
pred_data = read_feather(path_prediction_data) # TODO: Read all your prediction data as a tibble
# pred_data = read_excel(path_prediction_data)

for (focal_class in focal_classes) {
  
  # Filter for class predictions
  class_data = pred_data %>%
    filter(`Common name` == focal_class, Confidence_target > threshold) # TODO: Filter according to the model you want to use
  message("Found ", nrow(class_data), " predictions for class '", focal_class, "' using threshold ", threshold)
  
  # Store associated audio file path and name
  class_data$dir  = dirname(class_data$file_path)
  class_data$file = sub("\\.BirdNET\\.results\\.csv$", ".wav", basename(class_data$file_path))
  out_dir = paste0(path_out, focal_class)
  
  # Save .csv prediction files matching directory structure
  class_data %>%
    group_split(file_path) %>%
    walk(function(df) {
      
      rel_path = unique(df$file_path)
      
      # Build full output path under out_dir
      full_path = file.path(out_dir, rel_path)
      
      # Create nested directories
      dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
      
      # Write CSV
      write_csv(df, full_path)
    })
  if (nrow(class_data) > 0) message(crayon::green("Finished extracting predictions to", out_dir))
}
