# 1b_inspect_predictions.R ====================================================
# Inspect all classifier prediction data
#
## INPUT:
path_predictions_cache = "data/cache/1a_load_classifier_predictions/predictions.rds"
# ==============================================================================

source("scripts/methods/0_config.R")

message("Loading predictions from ", path_predictions_cache)
predictions = readRDS(path_predictions_cache)

# e.g. total number of predictions per class:
table(predictions$common_name)

p = predictions %>% count(common_name) %>%
  filter(!is.na(common_name), !grepl("Abiotic", common_name)) %>%
  ggplot(aes(x = n, y = reorder(common_name, n))) +
  geom_col() + labs(x = "Predictions", y = "Common name") +
  scale_x_log10(labels = scales::label_comma(), expand = c(0, 0)); print(p)

# Total number of predictions per class with confidence score (default BirdNET model) above 0.9:
table(predictions %>% filter(confidence_source > 0.9) %>% pull(common_name))

# Total number of predictions per class with confidence score (OESF model) above 0.9:
table(predictions %>% filter(confidence_target > 0.9) %>% pull(common_name))
