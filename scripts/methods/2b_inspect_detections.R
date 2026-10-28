# 2b_inspect_detections.R ======================================================
# Inspect all species detection data
#
## CONFIG:
class_predictions_to_inspect = c("hermit warbler")
subsample_validations = TRUE # subsample detections for validation of probabilistic threshold
set.seed(1)
## INPUT:
path_detections_cache   = "data/cache/2a_assign_detections/detections.rds"
path_calibration_table  = "data/Jacuzzi_OESF_calibration.csv"
## OUTPUT:
dir_validations_needed = "data/cache/2b_inspect_detections"
# ==============================================================================

source("scripts/methods/0_config.R")

message("Loading species detections from ", path_detections_cache)
detections = readRDS(path_detections_cache)

missing_datetimes = unique(detections %>% filter(is.na(datetime)) %>% pull(source_file))
if (length(missing_datetimes) > 0) {
  message(crayon::yellow("WARNING: Missing datetimes for invalid source_file:", missing_datetimes))
  message(crayon::yellow("These detections will be discarded"))
  detections = detections %>% filter(!is.na(datetime))
}

message("Loading classifier calibration table from ", path_calibration_table)
calibration = read.csv(path_calibration_table)

# message("Filtering detections for only focal species")
# detections = detections %>% filter(common_name %in% focal_species)

message("Number of focal species detected in each reserve (gamma diversity):")
reserve_naive_richness_gamma = detections %>% group_by(reserve) %>%
  summarise(
    n_species = n_distinct(common_name),
    n_sites   = n_distinct(site)
  ) %>% arrange(desc(n_species)) %>% print(n = Inf)
p = ggplot(reserve_naive_richness_gamma, aes(x = n_species, y = reserve, fill = reserve)) +
  geom_col() + labs(x = "Naive focal species richness (gamma diversity)", y = "Reserve", fill = "Reserve"); print(p)

message("Number of focal species detected at each site (alpha diversity):")
site_naive_richness_alpha = detections %>% group_by(reserve, site) %>%
  summarise(n_species = n_distinct(common_name)) %>%
  arrange(reserve, desc(n_species)) %>% print(n = Inf)
p = ggplot(site_naive_richness_alpha, aes(x = n_species, y = reorder(site, n_species), fill = reserve)) +
  geom_col() + labs(x = "Naive focal species richness (alpha diversity)", y = "Site", fill = "Reserve") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)); print(p)

message("Number of sites at which each focal species was detected:")
species_sites = detections %>% group_by(common_name) %>%
  summarise(
    common_name = first(common_name),
    n_sites     = n_distinct(site)
  ) %>% arrange(desc(n_sites)) %>% print(n = Inf)

message("Number of sites at which each focal species was detected, by reserve:")
species_sites_reserve = detections %>% group_by(common_name, reserve) %>%
  summarise(n_sites = n_distinct(site), .groups = "drop") %>%
  arrange(common_name, reserve) %>% print(n = Inf)
p = ggplot(species_sites_reserve, aes(x = n_sites, y = reorder(common_name, n_sites, FUN = sum), fill = reserve)) +
  geom_vline(xintercept = sum(reserve_naive_richness_gamma$n_sites), linetype = "dashed", color = "red") +
  geom_col() + labs(x = "Number of sites", y = "Species", fill = "Reserve"); print(p)

message("Number of unique site-date combinations at which each focal species was detected:")
species_sitedates = detections %>%
  mutate(date = as_date(datetime)) %>%
  group_by(common_name) %>%
  summarise(
    n_sites     = n_distinct(site),
    n_sitedates = n_distinct(site, date)
  ) %>% arrange(desc(n_sitedates)) %>% print(n = Inf)

# Data quality check -- are any sites outliers?
sort(table(detections$site))
boxplot(table(detections$site))
p = detections %>% count(site) %>%
  ggplot(aes(x = n, y = reorder(site, n))) +
  geom_col() + labs(x = "Predictions", y = "Site") +
  scale_x_continuous(labels = scales::label_comma(), expand = c(0, 0)); print(p)

message(crayon::yellow("Predictions for the classes below require require manual validation:"))
detections = detections %>% arrange(common_name, source_file, start_s)
classes_requiring_validation = detections %>%
  left_join(calibration %>% select(common_name, method), by = "common_name", relationship = "many-to-one") %>%
  filter(is.na(method) | method == "manual") %>%
  count(common_name, sort = TRUE) %>% print(n = Inf)
classes_requiring_validation = classes_requiring_validation %>% pull(common_name)

for (class_i in class_predictions_to_inspect) {
  message("Reporting predictions for class: ", class_i)
  validations_needed = detections %>% filter(common_name == class_i) %>% print()
  
  if (subsample_validations) {
    breaks = seq(0.4, 1, by = 0.05)
    
    validation_sample = validations_needed %>%
      mutate(conf_bin = cut(confidence, breaks = breaks, right = FALSE, include.lowest = TRUE)) %>%
      group_by(conf_bin) %>% slice_sample(n = 10) %>% ungroup()
    
    count(validation_sample, conf_bin)
    validations_needed = validation_sample %>% select(-conf_bin) %>% arrange(desc(confidence))
  }
  
  if (nrow(validations_needed) > 0) {
    validations_needed = validations_needed %>% mutate(truth = '', notes = '')
    path_validations_needed = paste0(dir_validations_needed, "/", class_i, "_validations.csv")
    if (!dir.exists(dirname(path_validations_needed))) dir.create(dirname(path_validations_needed), recursive = TRUE)
    message("Saving predictions needing validation to: ", path_validations_needed)
    write_csv(validations_needed, path_validations_needed)
  }
}
