# 2b_inspect_detections.R ======================================================
# Inspect all species detection data
#
## CONFIG:
class_predictions_to_inspect = "red-breasted sapsucker"
## INPUT:
path_detections_cache  = "data/cache/2a_assign_detections/detections.rds"
path_calibration_table = "data/Jacuzzi_OESF_calibration.csv"
# ==============================================================================

source("scripts/methods/0_config.R")

message("Loading detections from ", path_detections_cache)
detections = readRDS(path_detections_cache)

message("Loading classifier calibration table from ", path_calibration_table)
calibration = read.csv(path_calibration_table)

message("Filtering detections for only focal species")
detections = detections %>% filter(common_name %in% focal_species)

message(crayon::yellow("Predictions for the classes below require require manual validation:"))
detections %>%
  left_join(calibration %>% select(common_name, method), by = "common_name", relationship = "many-to-one") %>%
  filter(is.na(method) | method == "manual") %>%
  count(common_name, sort = TRUE) %>% print(n = Inf)

message(crayon::yellow("Reporting predictions for class:", class_predictions_to_inspect))
detections %>% filter(common_name == class_predictions_to_inspect) %>% print(n = Inf)

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
