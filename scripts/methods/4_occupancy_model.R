# 4_occupancy_model.R ==========================================================
# Construct the species x site x date detection history array and parameterize the multi-species occupancy model
#
## INPUT:
path_detections_cache  = "data/cache/2a_assign_detections/detections.rds"
path_gis_data_cache    = "data/cache/3_load_gis_data/gis_data.rds"
## OUTPUT:
out_cache_dir = "data/cache/4_occupancy_model"
# ==============================================================================

source("scripts/methods/0_config.R")

message("Loading species detections from ", path_detections_cache)
detections = readRDS(path_detections_cache)

message("Filtering detections for only focal species")
detections = detections %>% filter(common_name %in% focal_species)

missing_datetimes = unique(detections %>% filter(is.na(datetime)) %>% pull(source_file))
if (length(missing_datetimes) > 0) {
  message(crayon::yellow("WARNING: Missing datetimes for invalid source_file:", missing_datetimes))
  message(crayon::yellow("These detections will be discarded"))
  detections = detections %>% filter(!is.na(datetime))
}

message("Constructing species x site x date detection history array")
detections = detections %>% mutate(date = as.Date(datetime, tz = tz))
species = sort(unique(detections$common_name))
sites = levels(detections$site)

# Find all surveys conducted (site x date combinations)
survey_effort = detections %>% distinct(site, date)
dates = seq(min(survey_effort$date), max(survey_effort$date), by = "day")
surveyed = matrix(FALSE, length(sites), length(dates))
surveyed[cbind(match(survey_effort$site, sites), match(survey_effort$date, dates))] = TRUE

# Find all surveys for each species with at least one detection (species x site x date combinations)
site_date_detections = detections %>% distinct(common_name, site, date)

# Assign default values to 0 where surveyed, NA where not
y = array(rep(ifelse(surveyed, 0, NA), each = length(species)),
          dim = c(length(species), length(sites), length(dates)),
          dimnames = list(species = species, site = sites, date = format(dates)))
# Overwrite values to 1 where detected
y[cbind(match(site_date_detections$common_name, species),
        match(site_date_detections$site, sites),
        match(site_date_detections$date, dates))] = 1

str(y)

# Example data access for a given species
example_species = "pacific wren"
message("Example data access for ", example_species)
y[example_species, , ]

{
  message(crayon::yellow("TODO: Load GIS data from", path_gis_data_cache))
}

{
  message(crayon::yellow("TODO: Parameterize the multi-species occupancy model"))
  occupancy_data = NA
}

# Cache results
if (!dir.exists(out_cache_dir)) dir.create(out_cache_dir, recursive = TRUE)
path_occupancy_cache = file.path(out_cache_dir, "occupancy_data.rds")
message("Caching results to ", path_occupancy_cache)
saveRDS(occupancy_data, path_occupancy_cache)
message(crayon::green("Cached", path_occupancy_cache))
