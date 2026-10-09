# 3_load_gis_data.R ============================================================
# Load all gis covariate data as a single tibble
#
## CONFIG:

## INPUT:

## OUTPUT:
out_cache_dir = "data/cache/3_load_gis_data.R"
# ==============================================================================

source("scripts/methods/0_config.R")

{
  message(crayon::yellow("TODO: Load all gis covariate data as a single tibble with individual site ids matching those in 'predictions'
"))
  gis_data = NA
}

# Cache results
if (!dir.exists(out_cache_dir)) dir.create(out_cache_dir, recursive = TRUE)
path_gis_cache = file.path(out_cache_dir, "gis_data.rds")
message("Caching results to ", path_gis_cache)
saveRDS(gis_data, path_gis_cache)
message(crayon::green("Cached", path_gis_cache))
