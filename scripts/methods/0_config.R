# 0_config.R ###################################################################################################
# Global configuration, packages, data, and helper functions sourced by the methods scripts 
##############################################################################################################

# Load required packages (automatically install any missing) -------------------
if (!exists("pkgs", envir = .GlobalEnv)) {
  message("Loading required packages (automatically installing any missing)")
  pkgs = c(
    # data manipulation
    "janitor",          # data cleaning and standardization
    "readxl",           # excel data
    "tidyverse",        # general purpose
    # utility
    "crayon",           # console warnings
    "progress"          # dynamic progress bar
  )
  print(sapply(pkgs, function(pkg) {
    if (!pkg %in% installed.packages()[, "Package"]) install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
    as.character(packageVersion(pkg)) # print package version
  }))
}

# Shared variables among scripts -----------------------------------------------

# Timezone
tz = "America/Los_Angeles"

# Species metadata -------------------------------------------------------------

# Load Avian Conservation Assessment Database (ACAD) Partners in Flight database
acad_regional_species = read.csv("data/ACAD Regional Species.csv") %>% clean_names() %>% mutate(
  across(c(common_name, scientific_name), tolower),
)

# Load species traits from Jacuzzi et al. (in prep)
traits = read.csv("data/Jacuzzi_OESF_traits.csv")

# Assemble focal species list
focal_species = acad_regional_species %>% filter(
  group == "landbird", # landbird
  x_pop_b >= 10        # breeding population percentage within the ecoregion
) %>% filter(
  common_name %in% (traits %>% filter(
    home_range_radius_m <= 500 # maximum estimated home range radius
) %>% pull(common_name))) %>%
  pull(common_name) %>% sort()

# Set ggplot theme -------------------------------------------------------------

# Adapted from James Robinson
theme_sleek = function(base_size = 11, base_family = "") {
  half_line = base_size/2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey30"),
      strip.text.y = element_text(colour = "grey30"),
      axis.text = element_text(colour = "grey30"),
      axis.title = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey30", size = rel(1)),
      plot.subtitle = element_text(colour = "grey30", size = rel(.85))
    )
}
theme_set(theme_sleek())
