# 0_config.R ###################################################################################################
# Global configuration, packages, data, and helper functions sourced by the methods scripts 
##############################################################################################################

# Load required packages (automatically install any missing) -------------------------------------------------
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
