library(here)
library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
library(purrr)

#paths to prediction data
path.H <- "E:/TNC/TNC Plot Data/Hoh"
path.E <- "E:/TNC/TNC Plot Data/Ellsworth"
path.C <- "E:/TNC/TNC Plot Data/Clearwater"

#get all prediction files
files.H <- list.files(path.H, pattern = "\\.xlsx$", full.names = TRUE)
files.E <- list.files(path.E, pattern = "\\.xlsx$", full.names = TRUE)
files.C <- list.files(path.C, pattern = "\\.xlsx$", full.names = TRUE)

#remove temporary Excel files
files.H <- files.H[!str_detect(basename(files.H), "^~\\$")]
files.E <- files.E[!str_detect(basename(files.E), "^~\\$")]
files.C <- files.C[!str_detect(basename(files.C), "^~\\$")]

files <- c(files.H, files.E, files.C)

#check number of files
length(files.H)
length(files.E)
length(files.C)
length(files)


#read one prediction file
read.one.file <- function(file){
  
  input <- read_excel(file)
  
  #standardize filename column
  file.col <- names(input)[tolower(names(input)) %in% c("source_file", "filename")]
  
  if(length(file.col) != 1){
    stop(paste("Check filename column in", basename(file)))
  }
  
  names(input)[names(input) == file.col] <- "source_file"
  
  #keep only what you need
  input <- input %>%
    select(source_file,
           `Common name`,
           Confidence,
           Confidence_source,
           Confidence_target)
  
  colnames(input) <- c("source_file",
                       "common_name",
                       "confidence",
                       "confidence_source",
                       "confidence_target")
  
  #make confidence columns numeric
  input <- input %>%
    mutate(across(c(confidence,
                    confidence_source,
                    confidence_target),
                  ~suppressWarnings(as.numeric(.))))
  
  #add site
  input$site <- str_remove(basename(file), "_.*$")
  
  #make dates in the file
  input$date <- ymd(str_extract(input$source_file, "\\d{8}"))
  
  #keep detection files with valid dates
  input <- input %>%
    filter(!is.na(date))
  
  return(input)
}


#read all prediction data
all.detections <- map_dfr(files, read.one.file)

#review
dim(all.detections)
head(all.detections)


#get rid of non-birds
non.birds <- c("Abiotic Aircraft", "Abiotic Logging", "Abiotic Rain",
               "Abiotic Vehicle", "Abiotic Wind", "Biotic Anuran",
               "Biotic Insect")

all.detections <- all.detections %>%
  filter(!common_name %in% non.birds)


#read calibration data
calibration.file <- list.files("E:/TNC/TNC Plot Data",
                               pattern = "^Jacuzzi_OESF_calibration\\.csv$",
                               full.names = TRUE)

if(length(calibration.file) != 1){
  stop("Check calibration file")
}

calibration <- read.csv(calibration.file,
                        stringsAsFactors = FALSE)

#keep only what you need
calibration <- calibration %>%
  select(common_name, model, threshold) %>%
  mutate(threshold = as.numeric(threshold))

#make species names consistent
all.detections$common_name_join <- tolower(all.detections$common_name)
calibration$common_name_join <- tolower(calibration$common_name)


#check species without calibration information
missing.spp <- sort(setdiff(unique(all.detections$common_name_join),
                            unique(calibration$common_name_join)))

missing.spp


#add calibration information
all.detections <- all.detections %>%
  left_join(calibration %>%
              select(common_name_join, model, threshold),
            by = "common_name_join")

#treat species without calibration information as Inf species
all.detections <- all.detections %>%
  mutate(threshold = ifelse(is.na(threshold),
                            Inf,
                            threshold))


#get confidence score for the appropriate model
all.detections <- all.detections %>%
  mutate(confidence_model = case_when(
    model == "source" ~ confidence_source,
    model == "target" ~ confidence_target
  ))


#keep calibrated detections above threshold and all Inf species
detections.final <- all.detections %>%
  filter(is.infinite(threshold) |
           (!is.na(confidence_model) &
              confidence_model >= threshold))


#make final confidence
detections.final <- detections.final %>%
  mutate(confidence_final = ifelse(is.infinite(threshold),
                                   confidence_source,
                                   1))

#remove Inf detections without a raw score
detections.final <- detections.final %>%
  filter(!is.na(confidence_final))


#review final confidence
table(is.infinite(detections.final$threshold))
range(detections.final$confidence_final)

#check Inf species
sort(unique(detections.final$common_name[
  is.infinite(detections.final$threshold)
]))


#collapse to one record for species by site by day
daily <- detections.final %>%
  group_by(site, common_name, date) %>%
  summarize(confidence_final = max(confidence_final),
            .groups = "drop")


#get a comprehensive list of sites
sites <- sort(unique(all.detections$site))

#get a comprehensive list of species
species <- sort(unique(all.detections$common_name))

#get a comprehensive list of dates
dates <- sort(unique(all.detections$date))


#create an array of site by species by day
all.obs <- base::array(0,
                       dim = c(length(sites),
                               length(species),
                               length(dates)),
                       dimnames = list(site = sites,
                                       species = species,
                                       day = as.character(dates)))


#add detections to array
site.index <- match(daily$site, sites)
species.index <- match(daily$common_name, species)
date.index <- match(daily$date, dates)

all.obs[cbind(site.index,
              species.index,
              date.index)] <- daily$confidence_final


#review
dim(all.obs)
range(all.obs)

length(sites)
length(species)
length(dates)

stopifnot(dim(all.obs)[1] == length(sites))
stopifnot(dim(all.obs)[2] == length(species))
stopifnot(dim(all.obs)[3] == length(dates))
stopifnot(all(all.obs >= 0 & all.obs <= 1))


#save array
saveRDS(all.obs,
        "E:/TNC/TNC Plot Data/site_species_day_array.rds")