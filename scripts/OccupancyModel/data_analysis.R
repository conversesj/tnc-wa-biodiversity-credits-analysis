library(here)
library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
library(abind)
library(nimble)
library(MCMCvis)
library(coda)

#read in data 
source(here("scripts/OccupancyModel",'read_data.r'))

#put all the processing steps in a function 
get.simple <- function(input){
  #keep only what you need 
  input <- input %>% select(source_file, `Common name`)
  colnames(input) <- c("source_file","common_name")
  #get rid of non-birds 
  input <- input[input$common_name != "Abiotic Aircraft", ]
  input <- input[input$common_name != "Abiotic Logging", ]
  input <- input[input$common_name != "Abiotic Rain", ]
  input <- input[input$common_name != "Abiotic Vehicle", ]
  input <- input[input$common_name != "Abiotic Wind", ]
  input <- input[input$common_name != "Biotic Anuran", ]
  input <- input[input$common_name != "Biotic Insect", ]
  #make dates in the file 
  input$date <- make_date(year = substr(input$source_file,10,13),
                          month = substr(input$source_file,14,15), 
                          day =  substr(input$source_file,16,17))
  #collapse to a record for species by day, with a count of the observations 
  daily <- input %>% group_by(common_name,date) %>% tally() 

  return(daily)
} 

#process files 
for(i in 1:22){
  file <- get(paste0("H",i))
  name <-  paste0("H",i,".s")
  assign(name,as.data.frame(get.simple(file)))
}
for(i in 1:15){
  file <- get(paste0("C",i))
  name <-  paste0("C",i,".s")
  assign(name,as.data.frame(get.simple(file)))
}
for(i in 1:24){
  file <- get(paste0("E",i))
  name <-  paste0("E",i,".s")
  assign(name,as.data.frame(get.simple(file)))
}

#get a comprehensive list of dates
get.dates <- function(code,file.count){
  all.dates <- get(paste0(code,1,".s"))$date
  for(i in 2:file.count){
    names <- get(paste0(code,i,".s"))$date
    all.dates <- c(all.dates,names)
  }
  return(all.dates)
}
dates.H <- get.dates("H",22)
dates.E <- get.dates("E",24)
dates.C <- get.dates("C",15)
dates <- sort(unique(c(dates.H,dates.E,dates.C)))

#remove first date as it is anomalous 
#table(c(dates.H,dates.E,dates.C))
dates <- dates[-1]

#get a comprehensive list of species 
get.spp <- function(code,file.count){
  all.spp <- get(paste0(code,1,".s"))$common_name
  for(i in 2:file.count){
    names <- get(paste0(code,i,".s"))$common_name
    all.spp <- c(all.spp,names)
  }
  return(all.spp)
}
spp.H <- get.spp("H",22)
spp.E <- get.spp("E",24)
spp.C <- get.spp("C",15)
species <- sort(unique(c(spp.H,spp.E,spp.C)))

#create an array of species by date by point 
array <- function(code,file.count,species,dates){
  all.array <- base::array(NA,dim = c(length(species),length(dates),file.count))
  for(i in 1:file.count){
    site <- get(paste0(code,i,".s"))
    for(s in 1:length(species)){
      for(t in 1:length(dates)){
        spp <- which(site$common_name == species[s]) 
        dt <- which(site$date == dates[t])
        inter <- intersect(spp,dt)
        if(length(inter > 0)){
          all.array[s,t,i] <- site$n[inter]
        }
      }
    }
  }
  return(all.array)
}
H.array <- array("H",22,species,dates)
E.array <- array("E",24,species,dates)
C.array <- array("C",15,species,dates)
array.temp <- abind(H.array,E.array,along=3)
all.array <- abind(array.temp,C.array,along=3)

#now we can make this array 0 (if NA) or 1 (if otherwise)
all.obs <- all.array
all.obs[is.na(all.array==TRUE)] <- 0
all.obs[which(all.array>0)] <- 1

#Look at how often different species are seen
#these are the species seen on less than 10 point-days 
species[which(apply(all.obs,c(1),sum)<10)]

#we also need to create an effort matrix, which is dimensions dates by sites
#I am assuming here that if there are no detections of any species in a day, there was no effort on that day  

##TO DO ITEMS##
#we need to figure out why some dates aren't showing up in the data files and add those 
#and then we want to create an effort matrix that is the number of hours in the day with a successful recording 
#this could be from 0 to 24 
#so for each day and point, count the number of recording files that exist and put this in a day * point matrix 

#effort matrix
# effort <- matrix(0,nrow = length(dates),ncol = 22)
# for(i in 1:22){
#   site <- get(paste0("H",i,".s"))
#   for(t in 1:length(dates)){
#     dt <- which(site$date == dates[t])
#     if(length(dt > 0)){
#       effort[t,i] <- 1 
#     }
#   }
# }

#effort matrix: a day x point matrix, value ranging from 0 to 24
#each cell = num of successful recording hours for that day and point
get.effort.one.point <- function(input, dates){
 
  #keep one row per recording file
  files <- input %>%
    select(source_file) %>%
    distinct()
  
  #extract dates, hours, and time from the source_file column
  files$date_string <- str_extract(files$source_file, "\\d{8}")
  files$time_string <- str_extract(files$source_file, "(?<=_)[0-2][0-9][0-5][0-9][0-5][0-9](?=\\.)")
  
  files$date <- ymd(files$date_string)
  files$hour <- as.numeric(substr(files$time_string, 1, 2))
  
  #count the num of unique successful recording hours per day
  daily.effort <- files %>%
    filter(!is.na(date), !is.na(hour), hour >= 0, hour <= 23) %>%
    group_by(date) %>%
    summarize(hours_recorded = n_distinct(hour), .groups = "drop")
  
  #create one effort value for every date in the full dates vector
  effort.vec <- rep(0, length(dates))
  names(effort.vec) <- as.character(dates)
  
  matched <- match(as.character(daily.effort$date), as.character(dates))
  effort.vec[matched[!is.na(matched)]] <- daily.effort$hours_recorded[!is.na(matched)]
  
  return(effort.vec)
}


get.effort.matrix <- function(code, file.count, dates){
  
  effort.mat <- matrix(0, nrow = length(dates), ncol = file.count)
  rownames(effort.mat) <- as.character(dates)
  colnames(effort.mat) <- paste0(code, 1:file.count)
  
  for(i in 1:file.count){
    site <- get(paste0(code, i))
    effort.mat[, i] <- get.effort.one.point(site, dates)
  }
  
  return(effort.mat)
}


H.effort <- get.effort.matrix("H", 22, dates)
E.effort <- get.effort.matrix("E", 24, dates)
C.effort <- get.effort.matrix("C", 15, dates)

#combine them in the same point order as all.obs
effort <- cbind(H.effort, E.effort, C.effort)

#review 
dim(effort)
range(effort)
head(effort[, 1:6])

stopifnot(nrow(effort) == length(dates))
stopifnot(ncol(effort) == dim(all.obs)[3])
stopifnot(all(effort >= 0 & effort <= 24))

######################################################################
#                                                                    #  
#                      Model                                         #
#                                                                    #    
######################################################################

##NIMBLE code 
occ1 <- nimbleCode( { 
  
#Likelihood
for(p in 1:n.points){
  for(s in 1:n.species){
    
    # State Process  
    z[s,p] ~ dbern(psi[s,p])
    
    # Observation Process  
    for(t in 1:n.days){
      
      all.obs[s,t,p] ~ dbern(z[s,p] * p.det[s,t,p] * effort[t,p])  #state * p * effort (0 or 1)
      
      #observation model
      logit(p.det[s,t,p]) <- int.p  #+ p.rand.point[p] + p.rand.species[s]
      
    }#t replicate days
        
        
    #occupancy model 
    logit(psi[s,p]) <- int.psi  #+ psi.rand.species[s]
  }#s species
}#p point
  
int.p ~ dnorm(0,1)
int.psi ~ dnorm(0,1)


#Random effects 
#for(p in 1:n.points){
#  p.rand.point[p] ~ dnorm(0,sd = sd.p.site)
#}
#for(s in 1:n.species){
#  p.rand.species[y] ~ dnorm(0,sd = sd.p.species)
#}
#for(s in 1:n.species){
#  psi.rand.species[s] ~ dnorm(0,sd = sd.psi.species)
#}
#sd.p.site ~ dunif(0,1)
#sd.p.species ~ dunif(0,1)
#sd.psi.species ~ dunif(0,1)

})

######################################################################
#                                                                    #  
#                      Data and Constants                            #
#                                                                    #    
######################################################################
  
# Bundle data
data <- list(all.obs = all.obs)
  
n.points = dim(all.obs)[3]
n.species = dim(all.obs)[1]
n.days = dim(all.obs)[2]
constants <- list(effort = effort, n.points = n.points, n.species = n.species, n.days = n.days)
  
######################################################################
#                                                                    #  
#                             Inits                                  #
#                                                                    #    
######################################################################
  
z.init <- matrix(0,n.species,n.points)
for(s in 1:n.species){
  for(p in 1:n.points){
    if(any(all.obs[s,,p]==1)){
      z.init[s,p] <- 1 
    }  
  }
}
inits <- list(z=z.init)
  
######################################################################
#                                                                    #  
#                         Run the model                              #
#                                                                    #    
######################################################################
  
# Parameters monitored
params <- c("int.p","int.psi") 

# MCMC settings
ni <- 150000
nt <- 1
nb <- 25000
nc <- 3

Rmodel1 <- nimbleModel(code = occ1, constants = constants, data = data,
                         check = FALSE, calculate = FALSE, inits = inits)
conf1 <- configureMCMC(Rmodel1, monitors = params, thin = nt, useConjugacy = FALSE)
Rmcmc1 <- buildMCMC(conf1)
Cmodel1 <- compileNimble(Rmodel1, showCompilerOutput = FALSE)
Cmcmc1 <- compileNimble(Rmcmc1, project = Rmodel1)
  
## Run MCMC ####
out <- runMCMC(Cmcmc1, niter = ni, nburnin = nb , nchains = nc, inits = inits,
                 setSeed = FALSE, progressBar = TRUE, samplesAsCodaMCMC = TRUE)
  
out.all <- rbind(out$chain1,out$chain2,out$chain3)
  
R.hat <- gelman.diag(out[,c(1,2)],multivariate=TRUE)$mpsrf

