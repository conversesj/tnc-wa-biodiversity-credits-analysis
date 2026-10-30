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
  
  
  ######################################################################
  #                                                                    #  
  #                      Model                                         #
  #                                                                    #    
  ######################################################################
  
  ##NIMBLE code 
  occ1 <- nimbleCode( { 
    
    #Likelihood
    for(s in 1:n.species){
      for(p in 1:n.points){
        
        # State Process  
        z[s,p] ~ dbern(psi[s,p])
        
        # Observation Process  
        for(t in 1:n.days){
          
          y[s,p,t] ~ dbern(z[s,p] * p.det[s,p,t]) 
          
          #observation model
          logit(p.det[s,p,t]) <- int.p + beta.effort*log(effort[p,t]) #+ p.rand.point[p] + p.rand.species[s]  
          
        }#t replicate days
        
        #occupancy model 
        logit(psi[s,p]) <- int.psi #+ psi.rand.species[s]
      }#p point
    }#s species
    
    #for(p in 1:n.points){
    #  p.rand.point[p] ~ dnorm(0,sd = sd.p.point)
    #}
    #for(s in 1:n.species){
    #  p.rand.species[s] ~ dnorm(0,sd = sd.p.species)
    #}
    #for(s in 1:n.species){
    #  psi.rand.species[s] ~ dnorm(0,sd = sd.psi.species)
    #}
    
    int.p ~ dnorm(0,1)
    beta.effort ~ dnorm(0,1)
    #sd.p.point ~ dunif(0,5)
    #sd.p.species ~ dunif(0,5)
    
    int.psi ~ dnorm(0,1)
    #sd.psi.species ~ dunif(0,5)
    
  })
  
  ######################################################################
  #                                                                    #  
  #                      Data and Constants                            #
  #                                                                    #    
  ######################################################################
  
  #data y are of dimensions species, site, date 
  
  #create effort 
  effort <- y[1,,]
  effort[which(effort==0)] <- 1 
  effort[which(is.na(effort)==TRUE)] <- 0 
  
  #get rid of NAs in y 
  y.final <- y
  y.final[which(is.na(y)==TRUE)] <- 0 
  
  # Bundle data
  data <- list(y = y)
  
  n.species = dim(y)[1]
  n.points = dim(y)[2]
  n.days = dim(y)[3]
  constants <- list(n.points = n.points, n.species = n.species, n.days = n.days, effort = effort)

  ######################################################################
  #                                                                    #  
  #                             Inits                                  #
  #                                                                    #    
  ######################################################################
  
  z.init <- matrix(0,n.species,n.points)
  for(s in 1:n.species){
    for(p in 1:n.points){
      if(any(y.final[s,p,]==1)){
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
  ni <- 1500
  nt <- 1
  nb <- 250
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
  
  
  
  
}

# Cache results
if (!dir.exists(out_cache_dir)) dir.create(out_cache_dir, recursive = TRUE)
path_occupancy_cache = file.path(out_cache_dir, "occupancy_data.rds")
message("Caching results to ", path_occupancy_cache)
saveRDS(occupancy_data, path_occupancy_cache)
message(crayon::green("Cached", path_occupancy_cache))
