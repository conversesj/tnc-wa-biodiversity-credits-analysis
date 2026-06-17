library(here)
library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
library(nimble)
library(MCMCvis)
library(coda)

#read in data 
#MIA: you can go to the file read_data.r and enter the rest of the data files there 
#MIA: you will have to put all the data files in the data folder 
source(here("Mia_capstone",'read_data.r'))

#put all the processing steps in a function so you can apply them to any input file 
get.simple <- function(input){
  
#keep only what you need 
input <- input %>% select(source_file, `Common name`)
colnames(input) <- c("source_file","common_name")

###MAKE SURE TO CHECK FOR MORE NON-BIRDS IN OTHER FILES! 
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

daily.f1 <- as.data.frame(get.simple(f1))  #5-8 to 6-23 plus 7-1 
daily.f2 <- as.data.frame(get.simple(f2))  #5-8 to 6-23
daily.f3 <- as.data.frame(get.simple(f3))  #5-8 to 6-14
daily.f4 <- as.data.frame(get.simple(f4))  #5-8 to 6-13 with days missing 05/18,05/21,05/24,05/26,06/03,06/06,06/09,06/12
daily.f5 <- as.data.frame(get.simple(f5))  #5-8 to 6-23 with days missing incl stretch 5-20 to 6-1 and 06/14,06/16,06/18,06/20,06/22
daily.f6 <- as.data.frame(get.simple(f6))  #5-8 to 6-24
daily.f7 <- as.data.frame(get.simple(f7))  #5-8 to 6-25
daily.f8 <- as.data.frame(get.simple(f8))  #5-8 to 6-23
daily.f9 <- as.data.frame(get.simple(f9))  #5-8 to 6-16
daily.f10 <- as.data.frame(get.simple(f10))  #5-8 to 6-15
daily.f11 <- as.data.frame(get.simple(f11))  #5-8 to 6-22
daily.f12 <- as.data.frame(get.simple(f12))  #5-8 to 6-22
daily.f13 <- as.data.frame(get.simple(f13))  #5-13 to 6-23 with missing days 05/14,05/16,05/18
daily.f14 <- as.data.frame(get.simple(f14))  #5-8 to 6-24
daily.f15 <- as.data.frame(get.simple(f15))  #5-8 to 6-12 
daily.f16 <- as.data.frame(get.simple(f16))  #5-8 to 6-24 
daily.f17 <- as.data.frame(get.simple(f17))  #5-8 to 6-24 
daily.f18 <- as.data.frame(get.simple(f18))  #5-8 to 6-20
daily.f19 <- as.data.frame(get.simple(f19))  #5-8 to 6-14
daily.f20 <- as.data.frame(get.simple(f20))  #5-8 to 6-12 
daily.f21 <- as.data.frame(get.simple(f21))  #5-8 to 6-11
daily.f22 <- as.data.frame(get.simple(f22))  #5-8 to 6-08

#MIA: 
#once all of the daily files are read in, we need to create 
#a species-by-date matrix with 1s when species was seen and 0s when species was not 


start.time <- Sys.time() 

# MCMC settings
ni <- 150000
nt <- 1
nb <- 25000
nc <- 3

  
  ######################################################################
  #                                                                    #  
  #                      Data and Constants                            #
  #                                                                    #    
  ######################################################################
  
  # Bundle data
  data <- list()
  
  constants <- list()
  
  ######################################################################
  #                                                                    #  
  #                             Inits                                  #
  #                                                                    #    
  ######################################################################
  
  z.init <- matrix(0,nrow=n.points,ncol=n.years)
  for(s in 1:n.points){
    for(y in 1:n.years){
      if(any(spp.i[s,y,]==1)){
        z.init[s,y] <- 1 
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
  params <- c("int.p","sd.p.site","sd.p.year","int.psi","beta.site.type","beta.year","beta.year2","w.sitetype","w.year","w.year2") 
  
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
  
  R.hat[i] <- gelman.diag(out[,c(2,3,4,5,6,7,8)],multivariate=TRUE)$mpsrf
  
  write.csv(out.all,paste("results/occ_run4.",spp.names[i], ".csv",sep=""))
  
  print(i)
  
}
(elapsed <- Sys.time() - start.time)

#print R.hat 
R.hat 




