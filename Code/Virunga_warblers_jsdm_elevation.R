### Author: Samuel Ayebare
##  Virunga_warblers_jsdm_elevation.r 

## Code: To format the data
##     :  Run a hierarchical community distance sampling model to estimate residual correlations 
##        among five warbler species (Cisticolidae)  after accounting for elevation.
#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

setwd("./Data")
#----------------#
#-Load libraries-#
#----------------#

library(dplyr)
library(tidyr)
library(jagsUI)

#-------------------------#
#-Import point count data-#
#-------------------------#
Warblers_elev <- read.csv("Cisticolidae_raw_data.csv", header=TRUE)

Warblers_elev  <- tibble::as_tibble(Warblers_elev )
head(Warblers_elev )
dim(Warblers_elev )


#------------------#
#-Sort by species-#
#-----------------#
wbler <- arrange(Warblers_elev, spp_id)
wbler <- tbl_df(wbler)
head(wbler)

#------------------#
#-Trucant the data #
#-----------------#
# Distances classes; 0 - 10m, 10 - 20m, 20 -50m, 50 - 100m
# Distance class 50 - 100m is represented by 4

wbler_obs <- (filter(wbler, dist <= 4))
dim(wbler_obs)
head(wbler_obs)
summary(wbler_obs)

#----------------------------#
#-creating observation model-#
#----------------------------#
wbler_data <- uncount(wbler_obs, counts)
dim(wbler_data )
head(wbler_data )

#----------------------#
#-Species names- 5spp-#
#----------------------#

name= c("Apalis personata", "Apalis porphyrolaema", "Cisticola chubbi", "Oreolais ruwenzori", "Prinia bairdii")

#-------------------------#
#-Create distance classes-#
#-------------------------#
# distance class per observation
dclass <- wbler_data$dist

#Width of distance classes
delta <- c(10/100,10/100,30/100,50/100)  # scaled by max distance meters

# radial distance meters
truca <- 100/100  # scaled maximum radial distance

#Distance class midpoint ID
mdpt <- c(5/100,15/100,35/100,75/100)

#Number of distance classes
db <- length(mdpt)

#Re-define alternative class midpoints, width to compute cell probabilities. 
mdpt2 <- seq(0.05/2, 1-0.1/2 , 0.05)


delta2 <- rep(0.05, length(mdpt2))

db2 <- length(mdpt2)



#------------------------#
#-Create IDs#
#------------------------#

#Site ID
site <- wbler_data$pt

#Number of observations
nobs <- sum(lengths(wbler_data$spp))

#Species ID
spec <- wbler_data$spp_id

#Number of species
nspp <- 5

#Number of survey points
nsites <- 519

#Abundance matrix (site x species)
# Observation array
y <- matrix(nrow=519, ncol=5)

# Importing sample points
cova <- read.csv("Virunga_covariates.csv", header=TRUE)

cova <- tbl_df(cova)
head(cova)

# scaled elevation

alt_svy <- (cova$elev - mean(cova$elev))/sd(cova$elev)

# Habitat ID
habID <- cova$nha8

#Number of habitats
nhabs <- 8


#Generate observation array
for(s in 1:5){
  B <- (filter(wbler_obs, spp == name[s])) ## 
  B1 <- group_by(B, pt, family, spp)%>%summarize(counts = sum(counts))
  Y <- full_join(cova, B1, by = "pt" )
  Y$counts[is.na(Y$counts)] = 0
  X <- split(Y$counts, f = Y$pt)
  X <- do.call(rbind, X)
  y[,s] <- X
}

colSums(y)



#------------#
#-BUGS Model-#
#------------#

sink("HCDSM_warblers_elev.txt")
cat("
model{
  
  #--------#
  #-PRIORS-#
  #--------#

## Scale parameter
# alpha - species specific intercept
  
  mu_d ~ dnorm(0,0.01)
  tau_d <- 1/(sig_d*sig_d)
  sig_d~dunif(0,500)

# Abundance model # intercept
  mu_a ~ dnorm(0,0.01)
  sig_a <- 1/sqrt(tau_a)
  tau_a ~ dgamma(0.1,0.1)

# linear effect of altitude 
  mu_b ~ dnorm(0,0.01)
  sig_b <- 1/sqrt(tau_b)
  tau_b ~ dgamma(0.1,0.1)

# Quandratic effect of altitude
  
  mu_q ~ dnorm(0,0.01)
  sig_q <-1/sqrt(tau_q)
  tau_q ~ dgamma(0.1,0.1)
  

# MVN prior for random site effects in lambda for each species

for (j in 1:nsites){

eta.lam[j,1:nspp] ~ dmnorm(mu.eta[], Omega[,])
}

for (s in 1:nspp){
mu.eta[s] <- 0
}

# Vague inverse Wishart prior for variance-covariance matrix
Omega[1:nspp,1:nspp] ~ dwish(R[,], df)
Sigma2[1:nspp,1:nspp] <- inverse(Omega[,])

# Scale var/covar matrix to become the correlation matrix

for (j in 1:nspp){
for (s in 1:nspp){
rho[j,s] <- Sigma2[j,s] / (sqrt(Sigma2[j,j]) * sqrt(Sigma2[s,s]))
}
         }

 
 # Effect of habitat
    for (i in 2:nhabs){                              
      gamma[i] ~ dnorm(0, 0.01) # Prior for habitat means
    }


gamma[1] <- 0  ##set beta for category 1 = 0
  
  for (s in 1:nspp){
    
    alpha[s]~dnorm(mu_d, tau_d) #Intercept parameter
    beta0[s]~dnorm(mu_a,tau_a) #Intercept parameter
    beta1[s]~dnorm(mu_b,tau_b) # #Effect parameter
    beta2[s]~dnorm(mu_q,tau_q) # #Effect parameter
    

    for (j in 1:nsites)  {

      #Scale parameter

      sigma[j,s]<-exp(alpha[s] + gamma[habID[j]]) # 61
      
      
      ######## Distance sampling detection probability estimation
      
      for(d in 1:db){
        # Probability of detetection--> Pr(detect)
        
        log(g[d,j,s])<- -mdpt[d]*mdpt[d]/(2*sigma[j,s]*sigma[j,s])  # half-normal detection function
        
        # Probability of distribution--> Pr(distribution)
        
        f[d,j,s]<-  (( 2*mdpt[d] )/(truca*truca)) *delta[d]  # this is f(x), the scaled radial density function # line 68
        ##Delta[b] added , intervals are not the same among breaks
        
        # Product Pr(detect)*Pr(distribution)
        pi.pd[d,j,s]<- g[d,j,s]*f[d,j,s]  
        
        # Standardizing based on overall capture probability - conditional formulation
        pi.pd.c[d,j,s]<- pi.pd[d,j,s]/pdet[j,s]  
        
      }#end b loop
      
      # probability of detection is the sum of all rectangular areas
      pdet[j,s]<-sum(pi.pd[,j,s])  
     
      #Observed counts  (N-mixture)
      y[j,s] ~ dbin(pdet[j,s], N[j,s])
      
      ##Latent abundance  
      N[j,s] ~ dpois(lambda[j,s]) 


      #Linear predictor for Expected abundance
      lambda[j,s] <- exp(beta0[s] + beta1[s] * alt[j] + beta2[s] * pow(alt[j], 2) + eta.lam[j,s])


# Distance class probabilities based on fine scale distance classes

pi.pd.c2[1,j,s] <- sum (pi.pd.c[1:2,j,s])

pi.pd.c2[2,j,s] <- sum (pi.pd.c[3:4,j,s]) 

pi.pd.c2[3,j,s] <- sum (pi.pd.c[5:10,j,s]) 

pi.pd.c2[4,j,s] <- sum (pi.pd.c[11:db,j,s])

      
    } # jloop
  } # s loop
  
  
  for(i in 1:nobs){
    
    #Observed distance classes
    dclass[i] ~ dcat(pi.pd.c2[,site[i], spec[i]])
    
  }#end i loop
  
  
  for (s in 1:nspp) {
  
    #Abundance per species
    Ns[s] <- sum(N[,s])
    
    #Density per species
    D[s] <- Ns[s]/(519 *3.14*0.1^2) # Radius to km (100/ 1000)= 0.1
    
    ## Expected counts per species
    lbda[s] <- sum(lambda[,s])
    Dl[s] <- lbda[s]/(519 *3.14*0.1^2) # Radius to km (100/ 1000)= 0.1
    
  }
  
}# model loop
",fill=TRUE)
sink()

# Identity matrix

Rmat <- diag(nspp) 
df <- nspp + 1


#-------------------#
#-Compile BUGS data-#
#-------------------#

data <- list(nspp = nspp, db = db2, truca = truca, spec = spec, 
             y = y, delta = delta2, mdpt = mdpt2, nobs = nobs, dclass = dclass, site =site, nsites = nsites, 
             alt = alt_svy, nhabs = nhabs, habID = habID, R= Rmat, df = df)


#---------------#
#-Inital values-#
#---------------#

Nst <- data$y + 1

inits <- function(){list(mu_d = runif(1, 0, 1), sig_d = runif(1),      
                         mu_a = runif(1, 0, 1), tau_a = runif(1, 0, 1),
                         mu_b = runif(1, 0, 1), tau_b = runif(1, 0, 1),
                         mu_q = runif(1, 0, 1), tau_q = runif(1, 0, 1),
                         N = Nst, Omega = diag(5))} 

#--------------------#
#-Parameters to save-#
#--------------------#

params <- c('mu_d', 'sig_d',  
            'mu_a', 'sig_a',
            'mu_b', 'sig_b', 'mu_q', 'sig_q', 
            'alpha', paste('gamma[', 2:8,']', sep=''),
            'beta0', 'beta1','beta2','Ns','D','lbda','Dl','Sigma2', 'rho') 

#---------------#
#-MCMC settings-#
#---------------#

na <- 5000
nc <- 3
ni <- 200000
nb <- 50000
nt <- 50

HCDSM_warblers_Virunga_elev <- jags(data = data, inits = inits, parameters.to.save = params, model.file = "HCDSM_warblers_elev.txt", 
                               n.adapt = na, n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt,parallel = TRUE)


save(HCDSM_warblers_Virunga_elev , file = "HCDSM_warblers_elev.RData")







