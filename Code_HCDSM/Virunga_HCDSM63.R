### Author: Samuel Ayebare
##  Virunga_HCDSM63.r 

## Code : To format the data
##      : Run a hierarchical community distance sampling model relating abundance
##        along an elevation gradient for 63 species of birds 
#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

setwd("./Data_HCDSM63sp")
#----------------#
#-Load libraries-#
#----------------#

library(dplyr)
library(tidyr)
library(jagsUI)


#-------------------------#
#-Import point count data-#
#-------------------------#

virunga <- read.csv("HCDSM_raw_data.csv", header=TRUE)

virunga <- tibble::as_tibble(virunga )
head(virunga)
dim(virunga)

#------------------#
#-Sort by species-#
#-----------------#

gvt <- arrange(virunga, spp_id)
gvt <- tbl_df(gvt)
head(gvt)


#------------------#
#-Trucant the data #
#-----------------#
# Distances classes; 0 - 10m, 10 - 20m, 20 -50m, 50 - 100m
# Distance class 50 - 100m is represented by 4

gvt_obs<- (filter(gvt, dist <= 4))
dim(gvt_obs)
head(gvt_obs)
summary(gvt_obs)


#----------------------------#
#-creating observation model-#
#----------------------------#
obs_data <- uncount(gvt_obs, counts)
dim(obs_data )
head(obs_data )


#----------------------#
#-Species names- 63spp-#
#----------------------#

name= c("Buteo oreophilus", "Halcyon chelicuti", "Pogoniulus bilineatus", "Pogoniulus coryphaeus", "Trachylaemus purpuratus",
        "Colius striatus", "Columba arquatrix", "Streptopelia lugens", "Streptopelia semitorquata", "Centropus monachus",
        "Chrysococcyx klaas", "Cuculus solitarius", "Estrilda astrild", "Estrilda kandti","Crithagra frontalis", "Crithagra gularis",
        "Psalidoprocne pristoptera", "Dryoscopus gambensis", "Laniarius aethiopicus", "Laniarius luehderi", "Laniarius poensis",
        "Telophorus dohertyi ", "Merops oreobates", "Batis diops","Batis molitor","Melaenornis fisheri","Bradornis comitatus",
        "Terpsiphone viridis", "Ruwenzorornis johnstoni", "Tauraco schuettii","Cinnyris regius", "Cinnyris stuhlmanni","Cinnyris venustus",
        "Cyanomitra alinae", "Hedydipna collaris", "Nectarinia johnstoni", "Oriolus percivali", "Melaniparus fasciiventer", "Ploceus alienus",
        "Ploceus baglafecht", "Poicephalus meyeri", "Eurillas latirostris","Andropadus nigriceps", "Pycnonotus barbatus",
        "Apalis personata", "Apalis porphyrolaema", "Bradypterus cinnamomeus", "Iduna natalensis", "Iduna similis","Cisticola chubbi",
        "Oreolais ruwenzori","Phylloscopus laetus","Phylloscopus umbrovirens","Prinia bairdii","Sylvietta leocophrys", "Illadopsis pyrrhoptera",
        "Kakamega poliothorax", "Pseudoalcippe abyssinica", "Apaloderma narina", "Cossypha archeri", "Pogonocichla stellata", "Turdus olivaceus",
        "Zosterops senegalensis")

#-------------------------#
#-Create distance classes-#
#-------------------------#
# distance class per observation
dclass <- obs_data$dist

# Maximum radial distance meters
truca <- 100/100  # scaled maximum radial distance

#Distance class midpoint ID
mdpt <- c(5/100,15/100,35/100,75/100)

#Width of distance classes
delta <- c(10/100,10/100,30/100,50/100)  # scaled by max distance meters

#Number of distance classes
db <- length(mdpt)

#Re-define alternative class midpoints, width to compute cell probabilities. 

mdpt2 <- seq(0.05/2, 1-0.1/2 , 0.05)


delta2 <- rep(0.05, length(mdpt2))

db2 <- length(mdpt2)

#------------------------#
#-Create IDs and indices-#
#------------------------#

#Site ID
site <- obs_data$pt

#Number of observations
nobs <- sum(lengths(obs_data$spp))

#Species ID
spec <- obs_data$spp_id

#Number of species
nspp <- 63

#Number of survey points
nsites <- 519

#Abundance matrix (site x species)
# Observation array
y <- matrix(nrow=519, ncol=63)

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
for(s in 1:63){
  B <- (filter(gvt_obs, spp == name[s])) ## 
  B1 <- group_by(B, pt, family, spp)%>%summarize(counts = sum(counts))
  Y <- full_join(cova, B1, by = "pt" )
  Y$counts[is.na(Y$counts)] = 0
  X <- split(Y$counts, f = Y$pt)
  X <- do.call(rbind, X)
  y[,s] <- X
}


#------------#
#-BUGS Model-#
#------------#

sink("HCDSM63.txt")
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
  
  #Overdispersion
    
   r.N~dunif(0,100)

 
 # Effect of habitat
    for (i in 2:nhabs){                              # line48
      gamma[i] ~ dnorm(0, 0.01) # Prior for habitat means
    }


gamma[1] <- 0  ##set beta for category 1 = 0
  
  for (s in 1:nspp){
    
    alpha[s]~dnorm(mu_d, tau_d) # Intercept parameter
    beta0[s]~dnorm(mu_a,tau_a)  # Intercept parameter
    beta1[s]~dnorm(mu_b,tau_b)  # Effect parameter
    beta2[s]~dnorm(mu_q,tau_q)  # Effect parameter
    

    for (j in 1:nsites)  {

      #Scale parameter

      sigma[j,s]<-exp(alpha[s] + gamma[habID[j]]) # 61
      
      
      ######## Distance sampling detection probability estimation
      
      for(d in 1:db){
       
        # Probability of detetection--> Pr(detect)
        
        log(g[d,j,s])<- -mdpt[d]*mdpt[d]/(2*sigma[j,s]*sigma[j,s])  # half-normal detection function 
        
        # Probability of distribution--> Pr(distribution)
        
        f[d,j,s]<-  (( 2*mdpt[d] )/(truca*truca)) *delta[d]  # this is f(x), the scaled radial density function 
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
      
      ##Latent abundance  (negative binomial)
      N[j,s] ~ dpois(lambda.star[j,s]) 

     #Expected abundance
     lambda.star[j,s] <- rh[j,s] * lambda[j,s] # line 81

     # #Overdispersion parameter for Expected abundance
     rh[j,s] ~ dgamma(r.N, r.N)
      

      #Linear predictor for Expected abundance
      lambda[j,s] <- exp(beta0[s] + beta1[s] * alt[j] + beta2[s] * pow(alt[j], 2))

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
    lbda.star[s] <- sum(lambda.star[,s])
  }
  
}# model loop
",fill=TRUE)
sink()

#-------------------#
#-Compile BUGS data-#
#-------------------#

data <- list(nspp = nspp, db = db2, truca = truca, spec = spec, 
             y = y, delta = delta2, mdpt = mdpt2, nobs = nobs, dclass = dclass, site =site, nsites = nsites, 
             alt = alt_svy, nhabs = nhabs, habID = habID) 


#---------------#
#-Inital values-#
#---------------#

Nst <- data$y + 1

inits <- function(){list(mu_d = runif(1, 0, 1), sig_d = runif(1),      
                         mu_a = runif(1, 0, 1), tau_a = runif(1, 0, 1),
                         mu_b = runif(1, 0, 1), tau_b = runif(1, 0, 1),
                         mu_q = runif(1, 0, 1), tau_q = runif(1, 0, 1),
                         N = Nst)}  

#--------------------#
#-Parameters to save-#
#--------------------#

params <- c('mu_d', 'sig_d',  
             'mu_a', 'sig_a',
            'mu_b', 'sig_b', 'mu_q', 'sig_q', 
            'alpha', paste('gamma[', 2:8,']', sep=''),
            'beta0', 'beta1','beta2','Ns','D','lbda','lbda.star') 

#---------------#
#-MCMC settings-#
#---------------#


nc <- 3
ni <- 200000
nb <- 50000
nt <- 10


HCDSM_Virunga_63spp  <- jags(data = data, inits = inits, parameters.to.save = params, model.file = "HCDSM63.txt", 
                n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt,parallel = TRUE)


save(HCDSM_Virunga_63spp , file = "HCDSM63spp.RData")








