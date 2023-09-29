### Author: Samuel Ayebare
##  Null_model_elev.diet.strata.r 

## Code : Generate a null distribution niche overlap (Pianka) for a community of birds (63) along three niche axis
##                i) Elevation gradient 
##                ii) Diet 
##                iii) Forest strata
#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##


#----------------#
#-Load libraries-#
#----------------#

library(dplyr)
library(tidyr)
library(jagsUI)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(extrafont)
library(spaa)
library(data.table)
loadfonts(device = "win")

#-----------------------#
#-Set working directory-#
#-----------------------#

setwd("./Data_indices_figures")

#---------------------#
#-Load Files and data-#
#---------------------#

#---------------------------------------------------------------#
## Data: For estimating a  Null model distribution  -elevation #
#---------------------------------------------------------------#

load("../Data_indices_figures/HCDSM63spp.RData")

## Model output
HCDSM_Virunga_63spp


# Importing elevation as a covariate
svy_pts <- read.csv("Virunga_covariates.csv", header=TRUE)
svy_pts <- tibble::as_tibble(svy_pts)
head(svy_pts)

# scaled Elevation
elev <- (svy_pts$elev - mean(svy_pts$elev))/sd(svy_pts$elev)


###  - Diet
#--------------#

#---------------------------------------------------------------#
## Data: For estimating a  Null model distribution  - Diet #
#---------------------------------------------------------------#

## Import diet data
diet.63sp <- read.csv("Diet_categories_63species.csv",header = T)

head(diet.63sp)

#---------------------------------------------------------#
## Preparing data for Pianka niche overlap analysis - Diet
#---------------------------------------------------------#
diet.63sp.1 <- diet.63sp [,-1]


###  - Forest strata
#---------------------#

#-------------------------------------------------------------------#
## Data: For estimating a  Null model distribution  - Forest strata #
#------------------------------------------------------------------#

## Import Forest strata
strata.63sp <- read.csv("Strata_categories_63species.csv",header = T)

head(strata.63sp)
#------------------------------------------------------------------#
## Preparing data for Pianka niche overlap analysis - Forest strata
#-----------------------------------------------------------------#
strata.63sp.1 <- strata.63sp [,-1]


## Elevation

##### Estimate Species-specific expected abundance along an elevation gradient
#--------------------------------------------------------------------------------#
Spp1 <-  exp(HCDSM_Virunga_63spp$mean$beta0[1]  + HCDSM_Virunga_63spp$mean$beta1[1]  * elev + HCDSM_Virunga_63spp$mean$beta2[1] * elev^2)
Spp2 <-  exp(HCDSM_Virunga_63spp$mean$beta0[2]  + HCDSM_Virunga_63spp$mean$beta1[2]  * elev + HCDSM_Virunga_63spp$mean$beta2[2] * elev^2)
Spp3 <-  exp(HCDSM_Virunga_63spp$mean$beta0[3]  + HCDSM_Virunga_63spp$mean$beta1[3]  * elev + HCDSM_Virunga_63spp$mean$beta2[3] * elev^2)
Spp4 <-  exp(HCDSM_Virunga_63spp$mean$beta0[4]  + HCDSM_Virunga_63spp$mean$beta1[4]  * elev + HCDSM_Virunga_63spp$mean$beta2[4] * elev^2)
Spp5 <-  exp(HCDSM_Virunga_63spp$mean$beta0[5]  + HCDSM_Virunga_63spp$mean$beta1[5]  * elev + HCDSM_Virunga_63spp$mean$beta2[5] * elev^2)
Spp6 <-  exp(HCDSM_Virunga_63spp$mean$beta0[6]  + HCDSM_Virunga_63spp$mean$beta1[6]  * elev + HCDSM_Virunga_63spp$mean$beta2[6] * elev^2)
Spp7 <-  exp(HCDSM_Virunga_63spp$mean$beta0[7]  + HCDSM_Virunga_63spp$mean$beta1[7]  * elev + HCDSM_Virunga_63spp$mean$beta2[7] * elev^2)
Spp8 <-  exp(HCDSM_Virunga_63spp$mean$beta0[8]  + HCDSM_Virunga_63spp$mean$beta1[8]  * elev + HCDSM_Virunga_63spp$mean$beta2[8] * elev^2)
Spp9 <-  exp(HCDSM_Virunga_63spp$mean$beta0[9]  + HCDSM_Virunga_63spp$mean$beta1[9]  * elev + HCDSM_Virunga_63spp$mean$beta2[9] * elev^2)
Spp10 <-  exp(HCDSM_Virunga_63spp$mean$beta0[10]  + HCDSM_Virunga_63spp$mean$beta1[10]  * elev + HCDSM_Virunga_63spp$mean$beta2[10] * elev^2)
Spp11 <-  exp(HCDSM_Virunga_63spp$mean$beta0[11]  + HCDSM_Virunga_63spp$mean$beta1[11]  * elev + HCDSM_Virunga_63spp$mean$beta2[11] * elev^2)
Spp12 <-  exp(HCDSM_Virunga_63spp$mean$beta0[12]  + HCDSM_Virunga_63spp$mean$beta1[12]  * elev + HCDSM_Virunga_63spp$mean$beta2[12] * elev^2)
Spp13 <-  exp(HCDSM_Virunga_63spp$mean$beta0[13]  + HCDSM_Virunga_63spp$mean$beta1[13]  * elev + HCDSM_Virunga_63spp$mean$beta2[13] * elev^2)
Spp14 <-  exp(HCDSM_Virunga_63spp$mean$beta0[14]  + HCDSM_Virunga_63spp$mean$beta1[14]  * elev + HCDSM_Virunga_63spp$mean$beta2[14] * elev^2)
Spp15 <-  exp(HCDSM_Virunga_63spp$mean$beta0[15]  + HCDSM_Virunga_63spp$mean$beta1[15]  * elev + HCDSM_Virunga_63spp$mean$beta2[15] * elev^2)
Spp16 <-  exp(HCDSM_Virunga_63spp$mean$beta0[16]  + HCDSM_Virunga_63spp$mean$beta1[16]  * elev + HCDSM_Virunga_63spp$mean$beta2[16] * elev^2)
Spp17 <-  exp(HCDSM_Virunga_63spp$mean$beta0[17]  + HCDSM_Virunga_63spp$mean$beta1[17]  * elev + HCDSM_Virunga_63spp$mean$beta2[17] * elev^2)
Spp18 <-  exp(HCDSM_Virunga_63spp$mean$beta0[18]  + HCDSM_Virunga_63spp$mean$beta1[18]  * elev + HCDSM_Virunga_63spp$mean$beta2[18] * elev^2)
Spp19 <-  exp(HCDSM_Virunga_63spp$mean$beta0[19]  + HCDSM_Virunga_63spp$mean$beta1[19]  * elev + HCDSM_Virunga_63spp$mean$beta2[19] * elev^2)
Spp20 <-  exp(HCDSM_Virunga_63spp$mean$beta0[20]  + HCDSM_Virunga_63spp$mean$beta1[20]  * elev + HCDSM_Virunga_63spp$mean$beta2[20] * elev^2)
Spp21 <-  exp(HCDSM_Virunga_63spp$mean$beta0[21]  + HCDSM_Virunga_63spp$mean$beta1[21]  * elev + HCDSM_Virunga_63spp$mean$beta2[21] * elev^2)
Spp22 <-  exp(HCDSM_Virunga_63spp$mean$beta0[22]  + HCDSM_Virunga_63spp$mean$beta1[22]  * elev + HCDSM_Virunga_63spp$mean$beta2[22] * elev^2)
Spp23 <-  exp(HCDSM_Virunga_63spp$mean$beta0[23]  + HCDSM_Virunga_63spp$mean$beta1[23]  * elev + HCDSM_Virunga_63spp$mean$beta2[23] * elev^2)
Spp24 <-  exp(HCDSM_Virunga_63spp$mean$beta0[24]  + HCDSM_Virunga_63spp$mean$beta1[24]  * elev + HCDSM_Virunga_63spp$mean$beta2[24] * elev^2)
Spp25 <-  exp(HCDSM_Virunga_63spp$mean$beta0[25]  + HCDSM_Virunga_63spp$mean$beta1[25]  * elev + HCDSM_Virunga_63spp$mean$beta2[25] * elev^2)
Spp26 <-  exp(HCDSM_Virunga_63spp$mean$beta0[26]  + HCDSM_Virunga_63spp$mean$beta1[26]  * elev + HCDSM_Virunga_63spp$mean$beta2[26] * elev^2)
Spp27 <-  exp(HCDSM_Virunga_63spp$mean$beta0[27]  + HCDSM_Virunga_63spp$mean$beta1[27]  * elev + HCDSM_Virunga_63spp$mean$beta2[27] * elev^2)
Spp28 <-  exp(HCDSM_Virunga_63spp$mean$beta0[28]  + HCDSM_Virunga_63spp$mean$beta1[28]  * elev + HCDSM_Virunga_63spp$mean$beta2[28] * elev^2)
Spp29 <-  exp(HCDSM_Virunga_63spp$mean$beta0[29]  + HCDSM_Virunga_63spp$mean$beta1[29]  * elev + HCDSM_Virunga_63spp$mean$beta2[29] * elev^2)
Spp30 <-  exp(HCDSM_Virunga_63spp$mean$beta0[30]  + HCDSM_Virunga_63spp$mean$beta1[30]  * elev + HCDSM_Virunga_63spp$mean$beta2[30] * elev^2)
Spp31 <-  exp(HCDSM_Virunga_63spp$mean$beta0[31]  + HCDSM_Virunga_63spp$mean$beta1[31]  * elev + HCDSM_Virunga_63spp$mean$beta2[31] * elev^2)
Spp32 <-  exp(HCDSM_Virunga_63spp$mean$beta0[32]  + HCDSM_Virunga_63spp$mean$beta1[32]  * elev + HCDSM_Virunga_63spp$mean$beta2[32] * elev^2)
Spp33 <-  exp(HCDSM_Virunga_63spp$mean$beta0[33]  + HCDSM_Virunga_63spp$mean$beta1[33]  * elev + HCDSM_Virunga_63spp$mean$beta2[33] * elev^2)
Spp34 <-  exp(HCDSM_Virunga_63spp$mean$beta0[34]  + HCDSM_Virunga_63spp$mean$beta1[34]  * elev + HCDSM_Virunga_63spp$mean$beta2[34] * elev^2)
Spp35 <-  exp(HCDSM_Virunga_63spp$mean$beta0[35]  + HCDSM_Virunga_63spp$mean$beta1[35]  * elev + HCDSM_Virunga_63spp$mean$beta2[35] * elev^2)
Spp36 <-  exp(HCDSM_Virunga_63spp$mean$beta0[36]  + HCDSM_Virunga_63spp$mean$beta1[36]  * elev + HCDSM_Virunga_63spp$mean$beta2[36] * elev^2)
Spp37 <-  exp(HCDSM_Virunga_63spp$mean$beta0[37]  + HCDSM_Virunga_63spp$mean$beta1[37]  * elev + HCDSM_Virunga_63spp$mean$beta2[37] * elev^2)
Spp38 <-  exp(HCDSM_Virunga_63spp$mean$beta0[38]  + HCDSM_Virunga_63spp$mean$beta1[38]  * elev + HCDSM_Virunga_63spp$mean$beta2[38] * elev^2)
Spp39 <-  exp(HCDSM_Virunga_63spp$mean$beta0[39]  + HCDSM_Virunga_63spp$mean$beta1[39]  * elev + HCDSM_Virunga_63spp$mean$beta2[39] * elev^2)
Spp40 <-  exp(HCDSM_Virunga_63spp$mean$beta0[40]  + HCDSM_Virunga_63spp$mean$beta1[40]  * elev + HCDSM_Virunga_63spp$mean$beta2[40] * elev^2)
Spp41 <-  exp(HCDSM_Virunga_63spp$mean$beta0[41]  + HCDSM_Virunga_63spp$mean$beta1[41]  * elev + HCDSM_Virunga_63spp$mean$beta2[41] * elev^2)
Spp42 <-  exp(HCDSM_Virunga_63spp$mean$beta0[42]  + HCDSM_Virunga_63spp$mean$beta1[42]  * elev + HCDSM_Virunga_63spp$mean$beta2[42] * elev^2)
Spp43 <-  exp(HCDSM_Virunga_63spp$mean$beta0[43]  + HCDSM_Virunga_63spp$mean$beta1[43]  * elev + HCDSM_Virunga_63spp$mean$beta2[43] * elev^2)
Spp44 <-  exp(HCDSM_Virunga_63spp$mean$beta0[44]  + HCDSM_Virunga_63spp$mean$beta1[44]  * elev + HCDSM_Virunga_63spp$mean$beta2[44] * elev^2)
Spp45 <-  exp(HCDSM_Virunga_63spp$mean$beta0[45]  + HCDSM_Virunga_63spp$mean$beta1[45]  * elev + HCDSM_Virunga_63spp$mean$beta2[45] * elev^2)
Spp46 <-  exp(HCDSM_Virunga_63spp$mean$beta0[46]  + HCDSM_Virunga_63spp$mean$beta1[46]  * elev + HCDSM_Virunga_63spp$mean$beta2[46] * elev^2)
Spp47 <-  exp(HCDSM_Virunga_63spp$mean$beta0[47]  + HCDSM_Virunga_63spp$mean$beta1[47]  * elev + HCDSM_Virunga_63spp$mean$beta2[47] * elev^2)
Spp48 <-  exp(HCDSM_Virunga_63spp$mean$beta0[48]  + HCDSM_Virunga_63spp$mean$beta1[48]  * elev + HCDSM_Virunga_63spp$mean$beta2[48] * elev^2)
Spp49 <-  exp(HCDSM_Virunga_63spp$mean$beta0[49]  + HCDSM_Virunga_63spp$mean$beta1[49]  * elev + HCDSM_Virunga_63spp$mean$beta2[49] * elev^2)
Spp50 <-  exp(HCDSM_Virunga_63spp$mean$beta0[50]  + HCDSM_Virunga_63spp$mean$beta1[50]  * elev + HCDSM_Virunga_63spp$mean$beta2[50] * elev^2)
Spp51 <-  exp(HCDSM_Virunga_63spp$mean$beta0[51]  + HCDSM_Virunga_63spp$mean$beta1[51]  * elev + HCDSM_Virunga_63spp$mean$beta2[51] * elev^2)
Spp52 <-  exp(HCDSM_Virunga_63spp$mean$beta0[52]  + HCDSM_Virunga_63spp$mean$beta1[52]  * elev + HCDSM_Virunga_63spp$mean$beta2[52] * elev^2)
Spp53 <-  exp(HCDSM_Virunga_63spp$mean$beta0[53]  + HCDSM_Virunga_63spp$mean$beta1[53]  * elev + HCDSM_Virunga_63spp$mean$beta2[53] * elev^2)
Spp54 <-  exp(HCDSM_Virunga_63spp$mean$beta0[54]  + HCDSM_Virunga_63spp$mean$beta1[54]  * elev + HCDSM_Virunga_63spp$mean$beta2[54] * elev^2)
Spp55 <-  exp(HCDSM_Virunga_63spp$mean$beta0[55]  + HCDSM_Virunga_63spp$mean$beta1[55]  * elev + HCDSM_Virunga_63spp$mean$beta2[55] * elev^2)
Spp56 <-  exp(HCDSM_Virunga_63spp$mean$beta0[56]  + HCDSM_Virunga_63spp$mean$beta1[56]  * elev + HCDSM_Virunga_63spp$mean$beta2[56] * elev^2)
Spp57 <-  exp(HCDSM_Virunga_63spp$mean$beta0[57]  + HCDSM_Virunga_63spp$mean$beta1[57]  * elev + HCDSM_Virunga_63spp$mean$beta2[57] * elev^2)
Spp58 <-  exp(HCDSM_Virunga_63spp$mean$beta0[58]  + HCDSM_Virunga_63spp$mean$beta1[58]  * elev + HCDSM_Virunga_63spp$mean$beta2[58] * elev^2)
Spp59 <-  exp(HCDSM_Virunga_63spp$mean$beta0[59]  + HCDSM_Virunga_63spp$mean$beta1[59]  * elev + HCDSM_Virunga_63spp$mean$beta2[59] * elev^2)
Spp60 <-  exp(HCDSM_Virunga_63spp$mean$beta0[60]  + HCDSM_Virunga_63spp$mean$beta1[60]  * elev + HCDSM_Virunga_63spp$mean$beta2[60] * elev^2)
Spp61 <-  exp(HCDSM_Virunga_63spp$mean$beta0[61]  + HCDSM_Virunga_63spp$mean$beta1[61]  * elev + HCDSM_Virunga_63spp$mean$beta2[61] * elev^2)
Spp62 <-  exp(HCDSM_Virunga_63spp$mean$beta0[62]  + HCDSM_Virunga_63spp$mean$beta1[62]  * elev + HCDSM_Virunga_63spp$mean$beta2[62] * elev^2)
Spp63 <-  exp(HCDSM_Virunga_63spp$mean$beta0[63]  + HCDSM_Virunga_63spp$mean$beta1[63]  * elev + HCDSM_Virunga_63spp$mean$beta2[63] * elev^2)


##### Species species data.frame for expected abundance

Community.63sp <- data.frame(Spp1,Spp2,Spp3,Spp4,Spp5,Spp6,Spp7,Spp8,Spp9,Spp10,Spp11,Spp12,Spp13,Spp14,Spp15,Spp16,Spp17,Spp18,Spp19,
                      Spp20,Spp21,Spp22,Spp23,Spp24,Spp25,Spp26,Spp27,Spp28,Spp29,Spp30,Spp31,Spp32,Spp33,Spp34,Spp35,Spp36,Spp37,Spp38,Spp39,Spp40,
                      Spp41,Spp42,Spp43,Spp44,Spp45,Spp46,Spp47,Spp48,Spp49,Spp50,Spp51,Spp52,Spp53,Spp54,Spp55,Spp56,Spp57,Spp58,Spp59,Spp60,Spp61,
                      Spp62,Spp63 )

head(Community.63sp )



## Number of samples (each sample = 60 species pairs) selected to generate a null model expectation
n.samples <- 1000 

## Vector for storing niche overlap indices for 46 species (i.e) 60 species pairs
niche_ov <- rep(NA, 60)

## Vector for storing proportion of species pairs with strong overlap.
prop.strong.overlap.samples <- rep(NA, n.samples)

## Vector for storing proportion of species pairs with weak overlap.
prop.weak.overlap.samples <- rep(NA, n.samples)


## Computing null distribution for Pianka's niche overlap: elevation


for (i in 1:n.samples) {
  for (s in 1:60) { 
                       # 1. Choose a random species pair from the total 63 species
                       # 2. Calculate the overlap metric for that pair and store in a vector.
    
   niche_ov [s] <-  niche.overlap(sample(Community.63sp, 2,F) , method = "pianka")
  } # j
  
  prop.weak.overlap.samples [i] <- (sum(niche_ov < 0.5)/60)*100 # 3. Calculate the proportion of pairs with weak overlap. Save in the prop.weak.overlap.samples vector
  prop.strong.overlap.samples [i] <- (sum(niche_ov >= 0.5)/60)*100 # 4. Calculate the proportion of pairs with strong overlap. Save in the prop.strong.overlap.samples vector
  
} # i


### Write output to a csv file

write.csv (prop.weak.overlap.samples,"prop.weak.overlap.samples.elev.csv")

write.csv (prop.strong.overlap.samples,"prop.strong.overlap.samples.ele.csv")



# Diet
#-----------------------------------------------------------------#
## Computing null distribution for Pianka's niche overlap: Diet
#-----------------------------------------------------------------#

#### Number of samples (each sample = 60 species pairs) selected to generate a null model expectation
n.samples <- 1000 

## Vector for storing niche overlap indices for 46 species (i.e) 60 species pairs
niche_ov.d <- rep(NA, 60)

## Vector for storing proportion of species pairs with strong overlap.
prop.strong.overlap.samples.d <- rep(NA, n.samples)

## Vector for storing proportion of species pairs with weak overlap.
prop.weak.overlap.samples.d <- rep(NA, n.samples)


## Computing null distribution for Pianka's niche overlap: diet


for (i in 1:n.samples) {
  for (s in 1:60) { 
    # 1. Choose a random species pair from the total 63 species
    # 2. Calculate the overlap metric for that pair and store in a vector.
    
    niche_ov.d [s]  <-  niche.overlap(sample(diet.63sp.1, 2,F) , method = "pianka")
    
  } # j
  
  prop.weak.overlap.samples.d [i] <- (sum(niche_ov.d <= 0.6)/60)*100  # 3. Calculate the proportion of pairs with weak overlap. Save in the prop.weak.overlap.samples.d vector
  prop.strong.overlap.samples.d [i] <- (sum(niche_ov.d > 0.6)/60)*100# 4. Calculate the proportion of pairs with strong overlap. Save in the prop.strong.overlap.samples.d vector
 
} # i



### Write output to a csv file
write.csv (prop.weak.overlap.samples.d,"prop.weak.overlap.samples.d.csv")

write.csv (prop.strong.overlap.samples.d,"prop.strong.overlap.samples.d.csv")





# Forest strata
#----------------------------------------------------------------------#
## Computing null distribution for Pianka's niche overlap: Forest strata
#----------------------------------------------------------------------#


#### Number of samples (each sample = 60 species pairs) selected to generate a null model expectation
n.samples <- 1000 

## Vector for storing niche overlap indices for 46 species (i.e) 60 species pairs
niche_ov.s <- rep(NA, 60)

## Vector for storing proportion of species pairs with strong overlap.
prop.strong.overlap.samples.s <- rep(NA, n.samples)

## Vector for storing proportion of species pairs with weak overlap.
prop.weak.overlap.samples.s <- rep(NA, n.samples)

## Computing null distribution for Pianka's niche overlap: Forest strata


for (i in 1:n.samples) {
  for (s in 1:60) { 
    # 1. Choose a random species pair from the total 63 species
    # 2. Calculate the overlap metric for that pair and store in a vector.
    
    niche_ov.s [s] <-  niche.overlap(sample(strata.63sp.1, 2,F) , method = "pianka")
  } # j
 
  prop.weak.overlap.samples.s [i] <- (sum(niche_ov.s <= 0.6)/60)*100 # 5. Calculate the proportion of pairs with weak overlap. Save in the prop.weak.overlap.samples.s vector
  prop.strong.overlap.samples.s [i] <- (sum(niche_ov.s > 0.6)/60)*100 # 4. Calculate the proportion of pairs with strong overlap. Save in the prop.strong.overlap.samples.s vector
  
} # i


### Write output to a csv file

write.csv (prop.weak.overlap.samples.s,"prop.weak.overlap.samples.s.csv")

write.csv (prop.strong.overlap.samples.s,"prop.strong.overlap.samples.s.csv")























