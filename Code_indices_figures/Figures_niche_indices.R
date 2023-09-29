### Author: Samuel Ayebare
## Figures_niche_indices.r

## This script
              # generates 
                        #  a) Figures 2, 3; Appendix S8: Figures S1 - S15
              # computes
 
                        #  b) Elevation niche overlap indices
                        #  c) Diet niche overlap indices
                        #  d) Foraging vertical strata niche overlap indices


############################
###### Figure 1     ########
############################

# Created in ArcMap
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
library(reshape2)
library(ragg)
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

load("../Data_indices_figures/HCDSM63spp.RData")

## Model output
HCDSM_Virunga_63spp


# Importing elevation as a covariate
svy_pts <- read.csv("Virunga_covariates.csv", header=TRUE)
svy_pts <- tibble::as_tibble(svy_pts)
head(svy_pts)

# scaled Elevation
elev <- (svy_pts$elev - mean(svy_pts$elev))/sd(svy_pts$elev)

## Raw elevation values

elev2 <- svy_pts$elev


##################################################
#####Importing diet and foraging forest strata####
##################################################


##############################################################################
                     ###### Musophagidae -- (Turacos)######
###########################--------------------------######################

### Turacos- Diet
#----------------#

Turacos_diet <- read.csv("Turacos_diet.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Turacos_diet1 <- within(Turacos_diet, rm("Species"))

Turacos_diet_t <- transpose(Turacos_diet1)

## Pianka niche overlap -diet

niche.overlap(Turacos_diet_t, method = "pianka")


## Preparing data for ploting : Appendix S8

Turacos_diet2 <- reshape2::melt(Turacos_diet, id.vars = 1)


### Turacos- Foraging forest strata
#----------------------------------#

Turacos_strata <- read.csv("Turacos_strata.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Turacos_strata1 <- within(Turacos_strata, rm("Species"))

Turacos_strata_t <- transpose(Turacos_strata1)

## Pianka niche overlap -strata

niche.overlap(Turacos_strata_t, method = "pianka")


## Preparing data for ploting : Appendix S8

Turacos_strata2 <- reshape2::melt(Turacos_strata, id.vars = 1)



##############################################################################
                     ###### Nectariniidae -- (Sunbirds) ######
###########################-----------------------------######################

### Sunbirds - Diet
#-------------------#
Sunbirds_diet <- read.csv("sunbirds_diet.csv",header = T)


## Preparing data for Pianka niche overlap analysis

Sunbirds_diet1 <- within(Sunbirds_diet, rm("Species"))

Sunbirds_diet_t <- transpose(Sunbirds_diet1)

## Pianka niche overlap -diet

niche.overlap(Sunbirds_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8
Sunbirds_diet2 <- reshape2::melt(Sunbirds_diet, id.vars = 1)




### Sunbirds- Foraging forest strata
#----------------------------------#

Sunbirds_strata <- read.csv("Sunbirds_strata.csv",header = T)


## Preparing data for Pianka niche overlap analysis- strata

Sunbirds_strata1 <- within(Sunbirds_strata, rm("Species"))

Sunbirds_strata_t <- transpose(Sunbirds_strata1)

## Pianka niche overlap -strata

niche.overlap(Sunbirds_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Sunbirds_strata2 <- reshape2::melt(Sunbirds_strata, id.vars = 1)



#############################################################################
                     ###### Cisticolidae  -- (Warblers)######
###########################----------------------------######################


### Warblers - Diet
#------------------#

Warblers_diet <- read.csv("Warblers_diet.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Warblers_diet1 <- within(Warblers_diet, rm("Species"))

Warblers_diet_t <- transpose(Warblers_diet1)

## Pianka niche overlap -diet

niche.overlap(Warblers_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8


Warblers_diet2 <- reshape2::melt(Warblers_diet, id.vars = 1)



### Warblers - Foraging forest strata
#----------------------------------#

Warblers_strata <- read.csv("Warblers_strata.csv",header = T)


## Preparing data for Pianka niche overlap analysis

Warblers_strata1 <- within(Warblers_strata, rm("Species"))

Warblers_strata_t <- transpose(Warblers_strata1)

## Pianka niche overlap - strata

niche.overlap(Warblers_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8


Warblers_strata2 <- reshape2::melt(Warblers_strata, id.vars = 1)


#######################################################################
                     ###### Lybiidae -- (Barbets) ######
###########################----------------------######################

### Barbets - Diet
#----------------#

Barbets_diet <- read.csv("Barbets_diet.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Barbets_diet1 <- within(Barbets_diet, rm("Species"))

Barbets_diet_t <- transpose(Barbets_diet1)

## Pianka niche overlap -diet

niche.overlap(Barbets_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8


Barbets_diet2 <- reshape2::melt(Barbets_diet, id.vars = 1)



### Barbets- Foraging forest strata
#----------------------------------#


Barbets_strata <- read.csv("Barbets_strata.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Barbets_strata1 <- within(Barbets_strata, rm("Species"))

Barbets_strata_t <- transpose(Barbets_strata1)

## Pianka niche overlap - strata

niche.overlap(Barbets_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Barbets_strata2 <- reshape2::melt(Barbets_strata, id.vars = 1)



####################################################################################
                     ###### Phylloscopidae -- (Leaf Warblers) ######
###########################----------------------------------######################

###Leaf Warblers - Diet
#---------------------#

Leaf_warblers_diet <- read.csv("Leaf_warblers_diet.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Leaf_warblers_diet1 <- within(Leaf_warblers_diet, rm("Species"))

Leaf_warblers_diet_t <- transpose(Leaf_warblers_diet1)

## Pianka niche overlap - diet

niche.overlap(Leaf_warblers_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Leaf_warblers_diet2 <- reshape2::melt(Leaf_warblers_diet, id.vars = 1)



### Leaf warblers- Foraging forest strata
#----------------------------------#

Leaf_warblers_strata <- read.csv("Leaf_warblers_strata.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Leaf_warblers_strata1 <- within(Leaf_warblers_strata, rm("Species"))

Leaf_warblers_strata_t <- transpose(Leaf_warblers_strata1)

## Pianka niche overlap - strata

niche.overlap(Leaf_warblers_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Leaf_warblers_strata2 <- reshape2::melt(Leaf_warblers_strata, id.vars = 1)




################################################################################
                     ###### Malaconotidae -- (Bushshrikes) ######
###########################-------------------------------######################

###Bushshrikes - Diet
#-----------------------#
Bushshrikes_diet <- read.csv("Bushshrikes_diet.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Bushshrikes_diet1 <- within(Bushshrikes_diet, rm("Species"))

Bushshrikes_diet_t <- transpose(Bushshrikes_diet1)

## Pianka niche overlap -diet

niche.overlap(Bushshrikes_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8


Bushshrikes_diet2 <- reshape2::melt(Bushshrikes_diet, id.vars = 1)



### Bushshrikes- Foraging forest strata ###
#----------------------------------------#

Bushshrikes_strata <- read.csv("Bushshrikes_strata.csv",header = T)


## Preparing data for Pianka niche overlap analysis

Bushshrikes_strata1 <- within(Bushshrikes_strata, rm("Species"))

Bushshrikes_strata_t <- transpose(Bushshrikes_strata1)

## Pianka niche overlap - strata

niche.overlap(Bushshrikes_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Bushshrikes_strata2 <- reshape2::melt(Bushshrikes_strata, id.vars = 1)





#######################################################################
                     ###### Cuculidae -- (Cuckoos) ######
###########################------------------------####################


###Cuckoos - Diet
#---------------------#

Cuckoos_diet <- read.csv("Cuckoos_diet.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Cuckoos_diet1 <- within(Cuckoos_diet, rm("Species"))

Cuckoos_diet_t <- transpose(Cuckoos_diet1)

## Pianka niche overlap -diet

niche.overlap(Cuckoos_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Cuckoos_diet2 <- reshape2::melt(Cuckoos_diet, id.vars = 1)



### Cuckoos- Foraging forest strata
#----------------------------------#


Cuckoos_strata <- read.csv("Cuckoos_strata.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Cuckoos_strata1 <- within(Cuckoos_strata, rm("Species"))

Cuckoos_strata_t <- transpose(Cuckoos_strata1)

## Pianka niche overlap - strata

niche.overlap(Cuckoos_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Cuckoos_strata2 <- reshape2::melt(Cuckoos_strata, id.vars = 1)



############################################################################
                     ###### Pycnonotidae -- (Bulbuls) ######
###########################--------------------------######################

###Bulbuls - Diet
#---------------------#
Bulbuls_diet <- read.csv("Bulbuls_diet.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Bulbuls_diet1 <- within(Bulbuls_diet, rm("Species"))

Bulbuls_diet_t <- transpose(Bulbuls_diet1)

## Pianka niche overlap - - diet

niche.overlap(Bulbuls_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Bulbuls_diet2 <- reshape2::melt(Bulbuls_diet, id.vars = 1)



### Bulbuls- Foraging forest strata
#----------------------------------#

Bulbuls_strata <- read.csv("Bulbuls_strata.csv",header = T)

## Preparing data for Pianka niche overlap analysis

Bulbuls_strata1 <- within(Bulbuls_strata, rm("Species"))

Bulbuls_strata_t <- transpose(Bulbuls_strata1)

## Pianka niche overlap - strata

niche.overlap(Bulbuls_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Bulbuls_strata2 <- reshape2::melt(Bulbuls_strata, id.vars = 1)



################################################################################
                     ###### Columbidae -- (Doves and Pigeons) ######
###########################----------------------------------###################

###Doves & Pigeons - Diet
#------------------------#

DovesnPigeons_diet <- read.csv("Doves_n_Pigeons_diet.csv",header = T)

## Preparing data for Pianka niche overlap analysis

DovesnPigeons_diet1 <- within(DovesnPigeons_diet, rm("Species"))

DovesnPigeons_diet_t <- transpose(DovesnPigeons_diet1)

## Pianka niche overlap - - diet

niche.overlap(DovesnPigeons_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

DovesnPigeons_diet2 <- reshape2::melt(DovesnPigeons_diet, id.vars = 1)



###Doves & Pigeons - Foraging forest strata
#------------------------------------------#

DovesnPigeons_strata <- read.csv("Doves_n_Pigeons_strata.csv",header = T)


# Preparing data for Pianka niche overlap analysis

DovesnPigeons_strata1 <- within(DovesnPigeons_strata, rm("Species"))

DovesnPigeons_strata_t <- transpose(DovesnPigeons_strata1)

## Pianka niche overlap - - diet

niche.overlap(DovesnPigeons_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

DovesnPigeons_strata2 <- reshape2::melt(DovesnPigeons_strata, id.vars = 1)


#############################################################################
                     ###### Platysteiridae- (Batises) ######
###########################---------------------------######################

###Batises- Diet
#---------------------#

Batises_diet <- read.csv("Batises_diet.csv",header = T)

# Preparing data for Pianka niche overlap analysis

Batises_diet1 <- within(Batises_diet, rm("Species"))

Batises_diet_t <- transpose(Batises_diet1)

## Pianka niche overlap - - diet

niche.overlap(Batises_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Batises_diet2 <- reshape2::melt(Batises_diet, id.vars = 1)



###Batises - Foraging forest strata
#------------------------------------------#

Batises_strata <- read.csv("Batises_strata.csv",header = T)

# Preparing data for Pianka niche overlap analysis

Batises_strata1 <- within(Batises_strata, rm("Species"))

Batises_strata_t <- transpose(Batises_strata1)

## Pianka niche overlap - strata

niche.overlap(Batises_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Batises_strata2 <- reshape2::melt(Batises_strata, id.vars = 1)



#########################################################################
                      ###### Ploceidae- (Weavers) ######
###########################---------------------- ######################

###Weavers - Diet
#---------------------#

Weavers_diet <- read.csv("Weavers_diet.csv",header = T)

# Preparing data for Pianka niche overlap analysis

Weavers_diet1 <- within(Weavers_diet, rm("Species"))

Weavers_diet_t <- transpose(Weavers_diet1)

## Pianka niche overlap - - diet

niche.overlap(Weavers_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Weavers_diet2 <- reshape2::melt(Weavers_diet, id.vars = 1)


###Weavers - Foraging forest strata
#------------------------------------------#


Weavers_strata <- read.csv("Weavers_strata.csv",header = T)

# Preparing data for Pianka niche overlap analysis

Weavers_strata1 <- within(Weavers_strata, rm("Species"))

Weavers_strata_t <- transpose(Weavers_strata1)

## Pianka niche overlap - strata

niche.overlap(Weavers_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Weavers_strata2 <- reshape2::melt(Weavers_strata, id.vars = 1)



###########################################################################
                     ###### Fringillidae- (Finches) ######
###########################-------------------------######################


###Finches - Diet
#----------------#


Finches_diet <- read.csv("Finches_diet.csv",header = T)

# Preparing data for Pianka niche overlap analysis

Finches_diet1 <- within(Finches_diet, rm("Species"))

Finches_diet_t <- transpose(Finches_diet1)

## Pianka niche overlap - - diet

niche.overlap(Finches_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Finches_diet2 <- reshape2::melt(Finches_diet, id.vars = 1)


###Finches - Foraging forest strata
#------------------------------------------#

Finches_strata <- read.csv("Finches_strata.csv",header = T)


# Preparing data for Pianka niche overlap analysis

Finches_strata1 <- within(Finches_strata, rm("Species"))

Finches_strata_t <- transpose(Finches_strata1)

## Pianka niche overlap - strata

niche.overlap(Finches_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Finches_strata2 <- reshape2::melt(Finches_strata, id.vars = 1)



##########################################################################
                    ###### Estrildidae- (Waxbills) ######
###########################------------------------######################

###Waxbills - Diet
#-----------------#

Waxbills_diet <- read.csv("Waxbills_diet.csv",header = T)
# Preparing data for Pianka niche overlap analysis

Waxbills_diet1 <- within(Waxbills_diet, rm("Species"))

Waxbills_diet_t <- transpose(Waxbills_diet1)

## Pianka niche overlap -diet

niche.overlap(Waxbills_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Waxbills_diet2 <- reshape2::melt(Waxbills_diet, id.vars = 1)


###Waxbills- Foraging forest strata
#------------------------------------------#

Waxbills_strata <- read.csv("Waxbills_strata.csv",header = T)

# Preparing data for Pianka niche overlap analysis

Waxbills_strata1 <- within(Waxbills_strata, rm("Species"))

Waxbills_strata_t <- transpose(Waxbills_strata1)

## Pianka niche overlap - strata

niche.overlap(Waxbills_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Waxbills_strata2 <- reshape2::melt(Waxbills_strata, id.vars = 1)





###############################################################################
                      ###### Acrocephalid_warblers######
###########################-----------------------------######################

###Acrocephalid_warblers - Diet
#---------------------------------#

Acrocephalid_warblers_diet <- read.csv("Acrocephalid_warblers_diet.csv",header = T)

# Preparing data for Pianka niche overlap analysis

Acrocephalid_warblers_diet1 <- within(Acrocephalid_warblers_diet, rm("Species"))

Acrocephalid_warblers_diet_t <- transpose(Acrocephalid_warblers_diet1)

## Pianka niche overlap - - diet

niche.overlap(Acrocephalid_warblers_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Acrocephalid_warblers_diet2 <- reshape2::melt(Acrocephalid_warblers_diet, id.vars = 1)



###Acrocephalid_warblers - Foraging forest strata
#------------------------------------------#

Acrocephalid_warblers_strata <- read.csv("Acrocephalid_warblers_strata.csv",header = T)


# Preparing data for Pianka niche overlap analysis

Acrocephalid_warblers_strata1 <- within(Acrocephalid_warblers_strata, rm("Species"))

Acrocephalid_warblers_strata_t <- transpose(Acrocephalid_warblers_strata1)

## Pianka niche overlap - - diet

niche.overlap(Acrocephalid_warblers_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Acrocephalid_warblers_strata2 <- reshape2::melt(Acrocephalid_warblers_strata, id.vars = 1)


###############################################################################
                     ###### Muscicapidae- Flycatchers #####
###########################---------------------------######################

###Flycatchers - Diet
#---------------------#


Flycatchers_diet <- read.csv("Flycatchers_diet.csv",header = T)

# Preparing data for Pianka niche overlap analysis

Flycatchers_diet1 <- within(Flycatchers_diet, rm("Species"))

Flycatchers_diet_t <- transpose(Flycatchers_diet1)

## Pianka niche overlap - diet

niche.overlap(Flycatchers_diet_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Flycatchers_diet2 <- reshape2::melt(Flycatchers_diet, id.vars = 1)




###Flycatchers - Foraging forest strata
#------------------------------------------#

Flycatchers_strata <- read.csv("Flycatchers_strata.csv",header = T)


# Preparing data for Pianka niche overlap analysis

Flycatchers_strata1 <- within(Flycatchers_strata, rm("Species"))

Flycatchers_strata_t <- transpose(Flycatchers_strata1)

## Pianka niche overlap - strata

niche.overlap(Flycatchers_strata_t, method = "pianka")


# Preparing data for ploting : Appendix S8

Flycatchers_strata2 <- reshape2::melt(Flycatchers_strata, id.vars = 1)







##############################################
##### Species abundance-Elevation curves######
##############################################


###### Expected community_level_abundance- 63spp######
###### Figure 2 (a) ##################################
community <-  (exp(HCDSM_Virunga_63spp$mean$mu_a  + HCDSM_Virunga_63spp$mean$mu_b * elev + HCDSM_Virunga_63spp$mean$mu_q* elev^2))
max(community)

####### 95% credible intervals
lower <-  (exp(HCDSM_Virunga_63spp$q2.5$mu_a  + HCDSM_Virunga_63spp$q2.5$mu_b * elev + HCDSM_Virunga_63spp$q2.5$mu_q* elev^2))
upper <-  (exp(HCDSM_Virunga_63spp$q97.5$mu_a  + HCDSM_Virunga_63spp$q97.5$mu_b * elev + HCDSM_Virunga_63spp$q97.5$mu_q* elev^2))

##### Create community dataframe
community_data <- data.frame (community, elev2 , lower, upper)


# Plot
Figure2_a = ggplot(data=community_data, aes(x=elev2 , y= community)) +
  geom_line( color ="blue", size =3)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  
 
  
  annotate("text", x = 2700, y = 80, hjust = 0, size = 10, family = "Times New Roman", 
           label = "", fontface= "plain") +
  coord_cartesian(ylim = c(0, 2))

Figure2_a  + labs (x="Elevation(m)", y ="Expected community abundance", color = "Legend\n",fontface= "plain") +
  
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25,color = "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25,color = "black"),
        axis.title.y = element_text(size = rel(1.8), angle = 90),
        axis.title.x = element_text(size = rel(1.8), angle = 00),
        legend.text=element_text(size=25),
        legend.position = c(0.7,0.6), legend.title=element_blank())

ggsave(file = "Figure 2(a).jpg", bg = NULL, dpi = 400, width = 15, height = 10)



###### Figure 2 (b) ########
#####Species-specific abundance-Elevation response curves 

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


##### Create community dataframe- species specific

All_spp <- data.frame(Spp1,Spp2,Spp3,Spp4,Spp5,Spp6,Spp7,Spp8,Spp9,Spp10,Spp11,Spp12,Spp13,Spp14,Spp15,Spp16,Spp17,Spp18,Spp19,
                      Spp20,Spp21,Spp22,Spp23,Spp24,Spp25,Spp26,Spp27,Spp28,Spp29,Spp30,Spp31,Spp32,Spp33,Spp34,Spp35,Spp36,Spp37,Spp38,Spp39,Spp40,
                      Spp41,Spp42,Spp43,Spp44,Spp45,Spp46,Spp47,Spp48,Spp49,Spp50,Spp51,Spp52,Spp53,Spp54,Spp55,Spp56,Spp57,Spp58,Spp59,Spp60,Spp61,
                      Spp62,Spp63,elev2 )



#Plot
Community_birds <- ggplot (All_spp) +
  
  geom_line (aes(x=elev2 , y= Spp1), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp2),size =0.75) + 
  geom_line (aes(x=elev2 , y= Spp3),size =0.75) +
  geom_line (aes(x=elev2 , y= Spp4), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp5), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp6), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp7), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp8), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp9), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp10), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp11), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp12), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp13), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp14), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp15), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp16), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp17), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp18), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp19), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp20), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp21), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp22), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp23), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp24), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp25), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp26), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp27), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp28), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp29), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp30), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp31), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp32), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp33), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp34), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp35), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp36), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp37), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp38), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp39), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp40), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp41), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp42), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp43), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp44), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp45), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp46), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp47), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp48), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp49), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp50), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp51), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp52), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp53), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp54), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp55), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp56), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp57), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp58), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp59), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp60), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp61), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp62), size =0.75) +
  geom_line (aes(x=elev2 , y= Spp63), size =0.75)


Community_birds + labs (x="Elevation(m)", y ="Expected species abundance", color = "Legend\n",fontface= "plain") +

  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25,color = "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25,color = "black"),
        axis.title.y = element_text(size = rel(1.8), angle = 90),
        axis.title.x = element_text(size = rel(1.8), angle = 00),
        legend.text=element_text(size=25),
        legend.position = c(0.7,0.6), legend.title=element_blank())




ggsave(file = "Figure2(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



###### Figure 2 (c) ########
#####Species-specific abundance-Elevation response curves 
##### Standardized############

Sppz1 <-  exp(HCDSM_Virunga_63spp$mean$beta0[1]  + HCDSM_Virunga_63spp$mean$beta1[1]  * elev + HCDSM_Virunga_63spp$mean$beta2[1] * elev^2)
Sppz1c <- Sppz1/max(Sppz1)
Sppz2 <-  exp(HCDSM_Virunga_63spp$mean$beta0[2]  + HCDSM_Virunga_63spp$mean$beta1[2]  * elev + HCDSM_Virunga_63spp$mean$beta2[2] * elev^2)
Sppz2c <- Sppz2/max(Sppz2)
Sppz3 <-  exp(HCDSM_Virunga_63spp$mean$beta0[3]  + HCDSM_Virunga_63spp$mean$beta1[3]  * elev + HCDSM_Virunga_63spp$mean$beta2[3] * elev^2)
Sppz3c <- Sppz3/max(Sppz3)
Sppz4 <-  exp(HCDSM_Virunga_63spp$mean$beta0[4]  + HCDSM_Virunga_63spp$mean$beta1[4]  * elev + HCDSM_Virunga_63spp$mean$beta2[4] * elev^2)
Sppz4c <- Sppz4/max(Sppz4)
Sppz5 <-  exp(HCDSM_Virunga_63spp$mean$beta0[5]  + HCDSM_Virunga_63spp$mean$beta1[5]  * elev + HCDSM_Virunga_63spp$mean$beta2[5] * elev^2)
Sppz5c <- Sppz5/max(Sppz5)
Sppz6 <-  exp(HCDSM_Virunga_63spp$mean$beta0[6]  + HCDSM_Virunga_63spp$mean$beta1[6]  * elev + HCDSM_Virunga_63spp$mean$beta2[6] * elev^2)
Sppz6c <- Sppz6/max(Sppz6)
Sppz7 <-  exp(HCDSM_Virunga_63spp$mean$beta0[7]  + HCDSM_Virunga_63spp$mean$beta1[7]  * elev + HCDSM_Virunga_63spp$mean$beta2[7] * elev^2)
Sppz7c <- Sppz7/max(Sppz7)
Sppz8 <-  exp(HCDSM_Virunga_63spp$mean$beta0[8]  + HCDSM_Virunga_63spp$mean$beta1[8]  * elev + HCDSM_Virunga_63spp$mean$beta2[8] * elev^2)
Sppz8c <- Sppz8/max(Sppz8)
Sppz9 <-  exp(HCDSM_Virunga_63spp$mean$beta0[9]  + HCDSM_Virunga_63spp$mean$beta1[9]  * elev + HCDSM_Virunga_63spp$mean$beta2[9] * elev^2)
Sppz9c <- Sppz9/max(Sppz9)
Sppz10 <-  exp(HCDSM_Virunga_63spp$mean$beta0[10]  + HCDSM_Virunga_63spp$mean$beta1[10]  * elev + HCDSM_Virunga_63spp$mean$beta2[10] * elev^2)
Sppz10c <- Sppz10/max(Sppz10)
Sppz11 <-  exp(HCDSM_Virunga_63spp$mean$beta0[11]  + HCDSM_Virunga_63spp$mean$beta1[11]  * elev + HCDSM_Virunga_63spp$mean$beta2[11] * elev^2)
Sppz11c <- Sppz11/max(Sppz11)
Sppz12 <-  exp(HCDSM_Virunga_63spp$mean$beta0[12]  + HCDSM_Virunga_63spp$mean$beta1[12]  * elev + HCDSM_Virunga_63spp$mean$beta2[12] * elev^2)
Sppz12c <- Sppz12/max(Sppz12)
Sppz13 <-  exp(HCDSM_Virunga_63spp$mean$beta0[13]  + HCDSM_Virunga_63spp$mean$beta1[13]  * elev + HCDSM_Virunga_63spp$mean$beta2[13] * elev^2)
Sppz13c <- Sppz13/max(Sppz13)
Sppz14 <-  exp(HCDSM_Virunga_63spp$mean$beta0[14]  + HCDSM_Virunga_63spp$mean$beta1[14]  * elev + HCDSM_Virunga_63spp$mean$beta2[14] * elev^2)
Sppz14c <- Sppz14/max(Sppz14)
Sppz15 <-  exp(HCDSM_Virunga_63spp$mean$beta0[15]  + HCDSM_Virunga_63spp$mean$beta1[15]  * elev + HCDSM_Virunga_63spp$mean$beta2[15] * elev^2)
Sppz15c <- Sppz15/max(Sppz15)
Sppz16 <-  exp(HCDSM_Virunga_63spp$mean$beta0[16]  + HCDSM_Virunga_63spp$mean$beta1[16]  * elev + HCDSM_Virunga_63spp$mean$beta2[16] * elev^2)
Sppz16c <- Sppz16/max(Sppz16)
Sppz17 <-  exp(HCDSM_Virunga_63spp$mean$beta0[17]  + HCDSM_Virunga_63spp$mean$beta1[17]  * elev + HCDSM_Virunga_63spp$mean$beta2[17] * elev^2)
Sppz17c <- Sppz17/max(Sppz17)
Sppz18 <-  exp(HCDSM_Virunga_63spp$mean$beta0[18]  + HCDSM_Virunga_63spp$mean$beta1[18]  * elev + HCDSM_Virunga_63spp$mean$beta2[18] * elev^2)
Sppz18c <- Sppz18/max(Sppz18)
Sppz19 <-  exp(HCDSM_Virunga_63spp$mean$beta0[19]  + HCDSM_Virunga_63spp$mean$beta1[19]  * elev + HCDSM_Virunga_63spp$mean$beta2[19] * elev^2)
Sppz19c <- Sppz19/max(Sppz19)
Sppz20 <-  exp(HCDSM_Virunga_63spp$mean$beta0[20]  + HCDSM_Virunga_63spp$mean$beta1[20]  * elev + HCDSM_Virunga_63spp$mean$beta2[20] * elev^2)
Sppz20c <- Sppz20/max(Sppz20)
Sppz21 <-  exp(HCDSM_Virunga_63spp$mean$beta0[21]  + HCDSM_Virunga_63spp$mean$beta1[21]  * elev + HCDSM_Virunga_63spp$mean$beta2[21] * elev^2)
Sppz21c <- Sppz21/max(Sppz21)
Sppz22 <-  exp(HCDSM_Virunga_63spp$mean$beta0[22]  + HCDSM_Virunga_63spp$mean$beta1[22]  * elev + HCDSM_Virunga_63spp$mean$beta2[22] * elev^2)
Sppz22c <- Sppz22/max(Sppz22)
Sppz23 <-  exp(HCDSM_Virunga_63spp$mean$beta0[23]  + HCDSM_Virunga_63spp$mean$beta1[23]  * elev + HCDSM_Virunga_63spp$mean$beta2[23] * elev^2)
Sppz23c <- Sppz23/max(Sppz23)
Sppz24 <-  exp(HCDSM_Virunga_63spp$mean$beta0[24]  + HCDSM_Virunga_63spp$mean$beta1[24]  * elev + HCDSM_Virunga_63spp$mean$beta2[24] * elev^2)
Sppz24c <- Sppz24/max(Sppz24)
Sppz25 <-  exp(HCDSM_Virunga_63spp$mean$beta0[25]  + HCDSM_Virunga_63spp$mean$beta1[25]  * elev + HCDSM_Virunga_63spp$mean$beta2[25] * elev^2)
Sppz25c <- Sppz25/max(Sppz25)
Sppz26 <-  exp(HCDSM_Virunga_63spp$mean$beta0[26]  + HCDSM_Virunga_63spp$mean$beta1[26]  * elev + HCDSM_Virunga_63spp$mean$beta2[26] * elev^2)
Sppz26c <- Sppz26/max(Sppz26)
Sppz27 <-  exp(HCDSM_Virunga_63spp$mean$beta0[27]  + HCDSM_Virunga_63spp$mean$beta1[27]  * elev + HCDSM_Virunga_63spp$mean$beta2[27] * elev^2)
Sppz27c <- Sppz27/max(Sppz27)
Sppz28 <-  exp(HCDSM_Virunga_63spp$mean$beta0[28]  + HCDSM_Virunga_63spp$mean$beta1[28]  * elev + HCDSM_Virunga_63spp$mean$beta2[28] * elev^2)
Sppz28c <- Sppz28/max(Sppz28)
Sppz29 <-  exp(HCDSM_Virunga_63spp$mean$beta0[29]  + HCDSM_Virunga_63spp$mean$beta1[29]  * elev + HCDSM_Virunga_63spp$mean$beta2[29] * elev^2)
Sppz29c <- Sppz29/max(Sppz29)
Sppz30 <-  exp(HCDSM_Virunga_63spp$mean$beta0[30]  + HCDSM_Virunga_63spp$mean$beta1[30]  * elev + HCDSM_Virunga_63spp$mean$beta2[30] * elev^2)
Sppz30c <- Sppz30/max(Sppz30)
Sppz31 <-  exp(HCDSM_Virunga_63spp$mean$beta0[31]  + HCDSM_Virunga_63spp$mean$beta1[31]  * elev + HCDSM_Virunga_63spp$mean$beta2[31] * elev^2)
Sppz31c <- Sppz31/max(Sppz31)
Sppz32 <-  exp(HCDSM_Virunga_63spp$mean$beta0[32]  + HCDSM_Virunga_63spp$mean$beta1[32]  * elev + HCDSM_Virunga_63spp$mean$beta2[32] * elev^2)
Sppz32c <- Sppz32/max(Sppz32)
Sppz33 <-  exp(HCDSM_Virunga_63spp$mean$beta0[33]  + HCDSM_Virunga_63spp$mean$beta1[33]  * elev + HCDSM_Virunga_63spp$mean$beta2[33] * elev^2)
Sppz33c <- Sppz33/max(Sppz33)
Sppz34 <-  exp(HCDSM_Virunga_63spp$mean$beta0[34]  + HCDSM_Virunga_63spp$mean$beta1[34]  * elev + HCDSM_Virunga_63spp$mean$beta2[34] * elev^2)
Sppz34c <- Sppz34/max(Sppz34)
Sppz35 <-  exp(HCDSM_Virunga_63spp$mean$beta0[35]  + HCDSM_Virunga_63spp$mean$beta1[35]  * elev + HCDSM_Virunga_63spp$mean$beta2[35] * elev^2)
Sppz35c <- Sppz35/max(Sppz35)
Sppz36 <-  exp(HCDSM_Virunga_63spp$mean$beta0[36]  + HCDSM_Virunga_63spp$mean$beta1[36]  * elev + HCDSM_Virunga_63spp$mean$beta2[36] * elev^2)
Sppz36c <- Sppz36/max(Sppz36)
Sppz37 <-  exp(HCDSM_Virunga_63spp$mean$beta0[37]  + HCDSM_Virunga_63spp$mean$beta1[37]  * elev + HCDSM_Virunga_63spp$mean$beta2[37] * elev^2)
Sppz37c <- Sppz37/max(Sppz37)
Sppz38 <-  exp(HCDSM_Virunga_63spp$mean$beta0[38]  + HCDSM_Virunga_63spp$mean$beta1[38]  * elev + HCDSM_Virunga_63spp$mean$beta2[38] * elev^2)
Sppz38c <- Sppz38/max(Sppz38)
Sppz39 <-  exp(HCDSM_Virunga_63spp$mean$beta0[39]  + HCDSM_Virunga_63spp$mean$beta1[39]  * elev + HCDSM_Virunga_63spp$mean$beta2[39] * elev^2)
Sppz39c <- Sppz39/max(Sppz39)
Sppz40 <-  exp(HCDSM_Virunga_63spp$mean$beta0[40]  + HCDSM_Virunga_63spp$mean$beta1[40]  * elev + HCDSM_Virunga_63spp$mean$beta2[40] * elev^2)
Sppz40c <- Sppz40/max(Sppz40)
Sppz41 <-  exp(HCDSM_Virunga_63spp$mean$beta0[41]  + HCDSM_Virunga_63spp$mean$beta1[41]  * elev + HCDSM_Virunga_63spp$mean$beta2[41] * elev^2)
Sppz41c <- Sppz41/max(Sppz41)
Sppz42 <-  exp(HCDSM_Virunga_63spp$mean$beta0[42]  + HCDSM_Virunga_63spp$mean$beta1[42]  * elev + HCDSM_Virunga_63spp$mean$beta2[42] * elev^2)
Sppz42c <- Sppz42/max(Sppz42)
Sppz43 <-  exp(HCDSM_Virunga_63spp$mean$beta0[43]  + HCDSM_Virunga_63spp$mean$beta1[43]  * elev + HCDSM_Virunga_63spp$mean$beta2[43] * elev^2)
Sppz43c <- Sppz43/max(Sppz43)
Sppz44 <-  exp(HCDSM_Virunga_63spp$mean$beta0[44]  + HCDSM_Virunga_63spp$mean$beta1[44]  * elev + HCDSM_Virunga_63spp$mean$beta2[44] * elev^2)
Sppz44c <- Sppz44/max(Sppz44)
Sppz45 <-  exp(HCDSM_Virunga_63spp$mean$beta0[45]  + HCDSM_Virunga_63spp$mean$beta1[45]  * elev + HCDSM_Virunga_63spp$mean$beta2[45] * elev^2)
Sppz45c <- Sppz45/max(Sppz45)
Sppz46 <-  exp(HCDSM_Virunga_63spp$mean$beta0[46]  + HCDSM_Virunga_63spp$mean$beta1[46]  * elev + HCDSM_Virunga_63spp$mean$beta2[46] * elev^2)
Sppz46c <- Sppz46/max(Sppz46)
Sppz47 <-  exp(HCDSM_Virunga_63spp$mean$beta0[47]  + HCDSM_Virunga_63spp$mean$beta1[47]  * elev + HCDSM_Virunga_63spp$mean$beta2[47] * elev^2)
Sppz47c <- Sppz47/max(Sppz47)
Sppz48 <-  exp(HCDSM_Virunga_63spp$mean$beta0[48]  + HCDSM_Virunga_63spp$mean$beta1[48]  * elev + HCDSM_Virunga_63spp$mean$beta2[48] * elev^2)
Sppz48c <- Sppz48/max(Sppz48)
Sppz49 <-  exp(HCDSM_Virunga_63spp$mean$beta0[49]  + HCDSM_Virunga_63spp$mean$beta1[49]  * elev + HCDSM_Virunga_63spp$mean$beta2[49] * elev^2)
Sppz49c <- Sppz49/max(Sppz49)
Sppz50 <-  exp(HCDSM_Virunga_63spp$mean$beta0[50]  + HCDSM_Virunga_63spp$mean$beta1[50]  * elev + HCDSM_Virunga_63spp$mean$beta2[50] * elev^2)
Sppz50c <- Sppz50/max(Sppz50)
Sppz51 <-  exp(HCDSM_Virunga_63spp$mean$beta0[51]  + HCDSM_Virunga_63spp$mean$beta1[51]  * elev + HCDSM_Virunga_63spp$mean$beta2[51] * elev^2)
Sppz51c <- Sppz51/max(Sppz51)
Sppz52 <-  exp(HCDSM_Virunga_63spp$mean$beta0[52]  + HCDSM_Virunga_63spp$mean$beta1[52]  * elev + HCDSM_Virunga_63spp$mean$beta2[52] * elev^2)
Sppz52c <- Sppz52/max(Sppz52)
Sppz53 <-  exp(HCDSM_Virunga_63spp$mean$beta0[53]  + HCDSM_Virunga_63spp$mean$beta1[53]  * elev + HCDSM_Virunga_63spp$mean$beta2[53] * elev^2)
Sppz53c <- Sppz53/max(Sppz53)
Sppz54 <-  exp(HCDSM_Virunga_63spp$mean$beta0[54]  + HCDSM_Virunga_63spp$mean$beta1[54]  * elev + HCDSM_Virunga_63spp$mean$beta2[54] * elev^2)
Sppz54c <- Sppz54/max(Sppz54)
Sppz55 <-  exp(HCDSM_Virunga_63spp$mean$beta0[55]  + HCDSM_Virunga_63spp$mean$beta1[55]  * elev + HCDSM_Virunga_63spp$mean$beta2[55] * elev^2)
Sppz55c <- Sppz55/max(Sppz55)
Sppz56 <-  exp(HCDSM_Virunga_63spp$mean$beta0[56]  + HCDSM_Virunga_63spp$mean$beta1[56]  * elev + HCDSM_Virunga_63spp$mean$beta2[56] * elev^2)
Sppz56c <- Sppz56/max(Sppz56)
Sppz57 <-  exp(HCDSM_Virunga_63spp$mean$beta0[57]  + HCDSM_Virunga_63spp$mean$beta1[57]  * elev + HCDSM_Virunga_63spp$mean$beta2[57] * elev^2)
Sppz57c <- Sppz57/max(Sppz57)
Sppz58 <-  exp(HCDSM_Virunga_63spp$mean$beta0[58]  + HCDSM_Virunga_63spp$mean$beta1[58]  * elev + HCDSM_Virunga_63spp$mean$beta2[58] * elev^2)
Sppz58c <- Sppz58/max(Sppz58)
Sppz59 <-  exp(HCDSM_Virunga_63spp$mean$beta0[59]  + HCDSM_Virunga_63spp$mean$beta1[59]  * elev + HCDSM_Virunga_63spp$mean$beta2[59] * elev^2)
Sppz59c <- Sppz59/max(Sppz59)
Sppz60 <-  exp(HCDSM_Virunga_63spp$mean$beta0[60]  + HCDSM_Virunga_63spp$mean$beta1[60]  * elev + HCDSM_Virunga_63spp$mean$beta2[60] * elev^2)
Sppz60c <- Sppz60/max(Sppz60)
Sppz61 <-  exp(HCDSM_Virunga_63spp$mean$beta0[61]  + HCDSM_Virunga_63spp$mean$beta1[61]  * elev + HCDSM_Virunga_63spp$mean$beta2[61] * elev^2)
Sppz61c <- Sppz61/max(Sppz61)
Sppz62 <-  exp(HCDSM_Virunga_63spp$mean$beta0[62]  + HCDSM_Virunga_63spp$mean$beta1[62]  * elev + HCDSM_Virunga_63spp$mean$beta2[62] * elev^2)
Sppz62c <- Sppz62/max(Sppz62)
Sppz63 <-  exp(HCDSM_Virunga_63spp$mean$beta0[63]  + HCDSM_Virunga_63spp$mean$beta1[63]  * elev + HCDSM_Virunga_63spp$mean$beta2[63] * elev^2)
Sppz63c <- Sppz63/max(Sppz63)




###  Community_level ##
###Standardized########
Community <-  (exp(HCDSM_Virunga_63spp$mean$mu_a  + HCDSM_Virunga_63spp$mean$mu_b * elev + HCDSM_Virunga_63spp$mean$mu_q* elev^2))
Communityc <- Community /max(Community)
lower <-  (exp(HCDSM_Virunga_63spp$q2.5$mu_a  + HCDSM_Virunga_63spp$q2.5$mu_b * elev + HCDSM_Virunga_63spp$q2.5$mu_q* elev^2))
lowerc <- lower/max(Community)
upper <-  (exp(HCDSM_Virunga_63spp$q97.5$mu_a  + HCDSM_Virunga_63spp$q97.5$mu_b * elev + HCDSM_Virunga_63spp$q97.5$mu_q* elev^2))
upperc <- upper/max(Community)

##### Creat a dataframe
All_spp_standardized <- data.frame(Communityc,lowerc, upperc, Sppz1c,Sppz2c,Sppz3c,Sppz4c,Sppz5c,Sppz6c,Sppz7c,Sppz8c,Sppz9c,Sppz10c,Sppz11c,Sppz12c,Sppz13c,Sppz14c,Sppz15c,Sppz16c,Sppz17c,Sppz18c,Sppz19c,
                      Sppz20c,Sppz21c,Sppz22c,Sppz23c,Sppz24c,Sppz25c,Sppz26c,Sppz27c,Sppz28c,Sppz29c,Sppz30c,Sppz31c,Sppz32c,Sppz33c,Sppz34c,Sppz35c,Sppz36c,Sppz37c,Sppz38c,Sppz39c,Sppz40c,
                      Sppz41c,Sppz42c,Sppz43c,Sppz44c,Sppz45c,Sppz46c,Sppz47c,Sppz48c,Sppz49c,Sppz50c,Sppz51c,Sppz52c,Sppz53c,Sppz54c,Sppz55c,Sppz56c,Sppz57c,Sppz58c,Sppz59c,Sppz60c,Sppz61c,
                      Sppz62c,Sppz63c,Sppz1c,elev2 )




# Plot
Figure2_c = ggplot (All_spp_standardized) +
  geom_line (aes(x=elev2 , y= Communityc, color ="blue"), size =3) +
  geom_line (aes(x=elev2 , y= Sppz1c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz2c, color ="black"), size =0.8) + 
  geom_line (aes(x=elev2 , y= Sppz3c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz4c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz5c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz6c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz7c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz8c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz9c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz10c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz11c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz12c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz13c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz14c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz15c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz16c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz17c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz18c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz19c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz20c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz21c, color ="black"), size =0.5) +
  geom_line (aes(x=elev2 , y= Sppz22c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz23c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz24c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz25c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz26c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz27c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz28c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz29c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz30c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz31c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz32c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz33c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz34c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz35c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz36c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz37c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz38c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz39c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz40c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz41c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz42c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz43c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz44c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz45c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz46c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz47c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz48c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz49c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz50c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz51c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz52c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz53c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz54c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz55c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz56c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz57c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz58c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz59c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz60c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz61c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz62c, color ="black"), size =0.8) +
  geom_line (aes(x=elev2 , y= Sppz63c, color ="black"), size =0.8) +
  
  
  annotate("text", x = 2700, y = 1.4, hjust = 0, size = 8, family = "Times New Roman", 
           label = "", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1.0))
 
Figure2_c  + labs (x="Elevation(m)", y ="Expected abundance (standardized)",fontface= "plain") +
  scale_color_manual(labels = c("", ""), 
                     values=c(  "blue"="blue", "black"="black")) +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25,color = "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25,color = "black"),
        axis.title.y = element_text(size = rel(1.8), angle = 90),
        axis.title.x = element_text(size = rel(1.8), angle = 00),
        legend.text=element_text(size=25),
        legend.position = "none", legend.title=element_blank())

ggsave(file = "Figure2(c).jpg", bg = NULL, dpi = 200, width = 15, height = 10)



######################### Family: Musophagidae -- (Turacos)##################
##########------------------------- 2 species ------------------------#######
#############################################################################
######################### Species abundance-Elevation curves ################


###### Ruwenzorornis johnstoni ######
#####################################

#### Expected abundance
Abundance29 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[29]  + HCDSM_Virunga_63spp$mean$beta1[29]  * elev + HCDSM_Virunga_63spp$mean$beta2[29] * elev^2))
Ab29mx <- max(Abundance29) # Maximum abundance
Abundance29s <- Abundance29/Ab29mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l29 <-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[29]  + HCDSM_Virunga_63spp$q2.5$beta1[29] * elev + HCDSM_Virunga_63spp$q2.5$beta2[29]* elev^2))
l29s <- l29/Ab29mx

##### Upper 97.5 CI
u29 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[29]  + HCDSM_Virunga_63spp$q97.5$beta1[29] * elev + HCDSM_Virunga_63spp$q97.5$beta2[29]* elev^2))
u29s <- u29/Ab29mx

#### Create a dataframe
spp29 <- data.frame (Abundance29, Abundance29s, elev2 , l29, l29s, u29, u29s)


###### Tauraco schuettii######
##############################

#### Expected abundance
Abundance30 <- (exp(HCDSM_Virunga_63spp$mean$beta0[30]  + HCDSM_Virunga_63spp$mean$beta1[30]  * elev + HCDSM_Virunga_63spp$mean$beta2[30] * elev^2))
Ab30mx <- max(Abundance30) # # Maximum abundance
Abundance30s <- Abundance30/Ab30mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l30<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[30]  + HCDSM_Virunga_63spp$q2.5$beta1[30] * elev + HCDSM_Virunga_63spp$q2.5$beta2[30]* elev^2))
l30s <- l30/Ab30mx

##### Upper 97.5 CI
u30 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[30]  + HCDSM_Virunga_63spp$q97.5$beta1[30] * elev + HCDSM_Virunga_63spp$q97.5$beta2[30]* elev^2))
u30s <- u30/Ab30mx

#### Create a dataframe
spp30 <- data.frame (Abundance30, Abundance30s, elev2 , l30, l30s, u30, u30s)





###### Elevational niche overlap- Estimation ################
#########Family: Musophagidae -- (Turacos) ##################
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
##### same relative scale (maximum abundance = 1)############
#############################################################





######################################################
#### Integration function: Ruwenzorornis johnstoni ###
#####################################################

inter29 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[29]  + 
                              HCDSM_Virunga_63spp$mean$beta1[29]  * x + HCDSM_Virunga_63spp$mean$beta2[29] * x^2)/Ab29mx}


# Area under the curve: Ruwenzorornis johnstoni
Area_spp29 <- integrate(inter29, -2.24,3.16) 
Area_spp29 

######################################################
#### Integration function: Tauraco schuettii ###
#####################################################


inter30 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[30]  + 
                              HCDSM_Virunga_63spp$mean$beta1[30]  * x + HCDSM_Virunga_63spp$mean$beta2[30] * x^2)/Ab30mx}

# Area under the curve: Tauraco schuettii 
Area_spp30 <- integrate(inter30, -2.24,3.16) 
Area_spp30



#######################################################
###### Area of overlap along the elevation gradient####
#######################################################


##  Ruwenzorornis johnstoni & Tauraco schuettii 
x <- elev  ## Standardized elevation
y29 <- Abundance29s  ## Standardized abundance: Ruwenzorornis johnstoni 
y30 <- Abundance30s ## Standardized abundance: Tauraco schuettii 

### Estimating area of overlap
spps29n30 <- approxfun(x, pmin(y29,y30), ties = "mean") ### Applying approxfun to estimate
integrate(spps29n30, min(x), max(x))



########### Response curves: Turacos###
####Co-abundance variation
####Ruwenzorornis johnstoni and Tauraco schuettii 
#########--------------------##########
###### Figure 3a


Figure3_a = ggplot () +
  geom_line (data=spp29, aes(x=elev2 , y= Abundance29s, color ="blue"), size =2) +
  geom_ribbon(data=spp29,aes(x=elev2 , y= Abundance29s, ymin = l29s, ymax = u29s), alpha = 0.5, fill= "blue")+
  geom_line (data=spp30, aes(x=elev2 , y= Abundance30s, color = "#D55E00"), size =2) +
  geom_ribbon(data=spp30,aes(x=elev2 , y= Abundance30s, ymin = l30s, ymax = u30s), alpha = 0.5, fill= "#D55E00") +
  
  annotate("text", x = 2750, y = 8, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Musophagidae", fontface= "plain") +
  
  annotate("text", x = 2000, y = 5.5, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: elevation = 0.08", colour = "#D55E00", fontface= "plain") +
  
  annotate("text", x = 2000, y = 5, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: elevation = 0.03", colour = "blue", fontface= "plain") +
  
  annotate("text", x = 2000, y = 4.5, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: diet = 0.98", colour = "black", fontface= "plain") +
  
  annotate("text", x = 2000, y = 4, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: strata = 0.91", colour = "black", fontface= "plain") +
  
  annotate("text", x = 2000, y = 3.5, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Activity pattern = diurnal", colour = "black", fontface= "plain") 

coord_cartesian(ylim = c(0, 8))

Figure3_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Tauraco schuettii (235g)","Ruwenzorornis johnstoni (240g)"), 
                     values=c( "#D55E00"= "#D55E00", "blue"="blue"), name = "Species") +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 30),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=30, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=30, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=30,face="italic"),
        legend.position = c(c(0.3,0.8)), legend.title=element_text(size=30, color="black")) 





ggsave(file = "Figure3(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S1 (a)
##Standardized
FigureS1_a = ggplot () +
  geom_line (data=spp29, aes(x=elev2 , y= Abundance29s, color ="orange"), size =2) +
  geom_line (data=spp30, aes(x=elev2 , y= Abundance30s, color ="blue"), size =2) +
  
  
  
  annotate("text", x = 2500, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Musophagidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1))

FigureS1_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Ruwenzorornis johnstoni (240g)", "Tauraco schuettii (235g)"), 
                     values=c( "orange"= "orange", "blue"="blue"), name = "Species") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.29,0.82)), legend.title=element_text(size=25, color="black"))   




ggsave(file = "Appendix_S8_FigureS1(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S1 (b) 
## Confidence intervals

FigureS1_b = ggplot () +
  geom_line (data=spp29, aes(x=elev2 , y= Abundance29, color ="orange"), size =2) +
  geom_ribbon(data=spp29,aes(x=elev2 , y= Abundance29, ymin = l29, ymax = u29), alpha = 0.5, fill= "orange")+
  geom_line (data=spp30, aes(x=elev2 , y= Abundance30, color = "blue"), size =2) +
  geom_ribbon(data=spp30,aes(x=elev2 , y= Abundance30, ymin = l30, ymax = u30), alpha = 0.5, fill= "blue") +
  
  annotate("text", x = 2750, y = 6, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Musophagidae", fontface= "plain") +

coord_cartesian(ylim = c(0, 6))

FigureS1_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Ruwenzorornis johnstoni (240g)","Tauraco schuettii (235g)"), 
                     values=c( "orange"= "orange", "blue"="blue"), name = "Species") +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.3,0.8)), legend.title=element_text(size=25, color="black")) 





ggsave(file = "Appendix_S8_FigureS1(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


##################### Turacos ############
######## Foraging diet categories########
#########################################

## Appendix S8: FigureS1(c)
#### Plot diet categories - diet

ggplot(Turacos_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "orange", "brown")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Musophigidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  
  
  scale_x_discrete(labels = c("Tauraco schuetti" = "Tauraco\nschuetti","Ruwenzorornis johnstoni" = "Ruwenzorornis\njohnstoni"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 





ggsave(file = "Appendix_S8_FigureS1(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##################### Turacos ############
######## Foraging forest strata########
#########################################

## ## Appendix S8: FigureS1(d)
## Plot foraging forest strata- Turacos
ggplot(Turacos_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange", "purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Musophagidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Tauraco schuetti" = "Tauraco\nschuetti","Ruwenzorornis johnstoni" = "Ruwenzorornis\njohnstoni"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5,face = "plain")) 





ggsave(file = "Appendix_S8_FigureS1(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




###################### Family: Nectariniidae -- (Sunbirds)############
########-------------------- 6 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################


###### Cinnyris regius#######
#############################

###### Expected abundance 
Abundance31 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[31]  + HCDSM_Virunga_63spp$mean$beta1[31]  * elev + HCDSM_Virunga_63spp$mean$beta2[31] * elev^2))
Ab31mx <- max(Abundance31) ## Maximum abundance
Abundance31s <- Abundance31/Ab31mx


###### 95% credible interval (CI)

##### Lower 2.5 CI
l31<-   (exp(HCDSM_Virunga_63spp$q2.5$beta0[31]  + HCDSM_Virunga_63spp$q2.5$beta1[31] * elev + HCDSM_Virunga_63spp$q2.5$beta2[31]* elev^2))
l31s <- l31/Ab31mx

##### Upper 97.5 CI
u31 <-   (exp(HCDSM_Virunga_63spp$q97.5$beta0[31]  + HCDSM_Virunga_63spp$q97.5$beta1[31] * elev + HCDSM_Virunga_63spp$q97.5$beta2[31]* elev^2))
u31s <- u31/Ab31mx

#### Create a dataframe
spp31 <- data.frame (Abundance31, Abundance31s, elev2 , l31, l31s, u31, u31s)


###### Cinnyris stuhlmanni#######
#################################

###### Expected abundance 
Abundance32 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[32]  + HCDSM_Virunga_63spp$mean$beta1[32]  * elev + HCDSM_Virunga_63spp$mean$beta2[32] * elev^2))
Ab32mx <- max(Abundance32)
Abundance32s <- Abundance32/Ab32mx


###### 95% credible interval (CI)

##### Lower 2.5 CI
l32<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[32]  + HCDSM_Virunga_63spp$q2.5$beta1[32] * elev + HCDSM_Virunga_63spp$q2.5$beta2[32]* elev^2))
l32s <- l32/Ab32mx

##### Upper 97.5 CI
u32 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[32]  + HCDSM_Virunga_63spp$q97.5$beta1[32] * elev + HCDSM_Virunga_63spp$q97.5$beta2[32]* elev^2))
u32s <- u32/Ab32mx

#### Create a dataframe
spp32 <- data.frame (Abundance32, Abundance32s, elev2 ,l32, l32s, u32, u32s)



###### Cinnyris venustus ######
##############################


###### Expected abundance 
Abundance33 <- (exp(HCDSM_Virunga_63spp$mean$beta0[33]  + HCDSM_Virunga_63spp$mean$beta1[33]  * elev + HCDSM_Virunga_63spp$mean$beta2[33] * elev^2))
Ab33mx <- max(Abundance33) ### Maximum abundance
Abundance33s <- Abundance33/Ab33mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l33<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[33]  + HCDSM_Virunga_63spp$q2.5$beta1[33] * elev + HCDSM_Virunga_63spp$q2.5$beta2[33]* elev^2))
l33s <- l33/Ab33mx


##### Upper 97.5 CI
u33 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[33]  + HCDSM_Virunga_63spp$q97.5$beta1[33] * elev + HCDSM_Virunga_63spp$q97.5$beta2[33]* elev^2))
u33s <- u33/Ab33mx

###### create a data frame
spp33 <- data.frame (Abundance31, Abundance31s, elev2 , l33, l33s, u33, u33s)



###### Cyanomitra alinae######
##############################

###### Expected abundance 
Abundance34 <- (exp(HCDSM_Virunga_63spp$mean$beta0[34]  + HCDSM_Virunga_63spp$mean$beta1[34]  * elev + HCDSM_Virunga_63spp$mean$beta2[34] * elev^2))
Ab34mx <- max(Abundance34) ### Maximum abundance
Abundance34s <- Abundance34/Ab34mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l34<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[34]  + HCDSM_Virunga_63spp$q2.5$beta1[34] * elev + HCDSM_Virunga_63spp$q2.5$beta2[34]* elev^2))
l34s <- l34/Ab34mx

##### Upper 97.5 CI
u34 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[34]  + HCDSM_Virunga_63spp$q97.5$beta1[34] * elev + HCDSM_Virunga_63spp$q97.5$beta2[34]* elev^2))
u34s <- u34/Ab34mx


###### create a data frame
spp34 <- data.frame (Abundance34, Abundance34s, elev2, l34, l34s, u34, u34s)




###### Hedydipna collaris######
##############################

###### Expected abundance 
Abundance35 <- (exp(HCDSM_Virunga_63spp$mean$beta0[35]  + HCDSM_Virunga_63spp$mean$beta1[35]  * elev + HCDSM_Virunga_63spp$mean$beta2[35] * elev^2))
Ab35mx <- max(Abundance35)  ### Maximum abundance
Abundance35s <- Abundance35/Ab35mx


###### 95% credible interval (CI)

##### Lower 2.5 CI
l35<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[35]  + HCDSM_Virunga_63spp$q2.5$beta1[35] * elev + HCDSM_Virunga_63spp$q2.5$beta2[35]* elev^2))
l35s <- l35/Ab35mx


##### Upper 97.5 CI
u35 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[35]  + HCDSM_Virunga_63spp$q97.5$beta1[35] * elev + HCDSM_Virunga_63spp$q97.5$beta2[35]* elev^2))
u35s <- u35/Ab35mx

###### create a data frame
spp35 <- data.frame (Abundance35, Abundance35s, elev2 , l35, l35s, u35, u35s)



###### Nectarinia johnstoni######
##############################

###### Expected abundance 
Abundance36 <- (exp(HCDSM_Virunga_63spp$mean$beta0[36]  + HCDSM_Virunga_63spp$mean$beta1[36]  * elev + HCDSM_Virunga_63spp$mean$beta2[36] * elev^2))
Ab36mx <- max(Abundance36) ### Maximum abundance
Abundance36s <- Abundance36/Ab36mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l36<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[36]  + HCDSM_Virunga_63spp$q2.5$beta1[36] * elev + HCDSM_Virunga_63spp$q2.5$beta2[36]* elev^2))
l36s <- l36/Ab36mx

##### Upper 97.5 CI
u36 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[36]  + HCDSM_Virunga_63spp$q97.5$beta1[36] * elev + HCDSM_Virunga_63spp$q97.5$beta2[36]* elev^2))
u36s <- u36/Ab36mx


###### create a data frame
spp36 <- data.frame (Abundance36, Abundance36s, elev2 , l36, l36s, u36, u36s)







###### Elevational niche overlap- Estimation ################
#########Family: Nectariniidae -- (Sunbirds) ################
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
#####same relative scale (maximum abundance = 1)#############
#############################################################


######################################################
#### Integration function: Cinnyris regius###########
#####################################################

inter31 <- function (x){ (exp(HCDSM_Virunga_63spp$mean$beta0[31]  + 
                                HCDSM_Virunga_63spp$mean$beta1[31]  * x + HCDSM_Virunga_63spp$mean$beta2[31] * x^2))/Ab31mx}

### Area under the curve: Cinnyris regius
Area_spp31 <- integrate(inter31, -2.24,3.16)
Area_spp31 

######################################################
#### Integration function: Cinnyris Stuhlmanni########
########################################################

inter32 <- function (x){(exp(HCDSM_Virunga_63spp$mean$beta0[32]  + 
                               HCDSM_Virunga_63spp$mean$beta1[32]  * x + HCDSM_Virunga_63spp$mean$beta2[32] * x^2))/Ab32mx}

# Area under the curve: Cinnyris Stuhlmanni

Area_spp32 <- integrate(inter32, -2.24,3.16)
Area_spp32


######################################################
#### Integration function: Cinnyris venustus###########
#####################################################

inter33 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[33]  + 
                              HCDSM_Virunga_63spp$mean$beta1[33]  * x + HCDSM_Virunga_63spp$mean$beta2[33] * x^2)/Ab33mx}

# Area under the curve: Cinnyris Stuhlmanni
Area_spp33 <- integrate(inter33, -2.24,3.16)
Area_spp33


######################################################
#### Integration function: Cyanomitra alinae###########
#####################################################

inter34 <- function (x){(exp(HCDSM_Virunga_63spp$mean$beta0[34]  + 
                               HCDSM_Virunga_63spp$mean$beta1[34]  * x + HCDSM_Virunga_63spp$mean$beta2[34] * x^2))/Ab34mx}

### Area under the curve: Cyanomitra alinae
Area_spp34 <- integrate(inter34, -2.24,3.16)
Area_spp34


######################################################
#### Integration function: Hedydipna collaris###########
#####################################################

inter35 <- function (x){(exp(HCDSM_Virunga_63spp$mean$beta0[35]  + 
                               HCDSM_Virunga_63spp$mean$beta1[35]  * x + HCDSM_Virunga_63spp$mean$beta2[35] * x^2))/Ab35mx}

### Area under the curve: Hedydipna collaris
Area_spp35 <- integrate(inter35, -2.24,3.16)
Area_spp35 

######################################################
#### Integration function: Nectarinia johnstoni###########
#####################################################

inter36 <- function (x){(exp(HCDSM_Virunga_63spp$mean$beta0[36]  + 
                               HCDSM_Virunga_63spp$mean$beta1[36]  * x + HCDSM_Virunga_63spp$mean$beta2[36] * x^2))/Ab36mx}

### Area under the curve: Nectarinia johnstoni
Area_spp36 <- integrate(inter36, -2.24,3.16)
Area_spp36


#######################################################
###### Area of overlap along an elevation gradient####
#######################################################

## Cinnyris regius & Cinnyris Stuhlmanni#####
####-------------------------------------#####

x <- elev  ## Standardized elevation  
y1 <- Abundance31s ## Standardized abundance: Cinnyris regius 
y2 <- Abundance32s ## Standardized abundance: Cinnyris Stuhlmanni
### Computing area of overlap
spps31n32 <- approxfun(x, pmin(y1,y2), ties = "mean") 
integrate(spps31n32, min(x), max(x))


## Cinnyris regius & Cinnyris venustus
####-------------------------------------#####

x <- elev ## Standardized elevation  
y1 <- Abundance31s ## Standardized abundance: Cinnyris regius 
y3 <- Abundance33s ## Standardized abundance: Cinnyris venustus
### Computing area of overlap
spps31n33  <- approxfun(x, pmin(y1,y3), ties = "mean")
integrate(spps31n33 , min(x), max(x))



## Cinnyris regius & Cyanomitra alinae
####-------------------------------------#####
x <- elev  ## Standardized elevation 
y1 <- Abundance31s ## Standardized abundance: Cinnyris regius 
y4 <- Abundance34s ## Standardized abundance: Cyanomitra alinae
### Computing area of overlap
spps31n34 <- approxfun(x, pmin(y1,y4), ties = "mean")
integrate(spps31n34, min(x), max(x))



## Cinnyris regius & Hedydipna collaris
####-------------------------------------#####

x <- elev   ## Standardized elevation 
y1 <- Abundance31s  ## Standardized abundance: Cinnyris regius 
y5 <- Abundance35s  ## Standardized abundance:  Hedydipna collaris
### Computing area of overlap
spps31n35 <- approxfun(x, pmin(y1,y5), ties = "mean")
integrate(spps31n35, min(x), max(x))


## Cinnyris regius & Nectarinia johnstoni
####-------------------------------------#####
x <- elev   ## Standardized elevation 
y1 <- Abundance31s  ## Standardized abundance: Cinnyris regius 
y6 <- Abundance36s  ## Standardized abundance:  Nectarinia johnstoni
### Computing area of overlap
spps31n36  <- approxfun(x, pmin(y1,y6), ties = "mean")
integrate(spps31n36, min(x), max(x))



## Cinnyris stuhlmanni	Cinnyrisvenustus
####-------------------------------------#####
x <- elev  ## Standardized elevation 
y2 <- Abundance32s ## Standardized abundance: Cinnyris Stuhlmanni
y3 <- Abundance33s ## Standardized abundance: Cinnyris venustus
### Computing area of overlap
spps32n33 <- approxfun(x, pmin(y2,y3), ties = "mean")
integrate(spps32n33, min(x), max(x),subdivisions = 2000)


## Cinnyris Stuhlmanni & Cyanomitra alinae
####-------------------------------------#####
x <- elev ## Standardized elevation 
y2 <- Abundance32s  ## Standardized abundance: Cinnyris Stuhlmanni
y4 <- Abundance34s  ## Standardized abundance: Cyanomitra alinae
### Computing area of overlap
spps32n34 <- approxfun(x, pmin(y2,y4), ties = "mean")
integrate(spps32n34, min(x), max(x),subdivisions = 2000)


## Cinnyris Stuhlmanni & Hedydipna collaris
####-------------------------------------#####
x <- elev  ## Standardized elevation 
y2 <- Abundance32s ## Standardized abundance: Cinnyris Stuhlmanni
y5 <- Abundance35s  ## Standardized abundance: Hedydipna collaris
### Computing area of overlap
spps32n35 <- approxfun(x, pmin(y2,y5), ties = "mean")
integrate(spps32n35, min(x), max(x),subdivisions = 2000)


## Cinnyris Stuhlmanni & Nectarinia johnstoni
####-------------------------------------#####
x <- elev ## Standardized elevation 
y2 <- Abundance32s ## Standardized abundance: Cinnyris regius 
y6 <- Abundance36s  ## Standardized abundance: Nectarinia johnstoni
spps32n36 <- approxfun(x, pmin(y2,y6),ties = "mean")
### Computing area of overlap
integrate(spps32n36, min(x), max(x),subdivisions = 2000)


## Cinnyris venustus & Cyanomitra alinae
####-----------------------------------#####
x <- elev ## Standardized elevation 
y3 <- Abundance33s ## Standardized abundance: Cinnyris venustus
y4 <- Abundance34s ## Standardized abundance: Cyanomitra alinae
### Computing area of overlap
spps32n34 <- approxfun(x, pmin(y3,y4), ties = "mean")
integrate(spps32n34, min(x), max(x))


## Cinnyris venustus & Hedydipna collaris
####-------------------------------------#####
x <- elev ## Standardized elevation 
y3 <- Abundance33s ## Standardized abundance: Cinnyris venustus 
y5 <- Abundance35s  ## Standardized abundance: Hedydipna collaris
### Computing area of overlap
spps33n35 <- approxfun(x, pmin(y3,y5), ties = "mean")
integrate(spps33n35, min(x), max(x))


## Cinnyris venustus & Nectarinia johnstoni
####-------------------------------------#####
x <- elev ## Standardized elevation 
y3 <- Abundance33s  ## Standardized abundance: Cinnyris venustus 
y6 <- Abundance36s  ## Standardized abundance: Nectarinia johnstoni
### Computing area of overlap
spps33n36 <- approxfun(x, pmin(y3,y6), ties = "mean")
integrate(spps33n36, min(x), max(x))


##  Cyanomitra alinae & Hedydipna collaris
####-------------------------------------#####
x <- elev  ## Standardized elevation 
y4 <- Abundance34s ## Standardized abundance: Cyanomitra alinae 
y5 <- Abundance35s ## Standardized abundance: Hedydipna collaris 
### Computing area of overlap
spps34n35 <- approxfun(x, pmin(y4,y5), ties = "mean")
integrate(spps34n35, min(x), max(x),subdivisions = 2000) # Note there appears to be some numerical errors in the overlap calculations
# Area of overlap is greater than 1 in cases where there is complete overlap. This
# is fine


##  Cyanomitra alinae & Nectarinia johnstoni
####---------------------------------------#####
x <- elev ## Standardized elevation 
y4 <- Abundance34s ## Standardized abundance: Cyanomitra alinae 
y6 <- Abundance36s ## Standardized abundance: Nectarinia johnstoni
### Computing area of overlap
spps34n36 <- approxfun(x, pmin(y4,y6), ties = "mean")
integrate(spps34n36, min(x), max(x),subdivisions = 2000)


##  Hedydipna collaris & Nectarinia johnstoni
####----------------------------------------#####
x <- elev ## Standardized elevation 
y5 <- Abundance35s ## Standardized abundance: Hedydipna collaris 
y6 <- Abundance36s ## Standardized abundance: Nectarinia johnstoni
### Computing area of overlap
spps35n36 <- approxfun(x, pmin(y5,y6), ties = "mean")
integrate(spps35n36, min(x), max(x),subdivisions = 2000)



########### Response curves: Sunbirds###
#########--------------------##########
####Co-abundance variation
####Cinnyris regius and Cinnyris Stuhlmanni 
###### Figure 3b


Figure_3b = ggplot () +
  geom_line (data=spp31, aes(x=elev2 , y= Abundance31s, color = "#D55E00"), size =2) +
  geom_ribbon(data=spp31,aes(x=elev2 , y= Abundance31s, ymin = l31s, ymax = u31s), alpha = 0.5, fill= "#D55E00") +
  geom_line (data=spp32, aes(x=elev2 , y= Abundance32s, color ="blue"), size =2) +
  geom_ribbon(data=spp32, aes(x=elev2 , y= Abundance32s, ymin = l32s, ymax = u32s), alpha = 0.5, fill = "blue") +
  
  
  annotate("text", x = 2500, y = 6, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Nectariniidae", fontface= "plain") +
  
  annotate("text", x = 1950, y = 4, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: elevation = 0.26", colour = "#D55E00", fontface= "plain") +
  
  annotate("text", x = 1950, y = 3.65, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: elevation = 0.25", colour = "blue", fontface= "plain") +
  
  annotate("text", x = 1950, y = 3.25, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: diet = 1", colour = "black", fontface= "plain")+
  
  annotate("text", x = 1950, y = 2.9, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: strata = 0.74", colour = "black", fontface= "plain")+
  
  annotate("text", x = 1950, y = 2.55, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Activity pattern = diurnal", colour = "black", fontface= "plain") 


Figure_3b  + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Cinnyris regius (6.6g)", "Cinnyris stuhlmanni (8.5g)" ), #
                     values=c( "#D55E00"= "#D55E00", "blue"="blue") , name = "Species") +  ## "orange"= "orange"
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 30),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=30, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=30, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=30,face="italic"),
        legend.position = c(0.25,0.8), legend.title=element_text(size=30, color="black")) 




ggsave(file = "Figure3(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S2 (a)
##Standardized


FigureS2_a = ggplot () +
  geom_line (data=spp31, aes(x=elev2 , y= Abundance31s, color ="red"), size =2) +
  geom_line (data=spp32, aes(x=elev2 , y= Abundance32s, color ="blue"), size =2) +
  geom_line (data=spp33, aes(x=elev2 , y= Abundance33s, color ="purple"), size =2) +
  geom_line (data=spp34, aes(x=elev2 , y= Abundance34s, color ="black"), size =2) +
  geom_line (data=spp35, aes(x=elev2 , y= Abundance35s, color ="grey"), size =2) +
  geom_line (data=spp36, aes(x=elev2 , y= Abundance36s, color ="orange"), size =2) +
  
  
  annotate("text", x = 2700, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Nectariniidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1.0))

FigureS2_a  + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Cinnyris regius (6.58g)", "Cinnyris stuhlmanni (8.5g)","Cinnyris venustus (6.56g)", 
                                "Cyanomitra alinae (12.56g)", "Hedydipna collaris (6.98g)", "Nectarinia johnstoni (15.16g)"), 
                     values=c( "red"= "red", "blue"="blue", "purple"= "purple", "black"="black", "grey"= "grey", 
                                     "orange"= "orange"),name = "Species") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=22,face="italic"),
        legend.position = c(0.825,0.27), legend.title=element_text(size=22, color="black"))



ggsave(file = "Appendix_S8_FigureS2(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S2 (b)
## Confidence intervals

FigureS2_b = ggplot () +
  geom_line (data=spp31, aes(x=elev2 , y= Abundance31, color ="red"), size =2) +
  geom_ribbon(data=spp31,aes(x=elev2 , y= Abundance31, ymin = l31, ymax = u31), alpha = 0.5, fill= "red") +
  geom_line (data=spp32, aes(x=elev2 , y= Abundance32, color ="blue"), size =2) +
  geom_ribbon(data=spp32, aes(x=elev2, y= Abundance32, ymin = l32, ymax = u32), alpha = 0.5, fill = "blue") +
  geom_line (data=spp33, aes(x=elev2 , y= Abundance33, color ="purple"), size =2) +
  geom_ribbon(data=spp33,aes(x=elev2 , y= Abundance33, ymin = l33, ymax = u33), alpha = 0.5, fill = "purple") +
  geom_line (data=spp34, aes(x=elev2 , y= Abundance34, color ="black"), size =2) +
  geom_ribbon(data=spp34,aes(x=elev2 , y= Abundance34, ymin = l34, ymax = u34), alpha = 0.7, fill = "black") +
  geom_line (data=spp35, aes(x=elev2 , y= Abundance35, color ="grey"), size =2) +
  geom_ribbon(data=spp35,aes(x=elev2 , y= Abundance35, ymin = l35, ymax = u35), alpha = 0.7, fill = "grey") +
  geom_line (data=spp36, aes(x=elev2 , y= Abundance36, color ="orange"), size =2) +
  geom_ribbon(data=spp36,aes(x=elev2 , y= Abundance36, ymin = l36, ymax = u36), alpha = 0.5, fill = "orange") +
  
  annotate("text", x = 2700, y = 150, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Nectariniidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 150))

FigureS2_b  + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Cinnyris regius (6.58g)", "Cinnyris stuhlmanni (8.5g)","Cinnyris venustus (6.56g)", 
                                "Cyanomitra alinae (12.56g)", "Hedydipna collaris (6.98g)", "Nectarinia johnstoni (15.16g)"), 
                     values=c( "red"= "red", "blue"="blue", "purple"= "purple", "black"="black", "grey"= "grey", 
                                     "orange"= "orange"),name = "Species") +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(0.3,0.7), legend.title=element_text(size=25, color="black"))  




ggsave(file = "Appendix_S8_FigureS2(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


########Nectariniidae -- (Sunbirds) ######
######## Foraging diet categories########
#########################################

##Appendix S8: Figure S2 (c)
ggplot(Sunbirds_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange", "purple")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Nectariniidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Cinnyris stuhlmanni" = "Cinnyris\nstuhlmanni","Cinnyris regius" = "Cinnyris\nregius", 
                              "Cinnyris venustus" = "Cinnyris\nvenustus", "Cyanomitra alinae" = "Cyanomitra\nalinae",
                              "Hedydipna collaris" = "Hedydipna\ncollaris", "Nectarinia johnstoni" = "Nectarinia\njohnstoni")) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face= "plain")) 





ggsave(file = "Appendix_S8_FigureS2(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



########Nectariniidae -- (Sunbirds) ######
######## Foraging forest strata########
#########################################

##Appendix S8: Figure S2 (d)
ggplot(Sunbirds_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange", "purple","brown")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Nectariniidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Cinnyris stuhlmanni" = "Cinnyris\nstuhlmanni","Cinnyris regius" = "Cinnyris\nregius", 
                              "Cinnyris venustus" = "Cinnyris\nvenustus", "Cyanomitra alinae" = "Cyanomitra\nalinae",
                              "Hedydipna collaris" = "Hedydipna\ncollaris", "Nectarinia johnstoni" = "Nectarinia\njohnstoni")) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face= "plain")) 


ggsave(file = "Appendix_S8_FigureS2(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)






########################################################################
###################### Family: Lybiidae  -- (Barbets) ##############
########-------------------- 3 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################



###### Pogoniulus bilineatus #######
###################################


###### Expected abundance 
Abundance3 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[3]  + HCDSM_Virunga_63spp$mean$beta1[3]  * elev + HCDSM_Virunga_63spp$mean$beta2[3] * elev^2))
Ab3mx <- max(Abundance3)
Abundance3s <- Abundance3/Ab3mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l3<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[3]  + HCDSM_Virunga_63spp$q2.5$beta1[3] * elev + HCDSM_Virunga_63spp$q2.5$beta2[3]* elev^2))
l3s <- l3/Ab3mx

u3 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[3]  + HCDSM_Virunga_63spp$q97.5$beta1[3] * elev + HCDSM_Virunga_63spp$q97.5$beta2[3]* elev^2))
u3s <- u3/Ab3mx

spp3 <- data.frame (Abundance3, Abundance3s, elev2, l3, l3s, u3, u3s)


###### Pogoniulus coryphaeus ######
#################################

###### Expected abundance 
Abundance4 <- (exp(HCDSM_Virunga_63spp$mean$beta0[4]  + HCDSM_Virunga_63spp$mean$beta1[4]  * elev + HCDSM_Virunga_63spp$mean$beta2[4] * elev^2))
Ab4mx <- max(Abundance4)
Abundance4s <- Abundance4/Ab4mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l4<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[4]  + HCDSM_Virunga_63spp$q2.5$beta1[4] * elev + HCDSM_Virunga_63spp$q2.5$beta2[4]* elev^2))
l4s <- l4/Ab4mx

u4 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[4]  + HCDSM_Virunga_63spp$q97.5$beta1[4] * elev + HCDSM_Virunga_63spp$q97.5$beta2[4]* elev^2))
u4s <- u4/Ab4mx

spp4 <- data.frame (Abundance4, Abundance4s, elev2, l4, l4s, u4, u4s)


###### Trachylaemus purpuratus ######
####################################

###### Expected abundance 

Abundance5 <- (exp(HCDSM_Virunga_63spp$mean$beta0[5]  + HCDSM_Virunga_63spp$mean$beta1[5]  * elev + HCDSM_Virunga_63spp$mean$beta2[5] * elev^2))
Ab5mx <- max(Abundance5)
Abundance5s <- Abundance5/Ab5mx


###### 95% credible interval (CI)

##### Lower 2.5 CI

l5<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[5]  + HCDSM_Virunga_63spp$q2.5$beta1[5] * elev + HCDSM_Virunga_63spp$q2.5$beta2[5]* elev^2))
l5s <- l5/Ab5mx

u5 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[5]  + HCDSM_Virunga_63spp$q97.5$beta1[5] * elev + HCDSM_Virunga_63spp$q97.5$beta2[5]* elev^2))
u5s <- u5/Ab5mx

spp5 <- data.frame (Abundance5, Abundance5s, elev2, l5, l5s, u5, u5s)



###### Elevational niche overlap- Estimation ################
#########Family: Lybiidae -- (Barbets) ######################
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
#####same relative scale (maximum abundance = 1)#############
#############################################################



######################################################
#### Integration function: Pogoniulus bilineatus #####
#####################################################

inter3<- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[3]  + 
                            HCDSM_Virunga_63spp$mean$beta1[3]  * x + HCDSM_Virunga_63spp$mean$beta2[3] * x^2)/Ab3mx}

### Area under the curve: Pogoniulus bilineatus 
integrate(inter3, -2.24,3.16) 


######################################################
#### Integration function: Pogoniulus coryphaeus #####
#####################################################
inter4 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[4]  + 
                             HCDSM_Virunga_63spp$mean$beta1[4]  * x + HCDSM_Virunga_63spp$mean$beta2[4] * x^2)/Ab4mx}

### Area under the curve: Pogoniulus coryphaeus
integrate(inter4, -2.24,3.16) 


#######################################################
#### Integration function: Trachylaemus purpuratus ####
#######################################################
inter5 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[5]  + 
                             HCDSM_Virunga_63spp$mean$beta1[5]  * x + HCDSM_Virunga_63spp$mean$beta2[5] * x^2)/Ab5mx}

integrate(inter5, -2.24,3.16)



########################################################
###### Area of overlap along an elevation gradient #####
########################################################

## Pogoniulus bilineatus & Pogoniulus coryphaeus########
####--------------------------------------------########

x <- elev ## Standardized elevation 
y3 <- Abundance3s ## Standardized abundance: Pogoniulus bilineatus
y4 <- Abundance4s ## Standardized abundance: Pogoniulus coryphaeus
### Computing area of overlap
spps3n4 <- approxfun(x, pmin(y3,y4), ties = "mean")
integrate(spps3n4, min(x), max(x))




## Pogoniulus bilineatus & Trachylaemus purpuratus#####
####----------------------------------------------#####
x <- elev ## Standardized elevation  
y3 <- Abundance3s ## Standardized abundance: Pogoniulus bilineatus
y5 <- Abundance5s ## Standardized abundance: Trachylaemus purpuratus
### Computing area of overlap
spps3n5 <- approxfun(x, pmin(y3,y5), ties = "mean")
integrate(spps3n5, min(x), max(x))


#### Pogoniulus coryphaeus & Trachylaemus purpuratus ####
######----------------------------------------------#####
x <- elev ## Standardized elevation 
y4 <- Abundance4s ## Standardized abundance: Pogoniulus coryphaeus
y5 <- Abundance5s ## Standardized abundance: Trachylaemus purpuratus
### Computing area of overlap
spps4n5 <- approxfun(x, pmin(y4,y5),ties = "mean")
integrate(spps4n5, min(x), max(x))



########### Response curves: Barbets ######
#########----------------------------######
####Co-abundance variation
####Pogoniulus bilineatus	& Pogoniulus coryphaeus

###### Figure 3c


Figure3_c = ggplot () +
  geom_line (data=spp3, aes(x=elev2 , y= Abundance3s, color = "#D55E00"), size =2) +
  geom_ribbon(data=spp3,aes(x=elev2 , y= Abundance3s, ymin = l3s, ymax = u3s), alpha = 0.5, fill= "#D55E00")+
  geom_line (data=spp4, aes(x=elev2 , y= Abundance4s, color ="blue"), size =2) +
  geom_ribbon(data=spp4,aes(x=elev2 , y= Abundance4s, ymin = l4s, ymax = u4s), alpha = 0.5, fill= "blue") +
  
  annotate("text", x = 2750, y = 3, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Lybiidae", fontface= "plain") +
  
  annotate("text", x = 2900, y = 1.8, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: elevation = 0.83", colour = "#D55E00", fontface= "plain") +
  
  annotate("text", x = 2900, y = 1.6, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: elevation = 0.68", colour = "blue", fontface= "plain") +
  
  annotate("text", x = 2900, y = 1.4, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: diet =0.98", colour = "black", fontface= "plain") +
  
  annotate("text", x = 2900, y = 1.2, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: strata =0.97", colour = "black", fontface= "plain") +
  
  annotate("text", x = 2900, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Activity pattern = diurnal", colour = "black", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 3))

Figure3_c + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Pogoniulus bilineatus (13.1g)", "Pogoniulus coryphaeus (10.1g)"), 
                     values=c( "#D55E00" = "#D55E00", "blue"="blue"), name = "Species") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 30),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=30,face="italic"),
        legend.position = c(c(0.4,0.8)), legend.title=element_text(size=30, color="black"))  




ggsave(file = "Figure3(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S3 (a)
##Standardized

Figure_S3_a = ggplot () +
  geom_line (data=spp3, aes(x=elev2 , y= Abundance3s, color ="orange"), size =2) +
  geom_line (data=spp4, aes(x=elev2 , y= Abundance4s, color ="blue"), size =2) +
  geom_line (data=spp5, aes(x=elev2 , y= Abundance5s, color ="black"), size =2) +
  
  
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Lybiidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 1))

Figure_S3_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Pogoniulus bilineatus (13.1g)", "Pogoniulus coryphaeus (10.7g)", "Trachylaemus purpuratus (76.1g)"), 
                     values=c( "orange"= "orange", "blue"="blue", "black" = "black"), name = "Species") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.7,0.6)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS3(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S3 (b)
## Confidence intervals

Figure_S3_b = ggplot () +
  geom_line (data=spp3, aes(x=elev2 , y= Abundance3, color = "orange"), size =2) +
  geom_ribbon(data=spp3,aes(x=elev2 , y= Abundance3, ymin = l3, ymax = u3), alpha = 0.5, fill= "orange")+
  geom_line (data=spp4, aes(x=elev2 , y= Abundance4, color ="blue"), size =2) +
  geom_ribbon(data=spp4,aes(x=elev2 , y= Abundance4, ymin = l4, ymax = u4), alpha = 0.5, fill= "blue") +
  geom_line (data=spp5, aes(x=elev2 , y= Abundance5, color ="black"), size =2) +
  geom_ribbon(data=spp5,aes(x=elev2 , y= Abundance5, ymin = l5, ymax = u5), alpha = 0.5, fill= "black") +
  
  annotate("text", x = 2750, y = 4, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Lybiidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 4))

  Figure_S3_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Pogoniulus bilineatus (13.1g)", "Pogoniulus coryphaeus (10.7g)", "Trachylaemus purpuratus (76.1g)"), 
                     values=c( "orange"= "orange", "blue"="blue", "black" = "black"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.7,0.6)), legend.title=element_text(size=25, color="black"))  




ggsave(file = "Appendix_S8_FigureS3(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


######## Lybiidae  -- (Barbets) #########
######## Foraging diet categories #######
#########################################

##Appendix S8: Figure S3 (c)

  ggplot(Barbets_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "orange", "brown")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Lybiidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Pogoniulus coryphaeus" = "Pogoniulus\ncoryphaeus","Pogoniulus bilineatus" = "Pogoniulus\nbilineatus",
                              "Trachylaemus purpuratus" = "Trachylaemus\npurpuratus"), name= " Species") +
  

  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 





ggsave(file = "Appendix_S8_FigureS3(C).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Lybiidae -- (Barbets)  ########
######## Foraging forest strata ########
########################################

##Appendix S8: Figure S3 (d)
ggplot(Barbets_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange","purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Lybiidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Pogoniulus coryphaeus" = "Pogoniulus\ncoryphaeus","Pogoniulus bilineatus" = "Pogoniulus\nbilineatus",
                              "Trachylaemus purpuratus" = "Trachylaemus\npurpuratus"), name= " Species") +
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 





ggsave(file = "Appendix_S8_FigureS3(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




########################################################################
###################### Family: Cisticolidae -- (Warblers)##############
########-------------------- 5 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################



###### Apalis personata #######
#############################

###### Expected abundance
Abundance45 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[45]  + HCDSM_Virunga_63spp$mean$beta1[45]  * elev + HCDSM_Virunga_63spp$mean$beta2[45] * elev^2))
Ab45mx <- max(Abundance45) # # Maximum abundance
Abundance45s <- Abundance45/Ab45mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l45 <-   (exp(HCDSM_Virunga_63spp$q2.5$beta0[45]  + HCDSM_Virunga_63spp$q2.5$beta1[45] * elev + HCDSM_Virunga_63spp$q2.5$beta2[45]* elev^2))
l45s <- l45/Ab45mx

##### Upper 97.5 CI

u45 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[45]  + HCDSM_Virunga_63spp$q97.5$beta1[45] * elev + HCDSM_Virunga_63spp$q97.5$beta2[45]* elev^2))
u45s <- u45/Ab45mx

###### create a data frame
spp45 <- data.frame (Abundance45,Abundance45s, elev, l45, l45s, u45s)


###### Apalis porphyrolaema #######
###################################

###### Expected abundance
Abundance46 <- (exp(HCDSM_Virunga_63spp$mean$beta0[46]  + HCDSM_Virunga_63spp$mean$beta1[46]  * elev + HCDSM_Virunga_63spp$mean$beta2[46] * elev^2))

Ab46mx <- max(Abundance46) # # Maximum abundance
Abundance46s <- Abundance46/Ab46mx
###### 95% credible interval (CI)

##### Lower 2.5 CI

l46<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[46]  + HCDSM_Virunga_63spp$q2.5$beta1[46] * elev + HCDSM_Virunga_63spp$q2.5$beta2[46]* elev^2))
l46s <- l46/Ab46mx

##### Upper 97.5 CI
u46 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[46]  + HCDSM_Virunga_63spp$q97.5$beta1[46] * elev + HCDSM_Virunga_63spp$q97.5$beta2[46]* elev^2))
u46s <- u46/Ab46mx

###### create a data frame
spp46 <- data.frame (Abundance46, Abundance46s, elev, l46, l46s,u46,u46s)



###### Cisticola chubbi #######
###############################

###### Expected abundance
Abundance50 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[50]  + HCDSM_Virunga_63spp$mean$beta1[50]  * elev + HCDSM_Virunga_63spp$mean$beta2[50] * elev^2))
Ab50mx <- max(Abundance50)  # # Maximum abundance
Abundance50s <- Abundance50/Ab50mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l50<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[50]  + HCDSM_Virunga_63spp$q2.5$beta1[50] * elev + HCDSM_Virunga_63spp$q2.5$beta2[50]* elev^2))
l50s <- l50/Ab50mx

##### Upper 97.5 CI

u50 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[50]  + HCDSM_Virunga_63spp$q97.5$beta1[50] * elev + HCDSM_Virunga_63spp$q97.5$beta2[50]* elev^2))
u50s <- u50/Ab50mx


###### create a data frame
spp50 <- data.frame (Abundance50, Abundance50s, elev, l50, l50s, u50, u50s)



###### Oreolais ruwenzorii #######
##################################

###### Expected abundance
Abundance51 <- (exp(HCDSM_Virunga_63spp$mean$beta0[51]  + HCDSM_Virunga_63spp$mean$beta1[51]  * elev + HCDSM_Virunga_63spp$mean$beta2[51] * elev^2))
Ab51mx <- max(Abundance51) # # Maximum abundance
Abundance51s <- Abundance51/Ab51mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l51<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[51]  + HCDSM_Virunga_63spp$q2.5$beta1[51] * elev + HCDSM_Virunga_63spp$q2.5$beta2[51]* elev^2))
l51s <- l51/Ab51mx


##### Upper 97.5 CI
u51 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[51]  + HCDSM_Virunga_63spp$q97.5$beta1[51] * elev + HCDSM_Virunga_63spp$q97.5$beta2[51]* elev^2))
u51s <- u51/Ab51mx

###### create a data frame
spp51 <- data.frame (Abundance51s, Abundance51, elev, l51, l51s, u51, u51s)



###### Prinia bairdii #######
#############################

###### Expected abundance
Abundance54 <- (exp(HCDSM_Virunga_63spp$mean$beta0[54]  + HCDSM_Virunga_63spp$mean$beta1[54]  * elev + HCDSM_Virunga_63spp$mean$beta2[54] * elev^2))
Ab54mx <- max(Abundance54) # # Maximum abundance
Abundance54s <- Abundance54/Ab54mx


###### 95% credible interval (CI)

##### Lower 2.5 CI

l54<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[54]  + HCDSM_Virunga_63spp$q2.5$beta1[54] * elev + HCDSM_Virunga_63spp$q2.5$beta2[54]* elev^2))
l54s <- l54/Ab54mx


##### Upper 97.5 CI
u54 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[54]  + HCDSM_Virunga_63spp$q97.5$beta1[54] * elev + HCDSM_Virunga_63spp$q97.5$beta2[54]* elev^2))
u54s <- u54/Ab54mx


###### create a data frame
spp54 <- data.frame (Abundance54, Abundance54s, elev, l54, l54s, u54, u54s)





###### Elevational niche overlap- Estimation ################
######### Family: Cisticolidae -- (Warblers) #################
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
##### same relative scale (maximum abundance = 1)############
#############################################################



######################################################
#### Integration function -- Apalis personata#########
#####################################################


inter45 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[45]  + 
                              HCDSM_Virunga_63spp$mean$beta1[45]  * x + HCDSM_Virunga_63spp$mean$beta2[45] * x^2)/Ab45mx}

### Area under the curve: Apalis personata
integrate(inter45, -2.24,3.16) 


######################################################
#### Integration function -- Apalis porphyrolaema ####
#####################################################


inter46 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[46]  + 
                              HCDSM_Virunga_63spp$mean$beta1[46]  * x + HCDSM_Virunga_63spp$mean$beta2[46] * x^2)/Ab46mx}

### Area under the curve: Apalis porphyrolaema
integrate(inter46, -2.24,3.16) 



######################################################
#### Integration function -- Cisticola chubbi  ######
#####################################################


inter50 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[50]  + 
                              HCDSM_Virunga_63spp$mean$beta1[50]  * x + HCDSM_Virunga_63spp$mean$beta2[50] * x^2)/Ab50mx}


### Area under the curve: Cisticola chubbi
integrate(inter50, -2.24,3.16) 


######################################################
#### Integration function -- Oreolais ruwenzorii ####
#####################################################

inter51 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[51]  + 
                              HCDSM_Virunga_63spp$mean$beta1[51]  * x + HCDSM_Virunga_63spp$mean$beta2[51] * x^2)/Ab51mx}

### Area under the curve: Oreolais ruwenzorii
integrate(inter51, -2.24,3.16) 



######################################################
#### Integration function -- Prinia bairdii  ########
#####################################################

inter54 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[54]  + 
                              HCDSM_Virunga_63spp$mean$beta1[54]  * x + HCDSM_Virunga_63spp$mean$beta2[54] * x^2)/Ab54mx}


### Area under the curve: Prinia bairdii 
integrate(inter54, -2.24,3.16) 




#######################################################
###### Area of overlap along an elevation gradient####
#######################################################

## Apalis personata & Apalis porphyrolaema#####
####-------------------------------------#####
x <- elev ## Standardized elevation 
y45 <- Abundance45s ## Standardized abundance: Apalis personata 
y46 <- Abundance46s ## Standardized abundance: Apalis porphyrolaema
### Computing area of overlap
spps45n46 <- approxfun(x, pmin(y45,y46), ties = "mean")
integrate(spps45n46, min(x), max(x))



## Apalis personata & Cisticola chubbi #####
####-----------------------------------#####
x <- elev ## Standardized elevation 
y45 <- Abundance45s ## Standardized abundance: Apalis personata 
y50 <- Abundance50s ## Standardized abundance: Cisticola chubbi 
### Computing area of overlap
spps45n50 <- approxfun(x, pmin(y45,y50),ties = "mean")
integrate(spps45n50, min(x), max(x))


## Apalis personata & Oreolais ruwenzorii #####
####-------------------------------------#####
x <- elev ## Standardized elevation
y45 <- Abundance45s ## Standardized abundance: Apalis personata 
y51 <- Abundance51s ## Standardized abundance: Oreolais ruwenzorii
### Computing area of overlap
spps45n51 <- approxfun(x, pmin(y45,y51),ties = "mean")
integrate(spps45n51, min(x), max(x))




## Apalis personata & Prinia bairdii #####
####---------------------------------#####

x <- elev ## Standardized elevation
y45 <- Abundance45s ## Standardized abundance: Apalis personata 
y54 <- Abundance54s ## Standardized abundance: Prinia bairdii
### Computing area of overlap
spps45n54  <- approxfun(x, pmin(y45,y54),ties = "mean")
integrate(spps45n54, min(x), max(x))


## Apalis porphyrolaema &  Cisticola chubbi #####
####---------------------------------------#####
x <- elev ## Standardized elevation
y46 <- Abundance46s ## Standardized abundance: Apalis porphyrolaema
y50 <- Abundance50s ## Standardized abundance: Cisticola chubbi 
### Computing area of overlap
spps46n50  <- approxfun(x, pmin(y46,y50),ties = "mean")
integrate(spps46n50, min(x), max(x))



## Apalis porphyrolaema & Oreolais ruwenzorii #####
####-----------------------------------------#####
x <- elev ## Standardized elevation
y46 <- Abundance46s ## Standardized abundance: Apalis porphyrolaema
y51 <- Abundance51s ## Standardized abundance: Oreolais ruwenzorii
### Computing area of overlap
spps46n51 <- approxfun(x, pmin(y46,y51),ties = "mean")
integrate(spps46n51, min(x), max(x))



## Apalis porphyrolaema & Prinia bairdii 
x <- elev ## Standardized elevation
y46 <- Abundance46s ## Standardized abundance: Apalis porphyrolaema
y54 <- Abundance54s ## Standardized abundance: Prinia bairdii
 ### Computing area of overlap
spps46n54 <- approxfun(x, pmin(y46,y54),ties = "mean")
integrate(spps46n54, min(x), max(x))



###Cisticola chubbi & Oreolais ruwenzorii 
# Cisticola chubbi
y50 <- Abundance50s ## Standardized abundance: Cisticola chubbi 
y51 <- Abundance51s ## Standardized abundance: Oreolais ruwenzorii
### Computing area of overlap
spps50n51 <- approxfun(x, pmin(y50,y51),ties = "mean")
integrate(spps50n51, min(x), max(x))


###Cisticola chubbi & Prinia bairdii 
x <- elev ## Standardized elevation
y50 <- Abundance50s ## Standardized abundance: Cisticola chubbi 
y54 <- Abundance54s ## Standardized abundance: Prinia bairdii
### Computing area of overlap
spps50n54 <- approxfun(x, pmin(y50,y54),ties = "mean")
integrate(spps50n54 , min(x), max(x))


###Oreolais ruwenzorii & Prinia bairdii 
x <- elev ## Standardized elevation
y51 <- Abundance51s ## Standardized abundance: Oreolais ruwenzorii
y54 <- Abundance54s ## Standardized abundance: Prinia bairdii
### Computing area of overlap
spps50n54 <- approxfun(x, pmin(y51,y54),ties = "mean")
integrate(spps50n54, min(x), max(x))




########### Response curves: Sunbirds ###
#########--------------------------######
#### Co-abundance variation
#### Apalis personata & Apalis porphyrolaema 
###### Figure 3d


Figure3_d = ggplot () +
  geom_line (data=spp45, aes(x=elev2 , y= Abundance45s, color = "#D55E00"), size =2) +
  geom_ribbon(data=spp45,aes(x=elev2 , y= Abundance45s, ymin = l45s, ymax = u45s), alpha = 0.5, fill= "#D55E00")+
  geom_line (data=spp46, aes(x=elev2 , y= Abundance46s, color ="blue"), size =2) +
  geom_ribbon(data=spp46,aes(x=elev2 , y= Abundance46s, ymin = l46s, ymax = u46s), alpha = 0.5, fill= "blue") +
  
  
  annotate("text", x = 2500, y = 1.5, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Cisticolidae", fontface= "plain") +
  
  annotate("text", x = 3080, y = 1.12, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: elevation = 0.99", colour = "#D55E00", fontface= "plain") +
  
  annotate("text", x = 3080, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: elevation = 0.71", colour = "blue", fontface= "plain") +
  
  annotate("text", x = 2100, y = 0.25, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: diet = 1", colour = "black", fontface= "plain") +
  
  annotate("text", x = 2100, y = 0.15, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Niche overlap: strata = 0.38", colour = "black", fontface= "plain") +
  
  
  annotate("text", x = 2100, y = 0.05, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Activity pattern = diurnal", colour = "black", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 1.5))


Figure3_d  + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Apalis personata (11g)","Apalis porphyrolaema (8.4g)" ), 
                     values=c( "#D55E00"= "#D55E00", "blue"= "blue"), name ="Species") +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 30),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=30, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=30, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=30,face="italic"),
        legend.position = c(c(0.75,0.87)), legend.title=element_text(size=30, color="black")) 



ggsave(file = "Figure3(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




##Appendix S8: Figure S4 (a)
##Standardized

Figure_S4_a = ggplot () +
  geom_line (data=spp45, aes(x=elev2 , y= Abundance45s, color ="red"), size =2) +
  geom_line (data=spp46, aes(x=elev2 , y= Abundance46s, color ="blue"), size =2) +
  geom_line (data=spp50, aes(x=elev2 , y= Abundance50s, color ="orange"), size =2) +
  geom_line (data=spp51, aes(x=elev2 , y= Abundance51s, color ="black"), size =2) +
  geom_line (data=spp54, aes(x=elev2 , y= Abundance54s, color ="grey"), size =2) +
  
  
  
  annotate("text", x = 2700, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Cisticolidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1))

Figure_S4_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Apalis personata (11g)", "Apalis porphyrolaema (8.39g)","Cisticola chubbi (16.03g)", 
                                "Oreolais ruwenzori (9.9g)", "Prinia bairdii (13.4g)"), 
                     values=c( "red"= "red", "blue"="blue", "orange"= "orange", "black"="black", "grey"= "grey"), name = "Species") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.7)), legend.title=element_text(size=25, color="black")) 



ggsave(file = "Appendix_S8_Figure_S4(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




##Appendix S8: Figure S4 (b)
## Confidence intervals

Figure_S4_b = ggplot () +
  geom_line (data=spp45, aes(x=elev2 , y= Abundance45, color ="red"), size =2) +
  geom_ribbon(data=spp45,aes(x=elev2 , y= Abundance45, ymin = l45, ymax = u45), alpha = 0.5, fill= "red")+
  geom_line (data=spp46, aes(x=elev2 , y= Abundance46, color ="blue"), size =2) +
  geom_ribbon(data=spp46,aes(x=elev2 , y= Abundance46, ymin = l46, ymax = u46), alpha = 0.5, fill= "blue") +
  geom_line (data=spp50, aes(x=elev2 , y= Abundance50, color ="orange"), size =2) +
  geom_ribbon(data=spp50,aes(x=elev2 , y= Abundance50, ymin = l50, ymax = u50), alpha = 0.5, fill= "orange") +
  geom_line (data=spp51, aes(x=elev2 , y= Abundance51, color ="black"), size =2) +
  geom_ribbon(data=spp51,aes(x=elev2 , y= Abundance51, ymin = l51, ymax = u51), alpha = 0.5, fill= "black") +
  geom_line (data=spp54, aes(x=elev2 , y= Abundance54, color ="grey"), size =2) +
  geom_ribbon(data=spp54,aes(x=elev2 , y= Abundance54, ymin = l54, ymax = u54), alpha = 0.5, fill= "grey") +
  
  
  
  annotate("text", x = 2700, y = 20, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Cisticolidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 20))

Figure_S4_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Apalis personata (11g)", "Apalis porphyrolaema (8.39g)","Cisticola chubbi (16.3g)", 
                                "Oreolais ruwenzori (9.9g)", "Prinia bairdii (13.4g)" ), 
                     values=c( "red"= "red", "blue"="blue", "orange"= "orange", "black"="black", "grey"= "grey"), name ="Species") +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.7)), legend.title=element_text(size=25, color="black")) 



ggsave(file = "Appendix_S8_Figure_S4(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



########Cisticolidae  -- (Warblers) ######
######## Foraging diet categories########
#########################################

##Appendix S8: Figure S4 (c)

ggplot(Warblers_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Cisticolidae") +
  ylab("Proportional use (%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Apalis personata" = "Apalis\npersonata","Apalis porphyrolaema" = "Apalis\nporphyrolaema", 
                              "Cisticola chubbi" = "Cisticola\nchubbi", "Oreolais ruwenzorii" = "Oreolais\nruwenzorii",
                              "Prinia bairdii" = "Prinia\nbairdii")) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 


ggsave(file = "Appendix_S8_FigureS4(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Cisticolidae  -- (Warblers) #####
######## Foraging forest strata ##########
#########################################

##Appendix S8: Figure S4 (d)

ggplot(Warblers_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("black", "orange", "purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Cisticolidae") +
  ylab("Proportional use (%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Apalis personata" = "Apalis\npersonata","Apalis porphyrolaema" = "Apalis\nporphyrolaema", 
                              "Cisticola chubbi" = "Cisticola\nchubbi", "Oreolais ruwenzorii" = "Oreolais\nruwenzorii",
                              "Prinia bairdii" = "Prinia\nbairdii")) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 





ggsave(file = "Appendix_S8_FigureS4(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)






########################################################################
###################### Family: Phylloscopidae -- (Leaf warblers)##############
########-------------------- 2 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################



###### Phylloscopus laetus #######
##################################

###### Expected abundance

Abundance52 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[52]  + HCDSM_Virunga_63spp$mean$beta1[52]  * elev + HCDSM_Virunga_63spp$mean$beta2[52] * elev^2))
Ab52mx <- max(Abundance52) # # Maximum abundance
Abundance52s <- Abundance52/Ab52mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l52<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[52]  + HCDSM_Virunga_63spp$q2.5$beta1[52] * elev + HCDSM_Virunga_63spp$q2.5$beta2[52]* elev^2))
l52s <- l52/Ab52mx

##### Upper 97.5 CI
u52 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[52]  + HCDSM_Virunga_63spp$q97.5$beta1[52] * elev + HCDSM_Virunga_63spp$q97.5$beta2[52]* elev^2))
u52s <- u52/Ab52mx

###### create a data frame
spp52 <- data.frame (Abundance52, Abundance52s, elev, l52, l52s, u52, u52s)


 
###### Phylloscopus umbrovirens #######
#######################################

Abundance53 <- (exp(HCDSM_Virunga_63spp$mean$beta0[53]  + HCDSM_Virunga_63spp$mean$beta1[53]  * elev + HCDSM_Virunga_63spp$mean$beta2[53] * elev^2))
Ab53mx <- max(Abundance53) # # Maximum abundance
Abundance53s <- Abundance53/Ab53mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l53<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[53]  + HCDSM_Virunga_63spp$q2.5$beta1[53] * elev + HCDSM_Virunga_63spp$q2.5$beta2[53]* elev^2))
l53s <- l53/Ab53mx

##### Upper 97.5 CI

u53 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[53]  + HCDSM_Virunga_63spp$q97.5$beta1[53] * elev + HCDSM_Virunga_63spp$q97.5$beta2[53]* elev^2))
u53s <- u53/Ab53mx

###### create a data frame

spp53 <- data.frame (Abundance53, Abundance53s, elev, l53, l53s, l53, u53s)




###### Elevational niche overlap- Estimation ################
######### Phylloscopidae -- (Leaf warblers)###################
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
##### same relative scale (maximum abundance = 1)#############
#############################################################



######################################################
#### Integration function: Phylloscopus_laetus ###########
#####################################################


inter52 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[52]  + 
                              HCDSM_Virunga_63spp$mean$beta1[52]  * x + HCDSM_Virunga_63spp$mean$beta2[52] * x^2)/Ab52mx}

### Area under the curve: Phylloscopus_laetus
integrate(inter52, -2.24,3.16) 


############################################################
#### Integration function: Phylloscopus umbrovirens ########
###########################################################


inter53 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[53]  + 
                              HCDSM_Virunga_63spp$mean$beta1[53]  * x + HCDSM_Virunga_63spp$mean$beta2[53] * x^2)/Ab53mx}

### Area under the curve: Phylloscopus umbrovirens 

integrate(inter53, -2.24,3.16)



#######################################################
###### Area of overlap along an elevation gradient#####
#######################################################

####  Phylloscopus_laetus & Phylloscopus umbrovirens #####
####------------------------------------------------######
x <- elev ## Standardized elevation 
y52 <- Abundance52s ## Standardized abundance: Phylloscopus laetus
y53 <- Abundance53s ## Standardized abundance: Phylloscopus umbrovirens
### Computing area of overlap
spps52n53 <- approxfun(x, pmin(y52,y53), ties = "mean")
integrate(spps52n53, min(x), max(x))




##Appendix S8: Figure S5 (a)
##Standardized


Figure_S5_a = ggplot () +
  geom_line (data=spp52, aes(x=elev2 , y= Abundance52s, color ="orange"), size =2) +
  geom_line (data=spp53, aes(x=elev2 , y= Abundance53s, color ="blue"), size =2) +
  
  
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Phylloscopidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1))

Figure_S5_a  + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Phylloscopus laetus (9.54g)", "Phylloscopus umbrovirens (8.6g)"), 
                     values=c( "orange"= "orange", "blue"="blue"), name =" Species") +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.35,0.4)), legend.title=element_text(size=25, color="black"))   




ggsave(file = "Appendix_S8_FigureS5(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)

##Appendix S8: Figure S5 (b)
## Confidence intervals

Figure_S5_b = ggplot () +
  geom_line (data=spp52, aes(x=elev2 , y= Abundance52, color ="orange"), size =2) +
  geom_ribbon(data=spp52,aes(x=elev2 , y= Abundance52, ymin = l52, ymax = u52), alpha = 0.5, fill= "orange")+
  geom_line (data=spp53, aes(x=elev2 , y= Abundance53, color ="blue"), size =2) +
  geom_ribbon(data=spp53,aes(x=elev2 , y= Abundance53, ymin = l53, ymax = u53), alpha = 0.5, fill= "blue") +
  annotate("text", x = 2700, y = 40, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Phylloscopidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 40))

Figure_S5_b  + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Phylloscopus laetus (9.54g)", "Phylloscopus umbrovirens (8.6g)"), 
                     values=c( "orange"= "orange", "blue"="blue"), name =" Species") +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.25,0.7)), legend.title=element_text(size=25, color="black"))   





ggsave(file = "Appendix_S8_FigureS5(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


######## Phylloscopidae -- (Leaf_warblers ) ######
######## Foraging diet categories ##############
################################################

##Appendix S8: Figure S5 (c)

ggplot(Leaf_warblers_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Phylloscopidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Phylloscopus laetus" = "Phylloscopus\nlaetus","Phylloscopus umbrovirens" = "Phylloscopus\numbrovirens"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 


ggsave(file = "Appendix_S8_FigureS5(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




######## Phylloscopidae -- (Leaf_warblers) #####
######## Foraging forest strata ###############
###############################################

##Appendix S8: Figure S5 (d)

ggplot(Leaf_warblers_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("black", "orange", "purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Phylloscopidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Phylloscopus laetus" = "Phylloscopus\nlaetus","Phylloscopus umbrovirens" = "Phylloscopus\numbrovirens"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 





ggsave(file = "Appendix_S8_FigureS5(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


######################################################################
###################### Family: Malaconotidae -- (Bushshrikes)#########
########-------------------- 5 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################



###### Dryoscopus gambensis######
################################

###### Expected abundance
Abundance18 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[18]  + HCDSM_Virunga_63spp$mean$beta1[18]  * elev + HCDSM_Virunga_63spp$mean$beta2[18] * elev^2))
Ab18mx <- max(Abundance18) # # Maximum abundance
Abundance18s <- Abundance18/Ab18mx 

###### 95% credible interval (CI)

##### Lower 2.5 CI

l18<-   (exp(HCDSM_Virunga_63spp$q2.5$beta0[18]  + HCDSM_Virunga_63spp$q2.5$beta1[18] * elev + HCDSM_Virunga_63spp$q2.5$beta2[18]* elev^2))
l18s <- l18/Ab18mx 


##### Upper 97.5 CI
u18 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[18]  + HCDSM_Virunga_63spp$q97.5$beta1[18] * elev + HCDSM_Virunga_63spp$q97.5$beta2[18]* elev^2))
u18s <-u18/Ab18mx 

###### create a data frame
spp18 <- data.frame (Abundance18, Abundance18s, elev2, l18, l18s, u18, u18s)



###### Laniarius aethiopicus #######
###################################

###### Expected abundance
Abundance19 <- (exp(HCDSM_Virunga_63spp$mean$beta0[19]  + HCDSM_Virunga_63spp$mean$beta1[19]  * elev + HCDSM_Virunga_63spp$mean$beta2[19] * elev^2))
Ab19mx <- max(Abundance19) # # Maximum abundance
Abundance19s <- Abundance19/Ab19mx 

###### 95% credible interval (CI)

##### Lower 2.5 CI

l19<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[19]  + HCDSM_Virunga_63spp$q2.5$beta1[19] * elev + HCDSM_Virunga_63spp$q2.5$beta2[19]* elev^2))
l19s <- l19/Ab19mx 

##### Upper 97.5 CI
u19 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[19]  + HCDSM_Virunga_63spp$q97.5$beta1[19] * elev + HCDSM_Virunga_63spp$q97.5$beta2[19]* elev^2))
u19s <-u19/Ab19mx 

###### create a data frame

spp19 <- data.frame (Abundance19, Abundance19s, elev2, l19, l19s, u19, u19s)



###### Laniarius luehderi #######
################################

###### Expected abundance
Abundance20 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[20]  + HCDSM_Virunga_63spp$mean$beta1[20]  * elev + HCDSM_Virunga_63spp$mean$beta2[20] * elev^2))
Ab20mx <- max(Abundance20)
Abundance20s <- Abundance20/Ab20mx 


###### 95% credible interval (CI)

##### Lower 2.5 CI
l20<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[20]  + HCDSM_Virunga_63spp$q2.5$beta1[20] * elev + HCDSM_Virunga_63spp$q2.5$beta2[20]* elev^2))
l20s <- l20/Ab20mx

##### Upper 97.5 CI
u20 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[20]  + HCDSM_Virunga_63spp$q97.5$beta1[20] * elev + HCDSM_Virunga_63spp$q97.5$beta2[20]* elev^2))
u20s <-u20/Ab20mx

###### create a data frame
spp20 <- data.frame (Abundance20, Abundance20s, elev2, l20, l20s, u20, u20s)



###### Laniarius poensis #######
################################

###### Expected abundance
Abundance21 <- (exp(HCDSM_Virunga_63spp$mean$beta0[21]  + HCDSM_Virunga_63spp$mean$beta1[21]  * elev + HCDSM_Virunga_63spp$mean$beta2[21] * elev^2))
Ab21mx <- max(Abundance21)
Abundance21s <- Abundance21/Ab21mx 


###### 95% credible interval (CI)

##### Lower 2.5 CI

l21<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[21]  + HCDSM_Virunga_63spp$q2.5$beta1[21] * elev + HCDSM_Virunga_63spp$q2.5$beta2[21]* elev^2))
l21s <- l21/Ab21mx

##### Upper 97.5 CI
u21 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[21]  + HCDSM_Virunga_63spp$q97.5$beta1[21] * elev + HCDSM_Virunga_63spp$q97.5$beta2[21]* elev^2))
u21s <-u21/Ab21mx

###### create a data frame
spp21 <- data.frame (Abundance21, Abundance21s, elev2, l21, l21s, u21, u21s)



###### Telophorus dohertyi #######
#################################

###### Expected abundance
Abundance22 <- (exp(HCDSM_Virunga_63spp$mean$beta0[22]  + HCDSM_Virunga_63spp$mean$beta1[22]  * elev + HCDSM_Virunga_63spp$mean$beta2[22] * elev^2))
Ab22mx <- max(Abundance22)
Abundance22s <- Abundance22/Ab22mx 

###### 95% credible interval (CI)

##### Lower 2.5 CI

l22<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[22]  + HCDSM_Virunga_63spp$q2.5$beta1[22] * elev + HCDSM_Virunga_63spp$q2.5$beta2[22]* elev^2))
l22s <- l22/Ab22mx

##### Upper 97.5 CI
u22 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[22]  + HCDSM_Virunga_63spp$q97.5$beta1[22] * elev + HCDSM_Virunga_63spp$q97.5$beta2[22]* elev^2))
u22s <-u22/Ab22mx

###### create a data frame
spp22 <- data.frame (Abundance22, Abundance22s, elev2, l22, l22s, u22, u22s)



###### Elevational niche overlap- Estimation ################
######### Family: Malaconotidae -- (Bushshrikes) ############
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
##### same relative scale (maximum abundance = 1)############
#############################################################


######################################################
#### Integration function: Dryoscopus gambensis#######
#####################################################

inter18 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[18]  + 
                              HCDSM_Virunga_63spp$mean$beta1[18]  * x + HCDSM_Virunga_63spp$mean$beta2[18] * x^2)/Ab18mx}

### Area under the curve: Dryoscopus gambensis
integrate(inter18, -2.24,3.16) 



######################################################
#### Integration function: Laniarius aethiopicus######
#####################################################
 
inter19 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[19]  + 
                              HCDSM_Virunga_63spp$mean$beta1[19]  * x + HCDSM_Virunga_63spp$mean$beta2[19] * x^2)/Ab19mx}

### Area under the curve: Laniarius aethiopicus
integrate(inter19, -2.24,3.16) 




######################################################
#### Integration function:  Laniarius luehderi######
#####################################################

inter20 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[20]  + 
                              HCDSM_Virunga_63spp$mean$beta1[20]  * x + HCDSM_Virunga_63spp$mean$beta2[20] * x^2)/Ab20mx}

### Area under the curve: Laniarius luehderi

integrate(inter20, -2.24,3.16) 



######################################################
#### Integration function:  Laniarius poensis######
#####################################################

inter21 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[21]  + 
                              HCDSM_Virunga_63spp$mean$beta1[21]  * x + HCDSM_Virunga_63spp$mean$beta2[21] * x^2)/Ab21mx}

### Area under the curve: Laniarius poensis

integrate(inter21, -2.24,3.16) 


######################################################
#### Integration function:  Telophorus dohertyi######
#####################################################

inter22 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[22]  + 
                              HCDSM_Virunga_63spp$mean$beta1[22]  * x + HCDSM_Virunga_63spp$mean$beta2[22] * x^2)/Ab22mx}

### Area under the curve: Telophorus dohertyi

integrate(inter22, -2.24,3.16)



#######################################################
###### Area of overlap along the elevation gradient####
#######################################################


 
## Dryoscopus gambensis & Laniarius aethiopicus ###
####------------------------------------------#####
x <- elev   ## Standardized elevation 
y18 <- Abundance18s  ## Standardized abundance: Dryoscopus gambensis
y19 <- Abundance19s   ## Standardized abundance: Laniarius aethiopicus
### Estimating area of overlap
spps18n19 <- approxfun(x, pmin(y18,y19), ties = "mean")
integrate(spps18n19, min(x), max(x))


## Dryoscopus gambensis & Laniarius luehderi ####
####---------------------------------------#####
x <- elev  ## Standardized elevation 
y18 <- Abundance18s ## Standardized abundance: Dryoscopus gambensis
y20 <- Abundance20s ## Standardized abundance: Laniarius luehderi 
### Estimating area of overlap
spps18n20 <- approxfun(x, pmin(y18,y20),ties = "mean")
integrate(spps18n20 , min(x), max(x)) 

 
## Dryoscopus gambensis & Laniarius poensis ###
####-------------------------------------#####
x <- elev  ## Standardized elevation 
y18 <- Abundance18s ## Standardized abundance: Dryoscopus gambensis
y21 <- Abundance21s ## Standardized abundance: Laniarius poensis

### Estimating area of overlap
spps18n21 <- approxfun(x, pmin(y18,y21),ties = "mean")
integrate(spps18n21, min(x), max(x))



## Dryoscopus gambensis & Telophorus dohertyi ###
####----------------------------------------#####
x <- elev  ## Standardized elevation 
y18 <- Abundance18s ## Standardized abundance: Dryoscopus gambensis
y22 <- Abundance22s ## Standardized abundance: Telophorus dohertyi
### Estimating area of overlap
spps18n22 <- approxfun(x, pmin(y18,y22),ties = "mean")
integrate(spps18n22, min(x), max(x))




##  Laniarius aethiopicus &  Laniarius luehderi ####
####------------------------------------------#####
x <- elev  ## Standardized elevation 
y19 <- Abundance19s  ## Standardized abundance: Laniarius aethiopicus
y20 <- Abundance20s   ## Standardized abundance: Laniarius luehderi 
### Estimating area of overlap
spps19n20 <- approxfun(x, pmin(y19,y20), ties = "mean")
integrate(spps19n20, min(x), max(x))  # Note there appears to be some numerical errors in the overlap calculations
# Area of overlap is greater than 1 in cases where there is complete overlap. This
# is fine



##  Laniarius aethiopicus & Laniarius poensis #####
####-----------------------------------------#####
x <- elev  ## Standardized elevation 
y19 <- Abundance19s  ## Standardized abundance: Laniarius aethiopicus
y21 <- Abundance21s  ## Standardized abundance: Laniarius poensis
### Estimating area of overlap
spps19n21 <- approxfun(x, pmin(y19,y21),ties = "mean")
integrate(spps19n21 , min(x), max(x))



##  Laniarius aethiopicus & Telophorus dohertyi #####
####--------------------------------------------#####
x <- elev  ## Standardized elevation 
y19 <- Abundance19s  ## Standardized abundance: Laniarius aethiopicus
y22 <- Abundance22s ## Standardized abundance: Telophorus dohertyi
### Estimating area of overlap
spps19n22 <- approxfun(x, pmin(y19,y22),ties = "mean")
integrate(spps19n22 , min(x), max(x))



##  Laniarius luehderi & & Laniarius poensis ####
####----------------------------------------#####
x <- elev ## Standardized elevation 
y20 <- Abundance20s ## Standardized abundance: Laniarius luehderi 
y21 <- Abundance21s ## Standardized abundance: Laniarius poensis
### Estimating area of overlap
spps20n21 <- approxfun(x, pmin(y20,y21),ties = "mean")
integrate(spps20n21, min(x), max(x))


##  Laniarius luehderi & Telophorus dohertyi ####
####----------------------------------------#####
x <- elev ## Standardized elevation 
y20 <- Abundance20s ## Standardized abundance: Laniarius luehderi 
y22 <- Abundance22s ## Standardized abundance: Telophorus dohertyi
### Estimating area of overlap
spps20n22 <- approxfun(x, pmin(y20,y22),ties = "mean")
integrate(spps20n22, min(x), max(x))


##  Laniarius poensis & Telophorus dohertyi ####
####---------------------------------------#####
x <- elev  ## Standardized elevation 
y21 <- Abundance21s  ## Standardized abundance: Laniarius poensis
y22 <- Abundance22s ## Standardized abundance: Telophorus dohertyi
### Estimating area of overlap
spps21n22 <- approxfun(x, pmin(y21,y22),ties = "mean")
integrate(spps21n22 , min(x), max(x))



##Appendix S8: Figure S6(a)
##Standardized


Figure_S6_a = ggplot () +
  geom_line (data=spp18, aes(x=elev2 , y= Abundance18s, color ="brown"), size =2) +
  geom_line (data=spp19, aes(x=elev2 , y= Abundance19s, color ="blue"), size =2) +
  geom_line (data=spp20, aes(x=elev2 , y= Abundance20s, color ="orange"), size =2) +
  geom_line (data=spp21, aes(x=elev2 , y= Abundance21s, color ="black"), size =2) +
  geom_line (data=spp22, aes(x=elev2 , y= Abundance22s, color ="grey"), size =2) +
  
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Malaconotidae ", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1))

Figure_S6_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Dryoscopus gambensis (31.9g)", "Laniarius aethiopicus (49.44g)","Laniarius luehderi (42.9g)", 
                                "Laniarius poensis (44.9g)","Telophorus dohertyi (35.02g)"  ), 
                     values=c( "brown"= "brown", "blue"="blue", "orange"= "orange", "black"="black", "grey" = "grey"), name = "Species ") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.7)), legend.title=element_text(size=25, color="black"))  


ggsave(file = "Appendix_S8_FigureS6(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


##Appendix S8: Figure S6(b)
## Confidence intervals


Figure_S6_b = ggplot () +
  geom_line (data=spp18, aes(x=elev2 , y= Abundance18, color ="brown"), size =2) +
  geom_ribbon(data=spp18,aes(x=elev2 , y= Abundance18, ymin = l18, ymax = u18), alpha = 0.5, fill= "brown")+
  geom_line (data=spp19, aes(x=elev2 , y= Abundance19, color ="blue"), size =2) +
  geom_ribbon(data=spp19,aes(x=elev2 , y= Abundance19, ymin = l19, ymax = u19), alpha = 0.5, fill= "blue") +
  geom_line (data=spp20, aes(x=elev2 , y= Abundance20, color ="orange"), size =2) +
  geom_ribbon(data=spp20,aes(x=elev2 , y= Abundance20, ymin = l20, ymax = u20), alpha = 0.5, fill= "orange") +
  geom_line (data=spp21, aes(x=elev2 , y= Abundance21, color ="black"), size =2) +
  geom_ribbon(data=spp21,aes(x=elev2 , y= Abundance21, ymin = l21, ymax = u21), alpha = 0.5, fill= "black") +
  geom_line (data=spp22, aes(x=elev2 , y= Abundance22, color ="grey"), size =2) +
  geom_ribbon(data=spp22,aes(x=elev2 , y= Abundance22, ymin = l22, ymax = u22), alpha = 0.5, fill= "grey") +
  
  annotate("text", x = 2750, y = 5, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Malaconotidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 5))

Figure_S6_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Dryoscopus gambensis (31.9g)", "Laniarius aethiopicus (49.44g)","Laniarius luehderi (42.9g)", 
                                "Laniarius poensis (44.9g)", "Telophorus dohertyi (35.02g)"), 
                     values=c( "brown"= "brown", "blue"="blue", "orange"= "orange", "black"="black",
                                        "grey"="grey"), name = "Species") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.7)), legend.title=element_text(size=25, color="black"))  



ggsave(file = "Appendix_S8_FigureS6(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




######## Malaconotidae  -- (Bushshrike) ######
######## Foraging diet categories ###########
############################################

##Appendix S8: Figure S6(c)
ggplot(Bushshrikes_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("brown", "grey", "blue", "red", "orange")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Malaconotidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Dryoscopus gambensis" = "Dryoscopus\ngambensis","Laniarius luehderi" = "Laniarius\nluehderi", 
                              "Laniarius aethiopicus" = "Laniarius\naethiopicus", 
                              "Laniarius poensis" = "Laniarius\npoensis", "Telophorus dohertyi" = "Telophorus\ndohertyi"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 





ggsave(file = "Appendix_S8_FigureS6(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


######## Malaconotidae -- (BushshrikeS) ######
######## Foraging forest strata ##############

##Appendix S8: Figure S6(d)
ggplot(Bushshrikes_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("orange", "purple", "grey", "black")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Malaconotidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Dryoscopus gambensis" = "Dryoscopus\ngambensis","Laniarius luehderi" = "Laniarius\nluehderi", 
                              "Laniarius aethiopicus" = "Laniarius\naethiopicus", "Laniarius poensis" = "Laniarius\npoensis"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 





ggsave(file =  "Appendix_S8_FigureS6(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



########################################################################
###################### Family: Cuculidae --- (cuckoos) ##############
########-------------------- 3 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################


###### Centropus monachus #######
################################

###### Expected abundance

Abundance10 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[10]  + HCDSM_Virunga_63spp$mean$beta1[10]  * elev + HCDSM_Virunga_63spp$mean$beta2[10] * elev^2))
Ab10mx <- max(Abundance10)
Abundance10s <- Abundance10/Ab10mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l10<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[10]  + HCDSM_Virunga_63spp$q2.5$beta1[10] * elev + HCDSM_Virunga_63spp$q2.5$beta2[10]* elev^2))
l10s <- l10/Ab10mx

##### Upper 97.5 CI
u10 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[10]  + HCDSM_Virunga_63spp$q97.5$beta1[10] * elev + HCDSM_Virunga_63spp$q97.5$beta2[10]* elev^2))

u10s <- u10/Ab10mx

###### create a data frame
spp10 <- data.frame (Abundance10, Abundance10s, elev, l10, l10s, u10, u10s)


###### Chrysococcyx klaas #######
#################################

Abundance11 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[11]  + HCDSM_Virunga_63spp$mean$beta1[11]  * elev + HCDSM_Virunga_63spp$mean$beta2[11] * elev^2))
Ab11mx <- max(Abundance11)
Abundance11s <- Abundance11/Ab11mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l11<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[11]  + HCDSM_Virunga_63spp$q2.5$beta1[11] * elev + HCDSM_Virunga_63spp$q2.5$beta2[11]* elev^2))
l11s <- l11/Ab11mx

##### Upper 97.5 CI
u11 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[11]  + HCDSM_Virunga_63spp$q97.5$beta1[11] * elev + HCDSM_Virunga_63spp$q97.5$beta2[11]* elev^2))
u11s <- u11/Ab11mx

###### create a data frame
spp11 <- data.frame (Abundance11, Abundance11s, elev, l11, l11s, u11, u11s)


###### Cuculus solitarius #######
#################################

Abundance12 <- (exp(HCDSM_Virunga_63spp$mean$beta0[12]  + HCDSM_Virunga_63spp$mean$beta1[12]  * elev + HCDSM_Virunga_63spp$mean$beta2[12] * elev^2))
Ab12mx <- max(Abundance12)
Abundance12s <- Abundance12/Ab12mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l12<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[12]  + HCDSM_Virunga_63spp$q2.5$beta1[12] * elev + HCDSM_Virunga_63spp$q2.5$beta2[12]* elev^2))
l12s <- l12/Ab12mx

##### Upper 97.5 CI
u12 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[12]  + HCDSM_Virunga_63spp$q97.5$beta1[12] * elev + HCDSM_Virunga_63spp$q97.5$beta2[12]* elev^2))
u12s <- u12/Ab12mx

###### create a data frame
spp12 <- data.frame (Abundance12, Abundance12s, elev, l12s, l12s, u12, u12s)




###### Elevational niche overlap- Estimation ################
#########Family: Cuculidae --- (cuckoos) ################
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
#####same relative scale (maximum abundance = 1)#############
#############################################################



######################################################
#### Integration function: Centropus monachus ########
#####################################################

inter10 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[10]  + 
                              HCDSM_Virunga_63spp$mean$beta1[10]  * x + HCDSM_Virunga_63spp$mean$beta2[10] * x^2)/Ab10mx}

### Area under the curve: Centropus monachus 

integrate(inter10, -2.24,3.16) 


######################################################
#### Integration function: Chrysococcyx klaas ########
#####################################################
inter11 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[11]  + 
                              HCDSM_Virunga_63spp$mean$beta1[11]  * x + HCDSM_Virunga_63spp$mean$beta2[11] * x^2)/Ab11mx}


### Area under the curve: Chrysococcyx klaas 
integrate(inter11, -2.24,3.16) 


######################################################
#### Integration function: Cuculus solitarius ########
#####################################################

inter12 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[12]  + 
                              HCDSM_Virunga_63spp$mean$beta1[12]  * x + HCDSM_Virunga_63spp$mean$beta2[12] * x^2)/Ab12mx}


### Area under the curve: Cuculus solitarius

integrate(inter12, -2.24,3.16) 



##  Centropus monachus & Chrysococcyx klaas ###
####-------------------------------------#####
x <- elev  ## Standardized elevation  
y10 <- Abundance10s ## Standardized abundance: Centropus monachus
y11 <- Abundance11s ## Standardized abundance: Chrysococcyx klaas
### Computing area of overlap
spps10n11 <- approxfun(x, pmin(y10,y11), ties = "mean")
integrate(spps10n11, min(x), max(x))


##  Centropus monachus & Cuculus solitarius ###
####-------------------------------------#####
x <- elev  ## Standardized elevation  
y10 <- Abundance10s ## Standardized abundance: Centropus monachus
y12 <- Abundance12s ## Standardized abundance: Cuculus solitarius
### Computing area of overlap
spps10n12 <- approxfun(x, pmin(y10,y12),ties = "mean")
integrate(spps10n12, min(x), max(x))



##  Chrysococcyx klaas & Cuculus solitarius ###
####-------------------------------------#####
x <- elev  ## Standardized elevation  
y11 <- Abundance11s ## Standardized abundance: Chrysococcyx klaas
y12 <- Abundance12s ## Standardized abundance: Cuculus solitarius
### Computing area of overlap
spps11n12 <- approxfun(x, pmin(y11,y12),ties = "mean")
integrate(spps11n12, min(x), max(x))


##Appendix S8: Figure S7(a)
##Standardized

Figure_S7_a = ggplot () +
  geom_line (data=spp10, aes(x=elev2 , y= Abundance10s, color ="black"), size =2) +
  geom_line (data=spp11, aes(x=elev2 , y= Abundance11s, color ="orange"), size =2) +
  geom_line (data=spp12, aes(x=elev2 , y= Abundance12s, color ="blue"), size =2) +
  
  
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Cuculidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1))

Figure_S7_a  + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c( "Centropus monachus (201.31g)" , "Chrysococcyx klaas (27.37g)", "Cuculus solitarius (76.73g)"), 
                     values=c( "black"= "black", "orange"= "orange", "blue"="blue"), name= "Species ") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.7,0.6)), legend.title=element_text(size=25, color="black")) 


ggsave(file = "Appendix_S8_Figure_S7(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S7(b)
## Confidence intervals

Figure_S7_b = ggplot () +
  
  geom_line (data=spp10, aes(x=elev2 , y= Abundance10, color ="black"), size =2) +
  geom_ribbon(data=spp10,aes(x=elev2 , y= Abundance10, ymin = l10, ymax = u10), alpha = 0.5, fill= "black")+
  geom_line (data=spp11, aes(x=elev2 , y= Abundance11, color ="orange"), size =2) +
  geom_ribbon(data=spp11,aes(x=elev2 , y= Abundance11, ymin = l11, ymax = u11), alpha = 0.5, fill= "orange")+
  geom_line (data=spp12, aes(x=elev2 , y= Abundance12, color ="blue"), size =2) +
  geom_ribbon(data=spp12,aes(x=elev2 , y= Abundance12, ymin = l12, ymax = u12), alpha = 0.5, fill= "blue") +
  
  annotate("text", x = 2750, y = 2, hjust = 0, size = 10, family = "Times New Roman", 
           label = " Cuculidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 2))

Figure_S7_b  + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c( "Centropus monachus (201.31g)" , "Chrysococcyx klaas (27.37g)", "Cuculus solitarius (76.73g)"), 
                     values=c( "black"= "black", "orange"= "orange", "blue"="blue"), name= "Species ") +
  
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.7,0.6)), legend.title=element_text(size=25, color="black"))   



ggsave(file = "Appendix_S8_FigureS7(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


######## Cuculidae -- (Cuckoos) #########
######## Foraging diet categories ######
########################################

##Appendix S8: Figure S7(c)
ggplot(Cuckoos_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c( "blue","grey", "red", "orange" )) +
  theme_bw() +
  ggtitle("Foraging diet categories: Cuculidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c( "Centropus monachus" = "Centropus\n monachus"  , "Cuculus solitarius" = "Cuculus\nsolitarius",
                               "Chrysococcyx klaas" = "Chrysococcyx\nklaas"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 


ggsave(file = "Appendix_S8_FigureS7(C).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


######## Cuculidae -- (Cuckoos) ########
######## Foraging forest strata ########
########################################

##Appendix S8: Figure S6(d)

ggplot(Cuckoos_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange", "purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Cuculidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c( "Centropus monachus" = "Centropus\n monachus"  , "Cuculus solitarius" = "Cuculus\nsolitarius",
                               "Chrysococcyx klaas" = "Chrysococcyx\nklaas"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 


ggsave(file = "Appendix_S8_FigureS7(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



#####################################################################
###################### Family: Pycnonotidae-- (Bulbuls)##############
########-------------------- 3 species ---------------------#########
#####################################################################
############ Species abundance-Elevation curves #####################



###### Eurillas latirostris #####
#################################

###### Expected abundance
Abundance42 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[42]  + HCDSM_Virunga_63spp$mean$beta1[42]  * elev + HCDSM_Virunga_63spp$mean$beta2[42] * elev^2))
Ab42mx <- max(Abundance42)
Abundance42s <- Abundance42/Ab42mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l42<-   (exp(HCDSM_Virunga_63spp$q2.5$beta0[42]  + HCDSM_Virunga_63spp$q2.5$beta1[42] * elev + HCDSM_Virunga_63spp$q2.5$beta2[42]* elev^2))
l42s <- l42/Ab42mx

##### Upper 97.5 CI
u42 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[42]  + HCDSM_Virunga_63spp$q97.5$beta1[42] * elev + HCDSM_Virunga_63spp$q97.5$beta2[42]* elev^2))
u42s <- u42/Ab42mx

###### create a data frame
spp42 <- data.frame (Abundance42, Abundance42s, elev, l42, l42s, u42, u42s)

###### Andropadus nigriceps ######
#################################

###### Expected abundance
Abundance43 <- (exp(HCDSM_Virunga_63spp$mean$beta0[43]  + HCDSM_Virunga_63spp$mean$beta1[43]  * elev + HCDSM_Virunga_63spp$mean$beta2[43] * elev^2))
Ab43mx <- max(Abundance43)
Abundance43s <- Abundance43/Ab43mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l43<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[43]  + HCDSM_Virunga_63spp$q2.5$beta1[43] * elev + HCDSM_Virunga_63spp$q2.5$beta2[43]* elev^2))
l43s <- l43/Ab43mx
##### Upper 97.5 CI
u43 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[43]  + HCDSM_Virunga_63spp$q97.5$beta1[43] * elev + HCDSM_Virunga_63spp$q97.5$beta2[43]* elev^2))
u43s <- u43/Ab43mx
###### create a data frame
spp43 <- data.frame (Abundance43, Abundance43s, elev, l43, l43s, u43, u43s)


###### Andropadus nigriceps #######
##################################

###### Expected abundance
Abundance44 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[44]  + HCDSM_Virunga_63spp$mean$beta1[44]  * elev + HCDSM_Virunga_63spp$mean$beta2[44] * elev^2))
Ab44mx <- max(Abundance44)
Abundance44s <- Abundance44/Ab44mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l44<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[44]  + HCDSM_Virunga_63spp$q2.5$beta1[44] * elev + HCDSM_Virunga_63spp$q2.5$beta2[44]* elev^2))
l44s <- l44/Ab44mx

##### Upper 97.5 CI
u44 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[44]  + HCDSM_Virunga_63spp$q97.5$beta1[44] * elev + HCDSM_Virunga_63spp$q97.5$beta2[44]* elev^2))
u44s <- u44/Ab44mx

###### create a data frame
spp44 <- data.frame (Abundance44, Abundance44s, elev, l44, l44s, u44, u44s)



###### Elevational niche overlap- Estimation ################
######### Family: Pycnonotidae-- (Bulbuls) ###################
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
##### same relative scale (maximum abundance = 1)############
#############################################################



######################################################
#### Integration function: Eurillas latirostris ######
#####################################################

inter42 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[42]  + 
                              HCDSM_Virunga_63spp$mean$beta1[42]  * x + HCDSM_Virunga_63spp$mean$beta2[42] * x^2)/Ab42mx}

### Area under the curve: Eurillas latirostris

integrate(inter42, -2.24,3.16) 


######################################################
#### Integration function: Andropadus nigriceps ######
#####################################################

inter43 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[43]  + 
                              HCDSM_Virunga_63spp$mean$beta1[43]  * x + HCDSM_Virunga_63spp$mean$beta2[43] * x^2)/Ab43mx}

### Area under the curve: Andropadus nigriceps

integrate(inter43, -2.24,3.16) 


######################################################
#### Integration function: Pycnonotus barbatus#######
#####################################################

inter44 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[44]  + 
                              HCDSM_Virunga_63spp$mean$beta1[44]  * x + HCDSM_Virunga_63spp$mean$beta2[44] * x^2)/Ab44mx}

### Area under the curve: Pycnonotus barbatus

integrate(inter44, -2.24,3.16) 


##  Eurillas latirostris & Andropadus nigriceps ###
####-------------------------------------------#####
x <- elev ## Standardized elevation 
y42 <- Abundance42s ## Standardized abundance: Eurillas latirostris
y43 <- Abundance43s ## Standardized abundance: Andropadus nigriceps
### Computing area of overlap
spps42n43 <- approxfun(x, pmin(y42,y43), ties = "mean")
integrate(spps42n43, min(x), max(x),subdivisions = 2000)



##  Eurillas latirostris & Pycnonotus barbatus ###
####-------------------------------------------#####
x <- elev ## Standardized elevation 
y42 <- Abundance42s ## Standardized abundance: Eurillas latirostris
y44 <- Abundance44s ## Standardized abundance: Pycnonotus barbatus
### Computing area of overlap
spps42n44 <- approxfun(x, pmin(y42,y44), ties = "mean")
integrate(spps42n44, min(x), max(x))


##  Andropadus nigriceps & Pycnonotus barbatus ###
####------------------------------------------#####
x <- elev ## Standardized elevation 
y43 <- Abundance43s ## Standardized abundance: Andropadus nigriceps
y44 <- Abundance44s ## Standardized abundance: Pycnonotus barbatus
### Computing area of overlap
spps43n44  <- approxfun(x, pmin(y43,y44), ties = "mean")
integrate(spps43n44 , min(x), max(x),subdivisions = 2000)



##Appendix S8: Figure S8(a)
##Standardized

Figure_S8_a = ggplot () +
  geom_line (data=spp42, aes(x=elev2 , y= Abundance42s, color ="orange"), size =2) +
  geom_line (data=spp43, aes(x=elev2 , y= Abundance43s, color ="blue"), size =2) +
  geom_line (data=spp44, aes(x=elev2 , y= Abundance44s, color ="black"), size =2) +
  
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = " Pycnonotidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1))

Figure_S8_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Eurillas latirostris (26.2g)", "Andropadus nigriceps (32.8g)","Pycnonotus barbatus (32.1g)"), 
                     values=c( "orange"= "orange", "blue"="blue", "black"= "black"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.7,0.6)), legend.title=element_text(size=25, color="black")) 



ggsave(file = "Appendix_S8_FigureS8(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


##Appendix S8: Figure S8(b)
## Confidence intervals

Figure_S8_b = ggplot () +
  geom_line (data=spp42, aes(x=elev2 , y= Abundance42, color ="orange"), size =2) +
  geom_ribbon(data=spp42,aes(x=elev2 , y= Abundance42, ymin = l42, ymax = u42), alpha = 0.5, fill= "orange")+
  geom_line (data=spp43, aes(x=elev2 , y= Abundance43, color ="blue"), size =2) +
  geom_ribbon(data=spp43,aes(x=elev2 , y= Abundance43, ymin = l43, ymax = u43), alpha = 0.5, fill= "blue") +
  geom_line (data=spp44, aes(x=elev2 , y= Abundance44, color ="black"), size =2) +
  geom_ribbon(data=spp44,aes(x=elev2 , y= Abundance44, ymin = l44, ymax = u44), alpha = 0.5, fill= "black") +
  
  
  annotate("text", x = 2750, y = 50, hjust = 0, size = 10, family = "Times New Roman", 
           label = " Pycnonotidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 50))


Figure_S8_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Eurillas latirostris (26.2g)", "Andropadus nigriceps (32.8g)","Pycnonotus barbatus (32.1g)"), 
                     values=c( "orange"= "orange", "blue"="blue", "black"= "black"), name = "Species ") +
  

  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.7,0.6)), legend.title=element_text(size=25, color="black"))   



ggsave(file = "Appendix_S8_FigureS8(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)





######## Pycnonotidae-- (Bulbuls) ######
######## Foraging diet categories ######
########################################

##Appendix S8: Figure S8(c)


ggplot(Bulbuls_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("orange", "grey", "brown", "red", "green","black","purple")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Pycnonotidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Eurillas latirostris" = "Eurillas\nlatirostris","Andropadus nigriceps" = "Andropadus\nnigriceps",
                              "Pycnonotus barbatus" = " Pycnonotus\nbarbatus"), name= "Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 


ggsave(file = "Appendix_S8_FigureS8(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)





########Pycnonotidae-- (Bulbuls) ######
######## Foraging forest strata########
#######################################

##Appendix S8: Figure S8(d)


ggplot(Bulbuls_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange", "purple", "brown")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Pycnonotidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Eurillas latirostris" = "Eurillas\nlatirostris","Andropadus nigriceps" = "Andropadus\nnigriceps",
                              "Pycnonotus barbatus" = " Pycnonotus\nbarbatus"), name= "Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 



ggsave(file = "Appendix_S8_FigureS8(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)





######################################################################
################## Family: Columbidae -- (Doves and Pigeons)##########
########-------------------- 3 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################



###### Columba arquatrix #####
#############################

###### Expected abundance

Abundance7 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[7]  + HCDSM_Virunga_63spp$mean$beta1[7]  * elev + HCDSM_Virunga_63spp$mean$beta2[7] * elev^2))
Ab7mx <- max(Abundance7)
Abundance7s <- Abundance7/Ab7mx

###### 95% credible interval (CI)

##### Lower 2.5 CIl7s <- l7/Ab7mx

l7<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[7]  + HCDSM_Virunga_63spp$q2.5$beta1[7] * elev + HCDSM_Virunga_63spp$q2.5$beta2[7]* elev^2))


##### Upper 97.5 CI
u7 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[7]  + HCDSM_Virunga_63spp$q97.5$beta1[7] * elev + HCDSM_Virunga_63spp$q97.5$beta2[7]* elev^2))
u7s <- u7/Ab7mx

###### create a data frame
spp7 <- data.frame (Abundance7, Abundance7s, elev2, l7, l7s, u7, u7s)



###### Streptopelia lugens ######
#################################

###### Expected abundance

Abundance8 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[8]  + HCDSM_Virunga_63spp$mean$beta1[8]  * elev + HCDSM_Virunga_63spp$mean$beta2[8] * elev^2))
Ab8mx <- max(Abundance8)
Abundance8s <- Abundance8/Ab8mx
###### 95% credible interval (CI)

##### Lower 2.5 CI

l8<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[8]  + HCDSM_Virunga_63spp$q2.5$beta1[8] * elev + HCDSM_Virunga_63spp$q2.5$beta2[8]* elev^2))
l8s <- l8/Ab8mx

##### Upper 97.5 CI
u8 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[8]  + HCDSM_Virunga_63spp$q97.5$beta1[8] * elev + HCDSM_Virunga_63spp$q97.5$beta2[8]* elev^2))
u8s <- u8/Ab8mx

###### create a data frame
spp8 <- data.frame (Abundance8, Abundance8s, elev, l8, l8s, u8, u8s)


###### Streptopelia semitorquata #####
#####################################

###### Expected abundance

Abundance9 <- (exp(HCDSM_Virunga_63spp$mean$beta0[9]  + HCDSM_Virunga_63spp$mean$beta1[9]  * elev + HCDSM_Virunga_63spp$mean$beta2[9] * elev^2))
Ab9mx <- max(Abundance9)
Abundance9s <- Abundance9/Ab9mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l9<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[9]  + HCDSM_Virunga_63spp$q2.5$beta1[9] * elev + HCDSM_Virunga_63spp$q2.5$beta2[9]* elev^2))
l9s <- l9/Ab9mx

##### Upper 97.5 CI
u9 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[9]  + HCDSM_Virunga_63spp$q97.5$beta1[9] * elev + HCDSM_Virunga_63spp$q97.5$beta2[9]* elev^2))
u9s <- u9/Ab9mx

###### create a data frame
spp9 <- data.frame (Abundance9, Abundance9s, elev2, l9, l9s, u9, u9s)




###### Elevational niche overlap- Estimation ################
#########Family: Columbidae -- (Doves and Pigeons) ##########
######--------------------------------------------###########
############## Area under the curve #########################
######--------------------------------------------###########
###### Standardized species abundance-Elevation curves ######
#####same relative scale (maximum abundance = 1)#############
#############################################################


#####################################################
#### Integration function: Columba arquatrix#########
#####################################################


inter7 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[7]  + 
                             HCDSM_Virunga_63spp$mean$beta1[7]  * x + HCDSM_Virunga_63spp$mean$beta2[7] * x^2)/Ab7mx}

### Area under the curve: Columba arquatrix

integrate(inter7, -2.24,3.16) 



#######################################################
#### Integration function: Streptopelia lugens ########
#######################################################

inter8 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[8]  + 
                             HCDSM_Virunga_63spp$mean$beta1[8]  * x + HCDSM_Virunga_63spp$mean$beta2[8] * x^2)/Ab8mx}

### Area under the curve: Streptopelia lugens

integrate(inter8, -2.24,3.16) 

##########################################################
#### Integration function: Streptopelia semitorquata #####
#########################################################
inter9 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[9]  + 
                             HCDSM_Virunga_63spp$mean$beta1[9]  * x + HCDSM_Virunga_63spp$mean$beta2[9] * x^2)/Ab9mx}

### Area under the curve: Streptopelia semitorquata

integrate(inter9, -2.24,3.16) 


##   Columba arquatrix & Streptopelia lugens ###
####-----------------------------------------#####
x <- elev ## Standardized elevation  
y7 <- Abundance7s ## Standardized abundance: Columba arquatrix
y8 <- Abundance8s ## Standardized abundance: Streptopelia lugens 
### Computing area of overlap
spps7n8 <- approxfun(x, pmin(y7,y8), ties = "mean")
integrate(spps7n8, min(x), max(x))


##   Columba arquatrix & Streptopelia semitorquata ###
####-----------------------------------------------#####
x <- elev ## Standardized elevation 
y7 <- Abundance7s ## Standardized abundance: Columba arquatrix
y9 <- Abundance9s ## Standardized abundance: Streptopelia semitorquata 
### Computing area of overlap
spps7n9 <- approxfun(x, pmin(y7,y9), ties = "mean")
integrate(spps7n9, min(x), max(x))



##  Streptopelia lugens & Streptopelia semitorquata ###
####-----------------------------------------------#####
x <- elev ## Standardized elevation 
y8 <- Abundance8s ## Standardized abundance: Streptopelia lugens
y9 <- Abundance9s ## Standardized abundance: Streptopelia semitorquata 
### Computing area of overlap
spps8n9 <- approxfun(x, pmin(y8,y9), ties = "mean")
integrate(spps8n9, min(x), max(x))




##Appendix S8: Figure S9(a)
##Standardized


Figure_S9_a = ggplot () +
  
  geom_line (data=spp7, aes(x=elev2 , y= Abundance7s, color ="orange"), size =2) +
  geom_line (data=spp8, aes(x=elev2 , y= Abundance8s, color ="blue"), size =2) +
  geom_line (data=spp9, aes(x=elev2 , y= Abundance9s, color ="black"), size =2) +
  
  
  annotate("text", x = 2000, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Columbidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 1))

Figure_S9_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Columba arquatrix (300g)",  "Streptopelia lugens (155g)", "Streptopelia semitorquata (176g)"), 
                     values=c( "orange"= "orange", "blue" = "blue", "black" = "black"), name = "Species ") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.85)), legend.title=element_text(size=25, color="black")) 


ggsave(file = "Appendix_S8_FigureS9(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S9(b)
##Confidence intervals

Figure_S9_b = ggplot () +
  
  geom_line (data=spp7, aes(x=elev2 , y= Abundance7, color ="orange"), size =2) +
  geom_ribbon(data=spp7,aes(x=elev2 , y= Abundance7, ymin = l7, ymax = u7), alpha = 0.5, fill= "orange")+
  geom_line (data=spp8, aes(x=elev2 , y= Abundance8, color ="blue"), size =2) +
  geom_ribbon(data=spp8,aes(x=elev2 , y= Abundance8, ymin = l8, ymax = u8), alpha = 0.5, fill= "blue")+
  geom_line (data=spp9, aes(x=elev2 , y= Abundance9, color ="black"), size =2) +
  geom_ribbon(data=spp9,aes(x=elev2 , y= Abundance9, ymin = l9, ymax = u9), alpha = 0.5, fill= "black")+
  
  
  annotate("text", x = 2750, y = 10, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Columbidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 10))

Figure_S9_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Columba arquatrix (300g)",  "Streptopelia lugens (155g)", "Streptopelia semitorquata (176g)"), 
                     values=c( "orange"= "orange", "blue" = "blue", "black" = "black"), name = "Species ") +
  
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.3,0.7)), legend.title=element_text(size=25, color="black")) 


ggsave(file = "Appendix_S8_FigureS9(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Columbidae -- (Doves and Pigeons) ######
######## Foraging diet categories ##############
###############################################

##Appendix S8: Figure S9(c)


ggplot(DovesnPigeons_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "orange", "purple", "brown")) +
  theme_bw() +
  ggtitle("Foraging vertical diet: Columbidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Columba arquatrix" = "Columba\narquatrix ", "Streptopelia lugens" = "Streptopelia\nlugens",
                             "Streptopelia semitorquata" = "Streptopelia\nsemitorquata"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 


ggsave(file = "Appendix_S8_FigureS9(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Columbidae -- (Doves and Pigeons) ######
######## Foraging forest strata################
###############################################

##Appendix S8: Figure S9(d)


ggplot(DovesnPigeons_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange","purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Columbidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Columba arquatrix" = "Columba\narquatrix ", "Streptopelia lugens" = "Streptopelia\nlugens",
                              "Streptopelia semitorquata" = "Streptopelia\nsemitorquata"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 





ggsave(file = "Appendix_S8_FigureS9(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




#################################################################
##################Family: Platysteiridae- (Batises)#############
########-------------------- 2 species ---------------------####
################################################################
############ Species abundance-Elevation curves ################



###### Batis diops #####
########################

###### Expected abundance
Abundance24 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[24]  + HCDSM_Virunga_63spp$mean$beta1[24]  * elev + HCDSM_Virunga_63spp$mean$beta2[24] * elev^2))
Ab24mx <- max(Abundance24)
Abundance24s <- Abundance24/Ab24mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l24<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[24]  + HCDSM_Virunga_63spp$q2.5$beta1[24] * elev + HCDSM_Virunga_63spp$q2.5$beta2[24]* elev^2))
l24s <- l24/Ab24mx

##### Upper 97.5 CI
u24 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[24]  + HCDSM_Virunga_63spp$q97.5$beta1[24] * elev + HCDSM_Virunga_63spp$q97.5$beta2[24]* elev^2))
u24s <- u24/Ab24mx

###### create a data frame
spp24 <- data.frame (Abundance24, Abundance24s, elev2, l24, l24s, u24, u24s)


##### Batis molitor #########
#############################

###### Expected abundance
Abundance25 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[25]  + HCDSM_Virunga_63spp$mean$beta1[25]  * elev + HCDSM_Virunga_63spp$mean$beta2[25] * elev^2))
Ab25mx <- max(Abundance25)
Abundance25s <- Abundance25/Ab25mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l25<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[25]  + HCDSM_Virunga_63spp$q2.5$beta1[25] * elev + HCDSM_Virunga_63spp$q2.5$beta2[25]* elev^2))
l25s <- l25/Ab25mx

##### Upper 97.5 CI
u25 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[25]  + HCDSM_Virunga_63spp$q97.5$beta1[25] * elev + HCDSM_Virunga_63spp$q97.5$beta2[25]* elev^2))
u25s <- u25/Ab25mx

###### create a data frame
spp25 <- data.frame (Abundance25, Abundance25s, elev, l25, l25s, u25, u25s)



##################################################
#### Integration function: Batis diops ###########
##################################################

inter24 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[24]  + 
                             HCDSM_Virunga_63spp$mean$beta1[24]  * x + HCDSM_Virunga_63spp$mean$beta2[24] * x^2)/Ab24mx}

### Area under the curve: Batis diops 

integrate(inter24, -2.24,3.16) 



####################################################
#### Integration function: Batis molitor ###########
####################################################

inter25 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[25]  + 
                              HCDSM_Virunga_63spp$mean$beta1[25]  * x + HCDSM_Virunga_63spp$mean$beta2[25] * x^2)/Ab25mx}


### Area under the curve: Batis molitor

integrate(inter25, -2.24,3.16) 





##  Batis diops & Batis molitor  ###
####-----------------------------#####
x <- elev  ## Standardized elevation  
y24 <- Abundance24s ## Standardized abundance: Batis diops
y25 <- Abundance25s ## Standardized abundance: Batis molitor
### Computing area of overlap
spps24n25 <- approxfun(x, pmin(y24,y25), ties = "mean")
integrate(spps24n25, min(x), max(x))




##Appendix S8: Figure S10(a)
##Standardized

Figure_S10_a = ggplot () +
  
  geom_line (data=spp24, aes(x=elev2 , y= Abundance24s, color ="orange"), size =2) +
  geom_line (data=spp25, aes(x=elev2 , y= Abundance25s, color ="blue"), size =2) +
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = " Platysteiridae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 1))

Figure_S10_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Batis diops (12.7g)",  "Batis molitor (11.64g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.7,0.6)), legend.title=element_text(size=25, color="black")) 


ggsave(file = "Appendix_S8_FigureS10(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



##Appendix S8: Figure S10(b)
##Confidence intervals

Figure_S10_b = ggplot () +
  
  
  geom_line (data=spp24, aes(x=elev2 , y= Abundance24, color ="orange"), size =2) +
  geom_ribbon(data=spp24,aes(x=elev2 , y= Abundance24, ymin = l24, ymax = u24), alpha = 0.5, fill= "orange")+
  geom_line (data=spp25, aes(x=elev2 , y= Abundance25, color ="blue"), size =2) +
  geom_ribbon(data=spp25,aes(x=elev2 , y= Abundance25, ymin = l25, ymax = u25), alpha = 0.5, fill= "blue")+
  
  
  annotate("text", x = 2750, y = 15, hjust = 0, size = 10, family = "Times New Roman", 
           label = " Platysteiridae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 15))

  
  Figure_S10_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Batis diops (12.7g)",  "Batis molitor (11.64g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.7,0.6)), legend.title=element_text(size=25, color="black")) 


ggsave(file = "Appendix_S8_FigureS10(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Platysteiridae-- (Batises) ######
######## Foraging diet categories ########
##########################################

##Appendix S8: Figure S10(c)


ggplot(Batises_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey")) +
  theme_bw() +
  ggtitle("Foraging vertical diet: Platysteiridae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  
  scale_x_discrete(labels = c("Batis diops" = "Batis\ndiops ", "Batis molitor" = "Batis\nmolitor"), name= " Species") +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 



ggsave(file = "Appendix_S8_FigureS10(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




######## Platysteiridae-- (Batises) ######
######## Foraging forest strata ##########
##########################################

##Appendix S8: Figure S10(d)


ggplot(Batises_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("black", "orange","purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Platysteiridae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Batis diops" = "Batis\ndiops ", "Batis molitor" = "Batis\nmolitor"
                              ), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 


ggsave(file = "Appendix_S8_FigureS10(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)





#################################################################
################## Family: Ploceidae- (Weavers)##################
########-------------------- 2 species ---------------------####
################################################################
############ Species abundance-Elevation curves ################



###### Ploceus alienus #####
#############################

###### Expected abundance

Abundance39 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[39]  + HCDSM_Virunga_63spp$mean$beta1[39]  * elev + HCDSM_Virunga_63spp$mean$beta2[39] * elev^2))
Ab39mx <- max(Abundance39)
Abundance39s <- Abundance39/Ab39mx
###### 95% credible interval (CI)

##### Lower 2.5 CI
l39<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[39]  + HCDSM_Virunga_63spp$q2.5$beta1[39] * elev + HCDSM_Virunga_63spp$q2.5$beta2[39]* elev^2))
l39s <- l39/Ab39mx


##### Upper 97.5 CI
u39 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[39]  + HCDSM_Virunga_63spp$q97.5$beta1[39] * elev + HCDSM_Virunga_63spp$q97.5$beta2[39]* elev^2))
u39s <- u39/Ab39mx

###### create a data frame
spp39 <- data.frame (Abundance39, Abundance39s, elev2, l39, l39s, u39, u39s)



###### Ploceus baglafecht #####
##############################

###### Expected abundance

Abundance40 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[40]  + HCDSM_Virunga_63spp$mean$beta1[40]  * elev + HCDSM_Virunga_63spp$mean$beta2[40] * elev^2))
Ab40mx <- max(Abundance40)
Abundance40s <- Abundance40/Ab40mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l40<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[40]  + HCDSM_Virunga_63spp$q2.5$beta1[40] * elev + HCDSM_Virunga_63spp$q2.5$beta2[40]* elev^2))
l40s <- l40/Ab40mx


##### Upper 97.5 CI
u40 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[40]  + HCDSM_Virunga_63spp$q97.5$beta1[40] * elev + HCDSM_Virunga_63spp$q97.5$beta2[40]* elev^2))
u40s <- u40/Ab40mx

###### create a data frame
spp40 <- data.frame (Abundance40, Abundance40s, elev, l40, l40s, u40, u40s)




######################################################
#### Integration function:  Ploceus alienus ##########
#####################################################

inter39 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[39]  + 
                              HCDSM_Virunga_63spp$mean$beta1[39]  * x + HCDSM_Virunga_63spp$mean$beta2[39] * x^2)/Ab39mx}


### Area under the curve: Ploceus alienus
integrate(inter39, -2.24,3.16) 


######################################################
#### Integration function:  Ploceus baglafecht #######
#####################################################

inter40 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[40]  + 
                              HCDSM_Virunga_63spp$mean$beta1[40]  * x + HCDSM_Virunga_63spp$mean$beta2[40] * x^2)/Ab40mx}

### Area under the curve: Ploceus baglafecht

integrate(inter40, -2.24,3.16) 



##  Ploceus alienus & Ploceus baglafecht  ##
####-------------------------------------#####
x <- elev  ## Standardized elevation
y39 <- Abundance39s  ## Standardized abundance: Ploceus alienus
y40 <- Abundance40s  ## Standardized abundance: Ploceus baglafecht
### Computing area of overlap
spps39n40 <- approxfun(x, pmin(y39,y40), ties = "mean")
integrate(spps39n40, min(x), max(x))



##Appendix S8: Figure S11(a)
##Standardized

Figure_S11_a = ggplot () +
  
  geom_line (data=spp39, aes(x=elev2 , y= Abundance39s, color ="orange"), size =2) +
  geom_line (data=spp40, aes(x=elev2 , y= Abundance40s, color ="blue"), size =2) +
  
  
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Ploceidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 1))

Figure_S11_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Ploceus alienus (22.2g)",  "Ploceus baglafecht (31.6g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species ") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.7)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS11(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)





##Appendix S8: Figure S11(b)
## Confidence intervals

Figure_S11_b = ggplot () +
  
  
  geom_line (data=spp39, aes(x=elev2 , y= Abundance39, color ="orange"), size =2) +
  geom_ribbon(data=spp39,aes(x=elev2 , y= Abundance39, ymin = l39, ymax = u39), alpha = 0.5, fill= "orange")+
  geom_line (data=spp40, aes(x=elev2 , y= Abundance40, color ="blue"), size =2) +
  geom_ribbon(data=spp40,aes(x=elev2 , y= Abundance40, ymin = l40, ymax = u40), alpha = 0.5, fill= "blue")+
  
  
  annotate("text", x = 2750, y = 15, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Ploceidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 15))


Figure_S11_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Ploceus alienus (22.2g)",  "Ploceus baglafecht (31.6g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species ") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.7)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS11(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Ploceidae-- (Weavers) ########
######## Foraging diet categories #####
#######################################

##Appendix S8: Figure S11(c)


ggplot(Weavers_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("orange", "grey","black","purple")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Ploceidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  
  scale_x_discrete(labels = c("Ploceus alienus" = "Ploceus\nalienus ", "Ploceus alienus" = "Ploceus\nalienus"), name= " Species") +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 



ggsave(file = "Appendix_S8_FigureS11(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




######## Ploceidae-- (Weavers) #####
######## Foraging forest strata#####
####################################

##Appendix S8: Figure S11(d)


ggplot(Weavers_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c( "grey" , "black")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Ploceidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Ploceus alienus" = "Ploceus\nalienus ", "Ploceus alienus" = "Ploceus\nalienus"), name= " Species") +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 



ggsave(file = "Appendix_S8_FigureS11(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




#################################################################
################## Family: Fringillidae-- (Finches)##############
########-------------------- 2 species ---------------------####
################################################################
############ Species abundance-Elevation curves ################


###### Crithagra frontalis #####
################################

###### Expected abundance

Abundance15 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[15]  + HCDSM_Virunga_63spp$mean$beta1[15]  * elev + HCDSM_Virunga_63spp$mean$beta2[15] * elev^2))
Ab15mx <- max(Abundance15)
Abundance15s <- Abundance15/Ab15mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l15<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[15]  + HCDSM_Virunga_63spp$q2.5$beta1[15] * elev + HCDSM_Virunga_63spp$q2.5$beta2[15]* elev^2))
l15s <- l15/Ab15mx

##### Upper 97.5 CI
u15 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[15]  + HCDSM_Virunga_63spp$q97.5$beta1[15] * elev + HCDSM_Virunga_63spp$q97.5$beta2[15]* elev^2))
u15s <- u15/Ab15mx

###### create a data frame
spp15 <- data.frame (Abundance15, Abundance15s, elev2, l15, l15s, u15, u15s)



###### Crithagra gularis #####
#############################
Abundance16 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[16]  + HCDSM_Virunga_63spp$mean$beta1[16]  * elev + HCDSM_Virunga_63spp$mean$beta2[16] * elev^2))
Ab16mx <- max(Abundance16)
Abundance16s <- Abundance16/Ab16mx


###### 95% credible interval (CI)

##### Lower 2.5 CI
l16<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[16]  + HCDSM_Virunga_63spp$q2.5$beta1[16] * elev + HCDSM_Virunga_63spp$q2.5$beta2[16]* elev^2))
l16s <- l16/Ab16mx

##### Upper 97.5 CI
u16 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[16]  + HCDSM_Virunga_63spp$q97.5$beta1[16] * elev + HCDSM_Virunga_63spp$q97.5$beta2[16]* elev^2))
u16s <- u16/Ab16mx

###### create a data frame
spp16 <- data.frame (Abundance16, Abundance16s, elev, l16, l16s, u16, u16s)




######################################################
#### Integration function: Crithagra frontalis #######
#####################################################


inter15 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[15]  + 
                              HCDSM_Virunga_63spp$mean$beta1[15]  * x + HCDSM_Virunga_63spp$mean$beta2[15] * x^2)/Ab15mx}


### Area under the curve: Crithagra frontalis
integrate(inter15, -2.13,3.16) 



#####################################################
#### Integration function: Crithagra gularis#########
#####################################################


inter16 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[16]  + 
                              HCDSM_Virunga_63spp$mean$beta1[16]  * x + HCDSM_Virunga_63spp$mean$beta2[16] * x^2)/Ab16mx}

### Area under the curve: Crithagra gularis
integrate(inter16, -2.13,3.16) 



##  Crithagra frontalis & Crithagra gularis ##
####-------------------------------------#####
x <- elev  ## Standardized elevation 
y15 <- Abundance15s ## Standardized abundance: Crithagra frontalis
y16 <- Abundance16s ## Standardized abundance: Crithagra gularis
### Computing area of overlap
spps15n16 <- approxfun(x, pmin(y15,y16), ties = "mean")
integrate(spps15n16, min(x), max(x))  # Note there appears to be some numerical errors in the overlap calculations
# Area of overlap is greater than 1 in cases where there is complete overlap. This
# is fine




##Appendix S8: Figure S12(a)
##Standardized

Figure_S12_a = ggplot () +
  
  geom_line (data=spp15, aes(x=elev2 , y= Abundance15s, color ="orange"), size =2) +
  geom_line (data=spp16, aes(x=elev2 , y= Abundance16s, color ="blue"), size =2) +
  
  
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Fringillidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 1))

Figure_S12_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Crithagra frontalis (12.3g)",  "Crithagra gularis (16g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.6)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS12(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


##Appendix S8: Figure S12(b)
## Confidence intervals


Figure_S12_b = ggplot () +
  
  geom_line (data=spp15, aes(x=elev2 , y= Abundance15, color ="orange"), size =2) +
  geom_ribbon(data=spp15,aes(x=elev2 , y= Abundance15, ymin = l15, ymax = u15), alpha = 0.5, fill= "orange")+
  geom_line (data=spp16, aes(x=elev2 , y= Abundance16, color ="blue"), size =2) +
  geom_ribbon(data=spp16,aes(x=elev2 , y= Abundance16, ymin = l16, ymax = u16), alpha = 0.5, fill= "blue")+
  
  
  
  annotate("text", x = 2750, y = 7, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Fringillidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 7))

Figure_S12_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Crithagra frontalis (12.3g)",  "Crithagra gularis (16g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.6)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS12(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


######## Fringillidae-- (Finches) ######
######## Foraging diet categories #####
#######################################

##Appendix S8: Figure S12(c)

ggplot(Finches_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("orange", "grey", "brown","purple")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Fringillidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  
  scale_x_discrete(labels = c("Crithagra frontalis" = "Crithagra\nfrontalis", "Crithagra gularis" = "Crithagra\ngularis"), name= " Species") +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 


ggsave(file = "Appendix_S8_FigureS12(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)





######## Fringillidae-- (Finches)#####
######## Foraging forest strata #####
####################################

##Appendix S8: Figure S12(d)

ggplot(Finches_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Fringillidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Crithagra frontalis" = "Crithagra\nfrontalis", "Crithagra gularis" = "Crithagra\ngularis"), name= " Species") +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 



ggsave(file = "Appendix_S8_FigureS12(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




#################################################################
##################Family: Estrildidae- (Waxbills)################
########-------------------- 2 species ---------------------####
################################################################
############ Species abundance-Elevation curves ################



###### Estrilda astrild #####
#############################

###### Expected abundance


Abundance13 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[13]  + HCDSM_Virunga_63spp$mean$beta1[13]  * elev + HCDSM_Virunga_63spp$mean$beta2[13] * elev^2))
Ab13mx <- max(Abundance13)
Abundance13s <- Abundance13/Ab13mx
###### 95% credible interval (CI)

##### Lower 2.5 CI
l13<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[13]  + HCDSM_Virunga_63spp$q2.5$beta1[13] * elev + HCDSM_Virunga_63spp$q2.5$beta2[13]* elev^2))
l13s <- l13/Ab13mx

##### Upper 97.5 CI
u13 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[13]  + HCDSM_Virunga_63spp$q97.5$beta1[13] * elev + HCDSM_Virunga_63spp$q97.5$beta2[13]* elev^2))
u13s <- u13/Ab13mx

###### create a data frame

spp13 <- data.frame (Abundance13, Abundance13s, elev2, l13, l13s, u13, u13s)


###### Estrilda kandti #####
#############################


###### Expected abundance
Abundance14 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[14]  + HCDSM_Virunga_63spp$mean$beta1[14]  * elev + HCDSM_Virunga_63spp$mean$beta2[14] * elev^2))
Ab14mx <- max(Abundance14)
Abundance14s <- Abundance14/Ab14mx
###### 95% credible interval (CI)

##### Lower 2.5 CI
l14<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[14]  + HCDSM_Virunga_63spp$q2.5$beta1[14] * elev + HCDSM_Virunga_63spp$q2.5$beta2[14]* elev^2))
l14s <- l14/Ab14mx

##### Upper 97.5 CI
u14 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[14]  + HCDSM_Virunga_63spp$q97.5$beta1[14] * elev + HCDSM_Virunga_63spp$q97.5$beta2[14]* elev^2))
u14s <- u14/Ab14mx

###### create a data frame
spp14 <- data.frame (Abundance14, Abundance14s, elev, l14, l14s, u14, u14s)




######################################################
#### Integration function: Estrilda astrild###########
#####################################################

inter13 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[13]  + 
                              HCDSM_Virunga_63spp$mean$beta1[13]  * x + HCDSM_Virunga_63spp$mean$beta2[13] * x^2)/Ab13mx}

### Area under the curve: Estrilda astrild

integrate(inter13, -2.13,3.16) 


######################################################
#### Integration function: Estrilda kandti###########
####################################################

inter14 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[14]  + 
                              HCDSM_Virunga_63spp$mean$beta1[14]  * x + HCDSM_Virunga_63spp$mean$beta2[14] * x^2)/Ab14mx}

integrate(inter14, -2.13,3.16) 



##  Estrilda astrild & Estrilda kandti  ##
####-----------------------------------#####
x <- elev ## Standardized elevation 
y13 <- Abundance13s  ## Standardized abundance:  Estrilda astrild
y14 <- Abundance14s  ## Standardized abundance: Estrilda kandti
### Computing area of overlap
spps13n14 <- approxfun(x, pmin(y13,y14), ties = "mean")
integrate(spps13n14, min(x), max(x))




##Appendix S8: Figure S13(a)
##Standardized

Figure_S13_a = ggplot () +
  
  geom_line (data=spp13, aes(x=elev2 , y= Abundance13s, color ="orange"), size =2) +
  geom_line (data=spp14, aes(x=elev2 , y= Abundance14s, color ="blue"), size =2) +
  
  
  annotate("text", x = 2750, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Estrildidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 1))

Figure_S13_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Estrilda astrild (8.29g)",  "Estrilda kandti (7.48g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.6)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS13(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




##Appendix S8: Figure S13(b)
## Confidence intervals


Figure_S13_b = ggplot () +
  
  
  geom_line (data=spp13, aes(x=elev2 , y= Abundance13, color ="orange"), size =2) +
  geom_ribbon(data=spp13,aes(x=elev2 , y= Abundance13, ymin = l13, ymax = u13), alpha = 0.5, fill= "orange")+
  geom_line (data=spp14, aes(x=elev2 , y= Abundance14, color ="blue"), size =2) +
  geom_ribbon(data=spp14,aes(x=elev2 , y= Abundance14, ymin = l14, ymax = u14), alpha = 0.5, fill= "blue")+
  
  
  annotate("text", x = 2750, y = 60, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Estrildidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 60))


Figure_S13_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Estrilda astrild (8.29g)",  "Estrilda kandti (7.48g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species ") +
  
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.7)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS13(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




######## Estrildidae- (Waxbills) #######
######## Foraging diet categories ######
########################################

##Appendix S8: Figure S13(c)


ggplot(Waxbills_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("purple")) +
  theme_bw() +
  ggtitle("Foraging vertical diet: Estrildidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  
  scale_x_discrete(labels = c("Estrilda astrild" = "Estrilda\nastrild", "Estrilda kandti" = "Estrilda\nkandti"), 
                   name= " Species") +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 





ggsave(file = "Appendix_S8_FigureS13(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Estrildidae- (Waxbills) ######
######## Foraging forest strata #######
#######################################

##Appendix S8: Figure S13(d)


ggplot(Waxbills_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black","orange")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Estrildidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Estrilda astrild" = "Estrilda\nastrild", "Estrilda kandti" = "Estrilda\nkandti"), 
                   name= " Species") +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 





ggsave(file = "Appendix_S8_FigureS13(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)





#################################################################
################## Family: Acrocephalidae--warblers#############
########-------------------- 2 species ---------------------####
################################################################
############ Species abundance-Elevation curves ################



###### Iduna natalensis #####
#############################

###### Expected abundance

Abundance48 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[48]  + HCDSM_Virunga_63spp$mean$beta1[48]  * elev + HCDSM_Virunga_63spp$mean$beta2[48] * elev^2))
Ab48mx <- max(Abundance48)
Abundance48s <- Abundance48/Ab48mx

###### 95% credible interval (CI)

##### Lower 2.5 CI

l48<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[48]  + HCDSM_Virunga_63spp$q2.5$beta1[48] * elev + HCDSM_Virunga_63spp$q2.5$beta2[48]* elev^2))
l48s <- l48/Ab48mx

##### Upper 97.5 CI
u48 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[48]  + HCDSM_Virunga_63spp$q97.5$beta1[48] * elev + HCDSM_Virunga_63spp$q97.5$beta2[48]* elev^2))
u48s <- u48/Ab48mx

###### create a data frame
spp48 <- data.frame (Abundance48, Abundance48s, elev2, l48, l48s, u48, u48s)



###### Iduna similis #####
##########################

###### Expected abundance
Abundance49 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[49]  + HCDSM_Virunga_63spp$mean$beta1[49]  * elev + HCDSM_Virunga_63spp$mean$beta2[49] * elev^2))
Ab49mx <- max(Abundance49)
Abundance49s <- Abundance49/Ab49mx

###### 95% credible interval (CI)

##### Lower 2.5 CI
l49<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[49]  + HCDSM_Virunga_63spp$q2.5$beta1[49] * elev + HCDSM_Virunga_63spp$q2.5$beta2[49]* elev^2))
l49s <- l49/Ab49mx

##### Upper 97.5 CI
u49 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[49]  + HCDSM_Virunga_63spp$q97.5$beta1[49] * elev + HCDSM_Virunga_63spp$q97.5$beta2[49]* elev^2))
u49s <- u49/Ab49mx

###### create a data frame
spp49 <- data.frame (Abundance49, Abundance49s, elev, l49, l49s, u49, u49s)



######################################################
#### Integration function: Iduna natalensis ##########
#####################################################

inter48 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[48]  + 
                              HCDSM_Virunga_63spp$mean$beta1[48]  * x + HCDSM_Virunga_63spp$mean$beta2[48] * x^2)/Ab48mx}



### Area under the curve: Pogonocichla stellata

integrate(inter48, -2.24,3.16) 



###################################################
#### Integration function: Iduna similis ##########
###################################################

inter49 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[49]  + 
                              HCDSM_Virunga_63spp$mean$beta1[49]  * x + HCDSM_Virunga_63spp$mean$beta2[49] * x^2)/Ab49mx}



### Area under the curve: Iduna similis

integrate(inter49, -2.24,3.16) 



##  Iduna natalensis  & 	Iduna similis  ##
####-----------------------------------#####
x <- elev  ## Standardized elevation 
y48 <- Abundance48s  ## Standardized abundance: Iduna natalensis 
y49 <- Abundance49s  ## Standardized abundance: Iduna similis
### Computing area of overlap
spps48n49 <- approxfun(x, pmin(y48,y49), ties = "mean")
integrate(spps48n49, min(x), max(x))





##Appendix S8: Figure S14(a)
##Standardized

Figure_S14_a = ggplot () +
  
  geom_line (data=spp48, aes(x=elev2 , y= Abundance48s, color ="orange"), size =2) +
  geom_line (data=spp49, aes(x=elev2 , y= Abundance49s, color ="blue"), size =2) +
  
  
  annotate("text", x = 2000, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Acrocephalidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 1))

Figure_S14_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Iduna natalensis (11.7g)",  "Iduna similis (11.1g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.8,0.7)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS14(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)





##Appendix S8: Figure S14(b)
## Confidence intervals

Figure_S14_b = ggplot () +
  
  geom_line (data=spp48, aes(x=elev2 , y= Abundance48, color ="orange"), size =2) +
  geom_ribbon(data=spp48,aes(x=elev2 , y= Abundance48, ymin = l48, ymax = u48), alpha = 0.5, fill= "orange")    +
  geom_line (data=spp49, aes(x=elev2 , y= Abundance49, color ="blue"), size =2) +
  geom_ribbon(data=spp49,aes(x=elev2 , y= Abundance49, ymin = l49, ymax = u49), alpha = 0.5, fill= "blue")    +
  
  
  
  
  annotate("text", x = 2750, y = 8, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Acrocephalidae", fontface= "plain") +
  
  coord_cartesian(ylim = c(0, 8))

Figure_S14_b + labs (x="Elevation(m)", y ="Expected abundance ", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c(" Iduna natalensis (11.7g)",  "Iduna similis (11.1g)"), 
                     values=c( "orange"= "orange", "blue" = "blue"), name = "Species") +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.18,0.85)), legend.title=element_text(size=25, color="black")) 




ggsave(file = "Appendix_S8_FigureS14(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




######## Acrocephalidae-- (Warblers) ######
######## Foraging diet categories ########
##########################################

##Appendix S8: Figure S14(c)

ggplot(Acrocephalid_warblers_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey")) +
  theme_bw() +
  ggtitle("Foraging vertical diet: Acrocephalidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  
  scale_x_discrete(labels = c("Iduna natalensis" = "Iduna\nnatalensis", "Iduna similis" = "Iduna\nsimilis"), 
                   name= " Species") +
  
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 



ggsave(file = "Appendix_S8_FigureS14(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Acrocephalidae-- (Warblers) ######
######## Foraging forest strata ###########
###########################################

##Appendix S8: Figure S14(d)

ggplot(Acrocephalid_warblers_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("black", "orange","purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Acrocephalidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Iduna natalensis" = "Iduna\nnatalensis", "Iduna similis" = "Iduna\nsimilis"), 
                   name= " Species") +
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 





ggsave(file = "Appendix_S8_FigureS14(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######################################################################
###################### Family: Muscicapidae -- (Flycatchers)##########
########-------------------- 4 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################



###### Melaenornis fisheri #####
################################

###### Expected abundance
Abundance26 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[26]  + HCDSM_Virunga_63spp$mean$beta1[26]  * elev + HCDSM_Virunga_63spp$mean$beta2[26] * elev^2))
Ab26mx <- max(Abundance26) # # Maximum abundance
Abundance26s <- Abundance26/Ab26mx 

###### 95% credible interval (CI)

##### Lower 2.5 CI

l26<-   (exp(HCDSM_Virunga_63spp$q2.5$beta0[26]  + HCDSM_Virunga_63spp$q2.5$beta1[26] * elev + HCDSM_Virunga_63spp$q2.5$beta2[26]* elev^2))
l26s <- l26/Ab26mx 


##### Upper 97.5 CI
u26 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[26]  + HCDSM_Virunga_63spp$q97.5$beta1[26] * elev + HCDSM_Virunga_63spp$q97.5$beta2[26]* elev^2))
u26s <-u26/Ab26mx 

###### create a data frame
spp26 <- data.frame (Abundance26, Abundance26s, elev2, l26, l26s, u26, u26s)



###### Bradornis comitatus #####
################################

###### Expected abundance
Abundance27 <- (exp(HCDSM_Virunga_63spp$mean$beta0[27]  + HCDSM_Virunga_63spp$mean$beta1[27]  * elev + HCDSM_Virunga_63spp$mean$beta2[27] * elev^2))
Ab27mx <- max(Abundance27) # # Maximum abundance
Abundance27s <- Abundance27/Ab27mx 

###### 95% credible interval (CI)

##### Lower 2.5 CI

l27<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[27]  + HCDSM_Virunga_63spp$q2.5$beta1[27] * elev + HCDSM_Virunga_63spp$q2.5$beta2[27]* elev^2))
l27s <- l27/Ab27mx 

##### Upper 97.5 CI
u27 <-  (exp(HCDSM_Virunga_63spp$q97.5$beta0[27]  + HCDSM_Virunga_63spp$q97.5$beta1[27] * elev + HCDSM_Virunga_63spp$q97.5$beta2[27]* elev^2))
u27s <-u27/Ab27mx 

###### create a data frame

spp27 <- data.frame (Abundance27, Abundance27s, elev2, l27, l27s, u27, u27s)




###### Cossypha archeri #######
###############################

###### Expected abundance
Abundance60 <-  (exp(HCDSM_Virunga_63spp$mean$beta0[60]  + HCDSM_Virunga_63spp$mean$beta1[60]  * elev + HCDSM_Virunga_63spp$mean$beta2[60] * elev^2))
Ab60mx <- max(Abundance60)
Abundance60s <- Abundance60/Ab60mx 

###### 95% credible interval (CI)

##### Lower 2.5 CI
l60<-  (exp(HCDSM_Virunga_63spp$q2.5$beta0[60]  + HCDSM_Virunga_63spp$q2.5$beta1[60] * elev + HCDSM_Virunga_63spp$q2.5$beta2[60]* elev^2))
l60s <- l60/Ab60mx

##### Upper 97.5 CI
u60 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[60]  + HCDSM_Virunga_63spp$q97.5$beta1[60] * elev + HCDSM_Virunga_63spp$q97.5$beta2[60]* elev^2))
u60s <-u60/Ab60mx

###### create a data frame
spp60 <- data.frame (Abundance60, Abundance60s, elev2, l60, l60s, u60, u60s)




###### Pogonocichla stellata ####
#################################

###### Expected abundance
Abundance61 <- (exp(HCDSM_Virunga_63spp$mean$beta0[61]  + HCDSM_Virunga_63spp$mean$beta1[61]  * elev + HCDSM_Virunga_63spp$mean$beta2[61] * elev^2))
Ab61mx <- max(Abundance61)
Abundance61s <- Abundance61/Ab61mx 

###### 95% credible interval (CI)

##### Lower 2.5 CI

l61<- (exp(HCDSM_Virunga_63spp$q2.5$beta0[61]  + HCDSM_Virunga_63spp$q2.5$beta1[61] * elev + HCDSM_Virunga_63spp$q2.5$beta2[61]* elev^2))
l61s <- l61/Ab61mx

##### Upper 97.5 CI
u61 <- (exp(HCDSM_Virunga_63spp$q97.5$beta0[61]  + HCDSM_Virunga_63spp$q97.5$beta1[61] * elev + HCDSM_Virunga_63spp$q97.5$beta2[61]* elev^2))
u61s <-u61/Ab61mx

###### create a data frame
spp61 <- data.frame (Abundance61, Abundance61s, elev2, l61, l61s, u61, u61s)





######################################################
#### Integration function: Melaenornis fisheri #######
#####################################################

inter26 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[26]  + 
                              HCDSM_Virunga_63spp$mean$beta1[26]  * x + HCDSM_Virunga_63spp$mean$beta2[26] * x^2)/Ab26mx}

### Area under the curve: Melaenornis fisheri
integrate(inter26, -2.24,3.16) 



######################################################
#### Integration function: Bradornis comitatus #######
#####################################################

inter27 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[27]  + 
                              HCDSM_Virunga_63spp$mean$beta1[27]  * x + HCDSM_Virunga_63spp$mean$beta2[27] * x^2)/Ab27mx}

### Area under the curve: Bradornis comitatus
integrate(inter27, -2.24,3.16) 


######################################################
#### Integration function: Cossypha archeri ##########
#####################################################

inter60 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[60]  + 
                              HCDSM_Virunga_63spp$mean$beta1[60]  * x + HCDSM_Virunga_63spp$mean$beta2[60] * x^2)/Ab60mx}

### Area under the curve: Cossypha archeri

integrate(inter60, -2.24,3.16) 


######################################################
#### Integration function: Pogonocichla stellata #####
#####################################################

inter61 <- function (x){exp(HCDSM_Virunga_63spp$mean$beta0[61]  + 
                              HCDSM_Virunga_63spp$mean$beta1[61]  * x + HCDSM_Virunga_63spp$mean$beta2[61] * x^2)/Ab61mx}

### Area under the curve: Pogonocichla stellata

integrate(inter61, -2.24,3.16) 


##  Melaenornis fisheri & Bradornis comitatus  ##
####------------------------------------------####
x <- elev ## Standardized elevation 
y26 <- Abundance26s ## Standardized abundance: Melaenornis fisheri
y27 <- Abundance27s ## Standardized abundance: Bradornis comitatus
### Computing area of overlap
spps26n27 <- approxfun(x, pmin(y26,y27), ties = "mean")
integrate(spps26n27, min(x), max(x))



##  Melaenornis fisheri & Cossypha archeri ##
####--------------------------------------#####
x <- elev ## Standardized elevation 
y26 <- Abundance26s ## Standardized abundance: Melaenornis fisheri
y60 <- Abundance60s ## Standardized abundance: Cossypha archeri
### Computing area of overlap
spps26n60 <- approxfun(x, pmin(y26,y60), ties = "mean")
integrate(spps26n60, min(x), max(x))



##  Melaenornis fisheri & Pogonocichla stellata ##
####-------------------------------------------#####
x <- elev ## Standardized elevation 
y26 <- Abundance26s ## Standardized abundance: Melaenornis fisheri
y61 <- Abundance61s ## Standardized abundance: Pogonocichla stellata
### Computing area of overlap
spps26n61 <- approxfun(x, pmin(y26,y61), ties = "mean")
integrate(spps26n61, min(x), max(x))


## Bradornis comitatus & Cossypha archeri ##
####-------------------------------------#####
x <- elev  ## Standardized elevation 
y27<- Abundance27s ## Standardized abundance: Bradornis comitatus
y60 <- Abundance60s ## Standardized abundance: Cossypha archeri
### Computing area of overlap
spps27n60 <- approxfun(x, pmin(y27,y60), ties = "mean")
integrate(spps27n60, min(x), max(x))


## Bradornis comitatus & Pogonocichla stellata ##
####------------------------------------------#####
x <- elev ## Standardized elevation 
y27<- Abundance27s ## Standardized abundance: Bradornis comitatus
y61 <- Abundance61s ## Standardized abundance: Pogonocichla stellata
### Computing area of overlap
spps27n61 <- approxfun(x, pmin(y27,y61), ties = "mean")
integrate(spps27n61, min(x), max(x))




## Cossypha archeri & Pogonocichla stellata ##
####---------------------------------------#####
x <- elev ## Standardized elevation 
y60<- Abundance60s ## Standardized abundance: Cossypha archeri
y61 <- Abundance61s ## Standardized abundance: Pogonocichla stellata
### Computing area of overlap
spps60n61 <- approxfun(x, pmin(y60,y61), ties = "mean")
integrate(spps60n61, min(x), max(x))




##Appendix S8: Figure S15(a)
##Standardized


Figure_S15_a = ggplot () +
  geom_line (data=spp26, aes(x=elev2 , y= Abundance26s, color ="brown"), size =2) +
  geom_line (data=spp27, aes(x=elev2 , y= Abundance27s, color ="blue"), size =2) +
  geom_line (data=spp60, aes(x=elev2 , y= Abundance60s, color ="orange"), size =2) +
  geom_line (data=spp61, aes(x=elev2 , y= Abundance61s, color ="black"), size =2) +
  
  
  annotate("text", x = 2000, y = 1, hjust = 0, size = 10, family = "Times New Roman", 
           label = " Muscicapidae ", fontface= "plain") +
  coord_cartesian(ylim = c(0, 1))

Figure_S15_a + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Melaenornis fisheri (23.4g)", "Bradornis comitatus(14.1g)","Cossypha archeri(22.8g)", 
                                "Pogonocichla stellata (18.6g)"), 
                     values=c( "brown"= "brown", "blue"="blue", "orange"= "orange", "black"="black"), name = "Species") +
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.82,0.87)), legend.title=element_text(size=25, color="black"))  


ggsave(file = "Appendix_S8_FigureS15(a).jpg", bg = NULL, dpi = 300, width = 15, height = 10)


##Appendix S8: Figure S15(b)
##Standardized


Figure_S15_b = ggplot () +

  geom_line (data=spp26, aes(x=elev2 , y= Abundance26, color ="brown"), size =2) +
  geom_ribbon(data=spp26,aes(x=elev2 , y= Abundance26, ymin = l26, ymax = u26), alpha = 0.5, fill= "brown")    +
  geom_line (data=spp26, aes(x=elev2 , y= Abundance27, color ="blue"), size =2) +
  geom_ribbon(data=spp26,aes(x=elev2 , y= Abundance27, ymin = l27, ymax = u27), alpha = 0.5, fill= "blue")    +
  geom_line (data=spp60, aes(x=elev2 , y= Abundance60, color ="orange"), size =2) +
  geom_ribbon(data=spp60,aes(x=elev2 , y= Abundance60, ymin = l60, ymax = u60), alpha = 0.5, fill= "orange") +
  geom_line (data=spp61, aes(x=elev2 , y= Abundance61, color ="black"), size =2) +
  geom_ribbon(data=spp61,aes(x=elev2 , y= Abundance61, ymin = l61, ymax = u61), alpha = 0.5, fill= "black") +
  
  annotate("text", x = 2750, y = 20, hjust = 0, size = 10, family = "Times New Roman", 
           label = "Muscicapidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 20))

Figure_S15_b + labs (x="Elevation(m)", y ="Expected abundance (standardized)", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Melaenornis fisheri (23.4g)", "Bradornis comitatus(14.1g)","Cossypha archeri(22.8g)", 
                                "Pogonocichla stellata (18.6g)"), 
                     values=c( "brown"= "brown", "blue"="blue", "orange"= "orange", "black"="black"), name = "Species") +
  theme_few() +
  
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 25),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=25, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=25, color = "black"),
        axis.title.y = element_text(size = 30, angle = 90),
        axis.title.x = element_text(size = 30, angle = 00),
        legend.text=element_text(size=25,face="italic"),
        legend.position = c(c(0.42,0.75)), legend.title=element_text(size=25, color="black"))  



ggsave(file = "Appendix_S8_FigureS15(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)




######## Muscicapidae-- (Flycatchers) ####
######## Foraging diet categories ########
##########################################

##Appendix S8: Figure S15(c)

ggplot(Flycatchers_diet2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey","orange", "red")) +
  theme_bw() +
  ggtitle("Foraging diet categories: Muscicapidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Melaenornis fisheri" = "Melaenornis fisheri", "Bradornis comitatus" = "Bradornis\ncomitatus", 
                              "Cossypha archeri" = "Cossypha\narcheri", "Pogonocichla stellata" = "Pogonocichla\nstellata"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face ="plain")) 





ggsave(file = "Appendix_S8_FigureS15(c).jpg", bg = NULL, dpi = 300, width = 15, height = 10)



######## Muscicapidae-- (Flycatchers)###
######## Foraging forest strata #######
#######################################


##Appendix S8: Figure S15(d)
ggplot(Flycatchers_strata2, aes(Species, value, fill = variable)) + 
  geom_col( position = "dodge") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("grey", "black", "orange", "purple")) +
  theme_bw() +
  ggtitle("Foraging vertical strata: Muscicapidae") +
  ylab("Proportional use(%)") + xlab("Species") +
  
  scale_x_discrete(labels = c("Melaenornis fisheri" = "Melaenornis fisheri", "Bradornis comitatus" = "Bradornis\ncomitatus", 
                              "Cossypha archeri" = "Cossypha\narcheri", "Pogonocichla stellata" = "Pogonocichla\nstellata"), name= " Species") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text =element_text(size = 23),
    axis.title.x = element_text( size = 30,colour = "black"),
    axis.title.y = element_text( size = 30,colour = "black"),
    axis.text.x = element_text(face="italic", size = 25, colour = "black"),
    axis.text.y = element_text( size = 25, colour = "black"),
    panel.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill=NA, colour = "black", size=1),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5, face = "plain")) 



ggsave(file =  "Appendix_S8_FigureS15(d).jpg", bg = NULL, dpi = 300, width = 15, height = 10)








