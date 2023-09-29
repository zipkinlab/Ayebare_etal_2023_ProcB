### Author: Samuel Ayebare
#JSDM_output.r
          ## This script generates:  
#            Figure4 a, b


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
loadfonts(device = "win")


#-----------------------#
#-Set working directory-#
#-----------------------#

setwd("./Data_indices_figures")


#---------------------#
#-Load Files and data-#
#---------------------#

### Model 1
### Null model

load("../Data_indices_figures/HCDSM_warblers_null.RData")

###### Model output
null_model <- HCDSM_warblers_Virunga_null$summary

write.csv(null_model, "null_model.csv")

### Model 2
### Covariate model- Elevation

load("../Data_indices_figures/HCDSM_warblers_elev.RData")

###### Model output
covariate_model <- HCDSM_warblers_Virunga_elev$summary

write.csv(covariate_model, "covariate_model.csv")


# Importing sample points
svy_pts <- read.csv("Virunga_covariates.csv", header=TRUE)
svy_pts <- tibble::as_tibble(svy_pts)
head(svy_pts)

# scaled Elevation
elev <- (svy_pts$elev - mean(svy_pts$elev))/sd(svy_pts$elev)

## Raw elevation values

elev2 <- svy_pts$elev


#-------------#
#-Species ids-#
#-------------#
#  Apalis personata     (id = 1)
#  Apalis porphyrolaema (id = 2)
#  Cisticola chubbi     (id = 3)
#  Oreolais ruwenzori   (id = 4)
#  Prinia bairdii       (id = 5)
 

name= c("Apalis personata", "Apalis porphyrolaema", "Cisticola chubbi", "Oreolais ruwenzori", "Prinia bairdii")
#### Null model : correlation parameters --> Apalis personata 
##############################################################
#### [2,1] Apalis porphyrolaema (APpor) & Apalis personata (APper) 
#### [3,1] Cisticola chubbi    (CIchu) & Apalis personata (APper) 
#### [4,1] Oreolais ruwenzori (ORruw) & Apalis personata (APper) 
#### [5,1] Prinia bairdii   (PRbai) & Apalis personata (APper) 

Rho.val1 <- cbind(HCDSM_warblers_Virunga_null$q2.5$rho[c(2:5)],
                  HCDSM_warblers_Virunga_null$q25$rho[c(2:5)], 
                  HCDSM_warblers_Virunga_null$mean$rho[c(2:5)], 
                  HCDSM_warblers_Virunga_null$q75$rho[c(2:5)], 
                  HCDSM_warblers_Virunga_null$q97.5$rho[c(2:5)])

#### Species names (IDs)

sppnames1 <- c("APper-APpor", "APper-CIchu", "APper-ORruw", "APper-PRbai") 

#### Create a data frame

values1 <- data.frame(sppnames1, Rho.val1)
colnames(values1) <- c("species", "lower.rho", "l25.rho", "mean.rho", "u75.rho", "upper.rho")


#### Null model : correlation parameters--> Apalis porphyrolaema
##############################################################

#### [3,2] Cisticola chubbi (CIchu) & Apalis porphyrolaema (APpor) 
#### [4,2] Oreolais ruwenzori (ORruw) & Apalis porphyrolaema (APpor)
#### [5,2] Prinia bairdii (PRbai) & Apalis porphyrolaema (APpor) 

Rho.val2 <- cbind(HCDSM_warblers_Virunga_null$q2.5$rho[c(8:10)],
                  HCDSM_warblers_Virunga_null$q25$rho[c(8:10)], 
                  HCDSM_warblers_Virunga_null$mean$rho[c(8:10)], 
                  HCDSM_warblers_Virunga_null$q75$rho[c(8:10)], 
                  HCDSM_warblers_Virunga_null$q97.5$rho[c(8:10)])

#### Species names 
sppnames2 <- c("APPOR-CIchu","APpor-ORruw", "APpor-PRbai")  


#### Create a data frame
values2 <- data.frame(sppnames2, Rho.val2)
colnames(values2) <- c("species", "lower.rho", "l25.rho", "mean.rho", "u75.rho", "upper.rho")



#### Null model : correlation parameters--> Cisticola chubbi
##############################################################
#### [4,3] Oreolais ruwenzori (ORruw) & Cisticola chubbi (CIchu)
#### [5,3] Prinia bairdii (PRbai) & Cisticola chubbi (CIchu)

Rho.val3 <- cbind(HCDSM_warblers_Virunga_null$q2.5$rho[c(14:15)],
                  HCDSM_warblers_Virunga_null$q25$rho[c(14:15)], 
                  HCDSM_warblers_Virunga_null$mean$rho[c(14:15)], 
                  HCDSM_warblers_Virunga_null$q75$rho[c(14:15)], 
                  HCDSM_warblers_Virunga_null$q97.5$rho[c(14:15)])


#### Species names 

sppnames3 <- c( "CIchu-ORru", "CIchu-PRbaI")  

#### Create a data frame

values3 <- data.frame(sppnames3, Rho.val3)
colnames(values3) <- c("species", "lower.rho", "l25.rho", "mean.rho", "u75.rho", "upper.rho")


#### Null model : correlation parameters--> Oreolais ruwenzori
##############################################################
#### [5,4] Prinia bairdii (PRbai) & Oreolais ruwenzori (ORruw)

Rho.val4 <- cbind(HCDSM_warblers_Virunga_null$q2.5$rho[c(20)],
                  HCDSM_warblers_Virunga_null$q25$rho[c(20)], 
                  HCDSM_warblers_Virunga_null$mean$rho[c(20)], 
                  HCDSM_warblers_Virunga_null$q75$rho[c(20)], 
                  HCDSM_warblers_Virunga_null$q97.5$rho[c(20)])

sppnames4 <- c( "ORruw-PRbai")

#### Create a data frame

values4 <- data.frame(sppnames4, Rho.val4)
colnames(values4) <- c("species", "lower.rho", "l25.rho", "mean.rho", "u75.rho", "upper.rho")






#### Covariate (elevation) model 
#### ---------------------######

##### correlation parameters --> Apalis personata 
##############################################################
#### [2,1] Apalis porphyrolaema (APpor2) & Apalis personata (APper) 
#### [3,1] Cisticola chubbi    (CIchu2) & Apalis personata (APper) 
#### [4,1] Oreolais ruwenzori (ORruw2) & Apalis personata (APper) 
#### [5,1] Prinia bairdii   (PRbai2) & Apalis personata (APper) 



#Warblers_MV1 <- HCDSM_warblers_Virunga_null$summary

# write.csv(Warblers_MV1, "Virunga_warblers1.csv")

#########################################
Rho.val1ab <- cbind(HCDSM_warblers_Virunga_elev$q2.5$rho[c(2:5)],
                    HCDSM_warblers_Virunga_elev$q25$rho[c(2:5)], 
                    HCDSM_warblers_Virunga_elev$mean$rho[c(2:5)], 
                    HCDSM_warblers_Virunga_elev$q75$rho[c(2:5)], 
                    HCDSM_warblers_Virunga_elev$q97.5$rho[c(2:5)])


#### Species names 

sppnames1ab <- c("APper-APpor2", "APper-CIchu2", "APper-ORruw2", "APper-PRbai2") 

values1ab <- data.frame(sppnames1ab, Rho.val1ab)
colnames(values1ab) <- c("species", "lower.rho", "l25.rho", "mean.rho", "u75.rho", "upper.rho")


#### Covariate model : correlation parameters--> Apalis porphyrolaema
##############################################################
#### [3,2] Cisticola chubbi (CIchu2) & Apalis porphyrolaema (APpor) 
#### [4,2] Oreolais ruwenzori (ORruw2) & Apalis porphyrolaema (APpor)
#### [5,2] Prinia bairdii (PRbai2) & Apalis porphyrolaema (APpor) 

Rho.val2ab <- cbind(HCDSM_warblers_Virunga_elev$q2.5$rho[c(8:10)],
                    HCDSM_warblers_Virunga_elev$q25$rho[c(8:10)], 
                    HCDSM_warblers_Virunga_elev$mean$rho[c(8:10)], 
                    HCDSM_warblers_Virunga_elev$q75$rho[c(8:10)], 
                    HCDSM_warblers_Virunga_elev$q97.5$rho[c(8:10)])

#### Species names 

sppnames2ab <- c("APPOR-CIchu2", 
                 "APpor-ORruw2", "APpor-PRbai2")  



values2ab <- data.frame(sppnames2ab, Rho.val2ab)
colnames(values2ab) <- c("species", "lower.rho", "l25.rho", "mean.rho", "u75.rho", "upper.rho")

#### Covariate model : correlation parameters--> Cisticola chubbi
##############################################################
#### [4,3] Oreolais ruwenzori (ORruw2) & Cisticola chubbi (CIchu)
#### [5,3] Prinia bairdii (PRbai2) & Cisticola chubbi (CIchu)

Rho.val3ab <- cbind(HCDSM_warblers_Virunga_elev$q2.5$rho[c(14:15)],
                    HCDSM_warblers_Virunga_elev$q25$rho[c(14:15)], 
                    HCDSM_warblers_Virunga_elev$mean$rho[c(14:15)], 
                    HCDSM_warblers_Virunga_elev$q75$rho[c(14:15)], 
                    HCDSM_warblers_Virunga_elev$q97.5$rho[c(14:15)])

#### Species names 

sppnames3ab <- c( "CIchu-ORru2", "CIchu-PRbaI2")  

#### Create a data frame

values3ab <- data.frame(sppnames3ab, Rho.val3ab)
colnames(values3ab) <- c("species", "lower.rho", "l25.rho", "mean.rho", "u75.rho", "upper.rho")


#### Covariate model : correlation parameters--> Oreolais ruwenzori
##############################################################
#### [5,4] Prinia bairdii (PRbai2) & Oreolais ruwenzori (ORruw)

Rho.val4ab <- cbind(HCDSM_warblers_Virunga_elev$q2.5$rho[c(20)],
                    HCDSM_warblers_Virunga_elev$q25$rho[c(20)], 
                    HCDSM_warblers_Virunga_elev$mean$rho[c(20)], 
                    HCDSM_warblers_Virunga_elev$q75$rho[c(20)], 
                    HCDSM_warblers_Virunga_elev$q97.5$rho[c(20)])

#### Species names 

sppnames4ab <- c( "ORruw-PRbai2")  

#### Create a data frame

values4ab <- data.frame(sppnames4ab, Rho.val4ab)
colnames(values4ab) <- c("species", "lower.rho", "l25.rho", "mean.rho", "u75.rho", "upper.rho")




### Plot

Figure4_a <- ggplot() + 
  
  
  geom_errorbar(data = values1, aes(x = species, ymin = mean.rho, ymax = mean.rho, color = "orange"), 
                width = 0.5) +
  geom_errorbar(data = values1, aes(x = species, ymin = lower.rho, ymax = upper.rho, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values1, aes(x = species, ymin = l25.rho, ymax = u75.rho, color = "orange"), 
                size = 3.5, width = 0) +
  
  geom_errorbar(data = values2, aes(x = species, ymin = mean.rho, ymax = mean.rho, color = "orange"), 
                width = 0.5) +
  geom_errorbar(data = values2, aes(x = species, ymin = lower.rho, ymax = upper.rho, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values2, aes(x = species, ymin = l25.rho, ymax = u75.rho, color = "orange"), 
                size = 3.5, width = 0)  +
  
  geom_errorbar(data = values3, aes(x = species, ymin = mean.rho, ymax = mean.rho, color = "orange"), 
                width = 0.5) +
  geom_errorbar(data = values3, aes(x = species, ymin = lower.rho, ymax = upper.rho, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values3, aes(x = species, ymin = l25.rho, ymax = u75.rho, color = "orange"), 
                size = 3.5, width = 0) +
  
  geom_errorbar(data = values4, aes(x = species, ymin = mean.rho, ymax = mean.rho, color = "orange"), 
                width = 0.5) +
  geom_errorbar(data = values4, aes(x = species, ymin = lower.rho, ymax = upper.rho, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values4, aes(x = species, ymin = l25.rho, ymax = u75.rho, color = "orange"), 
                size = 3.5, width = 0) +
  
  geom_errorbar(data = values1ab, aes(x = species, ymin = mean.rho, ymax = mean.rho, color = "black"), 
                width = 0.5) +
  geom_errorbar(data = values1ab, aes(x = species, ymin = lower.rho, ymax = upper.rho, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values1ab, aes(x = species, ymin = l25.rho, ymax = u75.rho, color = "black"), 
                size = 3.5, width = 0) +
  
  geom_errorbar(data = values2ab, aes(x = species, ymin = mean.rho, ymax = mean.rho, color = "black"), 
                width = 0.5) +
  geom_errorbar(data = values2ab, aes(x = species, ymin = lower.rho, ymax = upper.rho, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values2ab, aes(x = species, ymin = l25.rho, ymax = u75.rho, color = "black"), 
                size = 3.5, width = 0)  +
  
  geom_errorbar(data = values3ab, aes(x = species, ymin = mean.rho, ymax = mean.rho, color = "black"), 
                width = 0.5) +
  geom_errorbar(data = values3ab, aes(x = species, ymin = lower.rho, ymax = upper.rho, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values3ab, aes(x = species, ymin = l25.rho, ymax = u75.rho, color = "black"), 
                size = 3.5, width = 0) +
  
  geom_errorbar(data = values4ab, aes(x = species, ymin = mean.rho, ymax = mean.rho, color = "black"), 
                width = 0.5) +
  geom_errorbar(data = values4ab, aes(x = species, ymin = lower.rho, ymax = upper.rho, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values4ab, aes(x = species, ymin = l25.rho, ymax = u75.rho, color = "black"), 
                size = 3.5, width = 0) +
  
  
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 0.75) +
  scale_color_manual(name = "", values = c("orange" = "#ff8000", "black" = "black" )) +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(family = "Times New Roman", size = 35),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,size=35, color= "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=35,color = "black"),
        axis.title.x = element_text( size = 35,colour = "black"),
        axis.title.y = element_text( size = 35,colour = "black"),
        legend.position = "none") +
  scale_x_discrete(labels = c("APper-APpor" = "1", "APper-APpor2" = "1", "APper-CIchu" = "2", "APper-CIchu2" = "2", "APper-ORruw" = "3", 
                              "APper-ORruw2" = "3","APper-PRbai" = "4","APper-PRbai2" = "4", "APPOR-CIchu" = "5", "APPOR-CIchu2" = "5", 
                              "APpor-ORruw" = "6",  "APpor-ORruw2" = "6","APpor-PRbai" = "7", "APpor-PRbai2" = "7", "CIchu-ORru" = "8", 
                              "CIchu-ORru2" = "8", "CIchu-PRbaI" = "9", "CIchu-PRbaI2" = "9", "ORruw-PRbai" = "10", "ORruw-PRbai2" = "10")) +
  labs(y = expression("Residual correlation"), x = expression("Species pairs"))

ggsave(file = "Residual_correlation_figure4(a).jpg", bg = NULL, dpi = 400, width = 15, height = 10)






########################################################################
###################### Family: Cisticolidae -- (Warblers)##############
########-------------------- 5 species ---------------------##########
######################################################################
############ Species abundance-Elevation curves ######################

### Joint Species Distribution Model####
############################################



###### Apalis personata #######
#############################

###### Expected abundance
Abundance1 <-  (exp(HCDSM_warblers_Virunga_elev$mean$beta0[1]  + HCDSM_warblers_Virunga_elev$mean$beta1[1]  * elev + HCDSM_warblers_Virunga_elev$mean$beta2[1] * elev^2))

###### 95% credible interval (CI)
##### Lower 2.5 CI

l1 <-   (exp(HCDSM_warblers_Virunga_elev$q2.5$beta0[1]  + HCDSM_warblers_Virunga_elev$q2.5$beta1[1] * elev + HCDSM_warblers_Virunga_elev$q2.5$beta2[1]* elev^2))

##### Upper 97.5 CI
u1 <-  (exp(HCDSM_warblers_Virunga_elev$q97.5$beta0[1]  + HCDSM_warblers_Virunga_elev$q97.5$beta1[1] * elev + HCDSM_warblers_Virunga_elev$q97.5$beta2[1]* elev^2))

###### create a data frame
spp1 <- data.frame (Abundance1, elev, l1, u1)


###### Apalis porphyrolaema #######
#############################

###### Expected abundance
Abundance2 <- (exp(HCDSM_warblers_Virunga_elev$mean$beta0[2]  + HCDSM_warblers_Virunga_elev$mean$beta1[2]  * elev + HCDSM_warblers_Virunga_elev$mean$beta2[2] * elev^2))

###### 95% credible interval (CI)

##### Lower 2.5 CI

l2<-  (exp(HCDSM_warblers_Virunga_elev$q2.5$beta0[2]  + HCDSM_warblers_Virunga_elev$q2.5$beta1[2] * elev + HCDSM_warblers_Virunga_elev$q2.5$beta2[2]* elev^2))

##### Upper 97.5 CI
u2 <-  (exp(HCDSM_warblers_Virunga_elev$q97.5$beta0[2]  + HCDSM_warblers_Virunga_elev$q97.5$beta1[2] * elev + HCDSM_warblers_Virunga_elev$q97.5$beta2[2]* elev^2))

###### create a data frame
spp2 <- data.frame (Abundance2, elev, l2, u2)



###### Cisticola chubbi #######
#############################

###### Expected abundance
Abundance3 <-  (exp(HCDSM_warblers_Virunga_elev$mean$beta0[3]  + HCDSM_warblers_Virunga_elev$mean$beta1[3]  * elev + HCDSM_warblers_Virunga_elev$mean$beta2[3] * elev^2))


###### 95% credible interval (CI)

##### Lower 2.5 CI

l3<-  (exp(HCDSM_warblers_Virunga_elev$q2.5$beta0[3]  + HCDSM_warblers_Virunga_elev$q2.5$beta1[3] * elev + HCDSM_warblers_Virunga_elev$q2.5$beta2[3]* elev^2))

##### Upper 97.5 CI

u3 <- (exp(HCDSM_warblers_Virunga_elev$q97.5$beta0[3]  + HCDSM_warblers_Virunga_elev$q97.5$beta1[3] * elev + HCDSM_warblers_Virunga_elev$q97.5$beta2[3]* elev^2))


###### create a data frame
spp3 <- data.frame (Abundance3, elev, l3, u3)



###### Oreolais ruwenzorii #######
#############################

###### Expected abundance
Abundance4 <- (exp(HCDSM_warblers_Virunga_elev$mean$beta0[4]  + HCDSM_warblers_Virunga_elev$mean$beta1[4]  * elev + HCDSM_warblers_Virunga_elev$mean$beta2[4] * elev^2))

###### 95% credible interval (CI)

##### Lower 2.5 CI

l4<- (exp(HCDSM_warblers_Virunga_elev$q2.5$beta0[4]  + HCDSM_warblers_Virunga_elev$q2.5$beta1[4] * elev + HCDSM_warblers_Virunga_elev$q2.5$beta2[4]* elev^2))



##### Upper 97.5 CI
u4 <- (exp(HCDSM_warblers_Virunga_elev$q97.5$beta0[4]  + HCDSM_warblers_Virunga_elev$q97.5$beta1[4] * elev + HCDSM_warblers_Virunga_elev$q97.5$beta2[4]* elev^2))

###### create a data frame
spp4 <- data.frame (Abundance4, elev, l4, u4)



###### Prinia bairdii #######
#############################

###### Expected abundance
Abundance5 <- (exp(HCDSM_warblers_Virunga_elev$mean$beta0[5]  + HCDSM_warblers_Virunga_elev$mean$beta1[5]  * elev + HCDSM_warblers_Virunga_elev$mean$beta2[5] * elev^2))

###### 95% credible interval (CI)

##### Lower 2.5 CI

l5<- (exp(HCDSM_warblers_Virunga_elev$q2.5$beta0[5]  + HCDSM_warblers_Virunga_elev$q2.5$beta1[5] * elev + HCDSM_warblers_Virunga_elev$q2.5$beta2[5]* elev^2))


##### Upper 97.5 CI
u5 <- (exp(HCDSM_warblers_Virunga_elev$q97.5$beta0[5]  + HCDSM_warblers_Virunga_elev$q97.5$beta1[5] * elev + HCDSM_warblers_Virunga_elev$q97.5$beta2[5]* elev^2))

###### create a data frame
spp5 <- data.frame (Abundance5, elev, l5, u5)



##Appendix S5: Figure S4 (b)
## With confidence intervals

Figure4_b = ggplot () +
  geom_line (data=spp1, aes(x=elev2 , y= Abundance1, color ="red"), size =2) +
  geom_ribbon(data=spp1,aes(x=elev2 , y= Abundance1, ymin = l1, ymax = u1), alpha = 0.5, fill= "red") +
  geom_line (data=spp2, aes(x=elev2 , y= Abundance2, color ="blue"), size =2) +
  geom_ribbon(data=spp2,aes(x=elev2 , y= Abundance2, ymin = l2, ymax = u2), alpha = 0.5, fill= "blue") +
  geom_line (data=spp3, aes(x=elev2 , y= Abundance3, color ="orange"), size =2) +
  geom_ribbon(data=spp3,aes(x=elev2 , y= Abundance3, ymin = l3, ymax = u3), alpha = 0.5, fill= "orange") +
  geom_line (data=spp4, aes(x=elev2 , y= Abundance4, color ="black"), size =2) +
  geom_ribbon(data=spp4,aes(x=elev2 , y= Abundance4, ymin = l4, ymax = u4), alpha = 0.5, fill= "black") +
  geom_line (data=spp5, aes(x=elev2 , y= Abundance5, color ="grey"), size =2) +
  geom_ribbon(data=spp5,aes(x=elev2 , y= Abundance5, ymin = l5, ymax = u5), alpha = 0.5, fill= "grey") +
  
  
  
  annotate("text", x = 2700, y = 20, hjust = 0, size = 11, family = "Times New Roman", 
           label = "Cisticolidae", fontface= "plain") +
  coord_cartesian(ylim = c(0, 20))

Figure4_b + labs (x="Elevation(m)", y ="Expected abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(labels = c("Apalis personata (11g)", "Apalis porphyrolaema (8.39g)","Cisticola chubbi (16.3g)", 
                                "Oreolais ruwenzori (9.9g)", "Prinia bairdii (13.4g)" ), 
                     values=c( "red"= "red", "blue"="blue", "orange"= "orange", "black"="black", "grey"= "grey"), name ="Species") +
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
        legend.position = c(c(0.7,0.7)), legend.title=element_text(size=30, color="black")) 



ggsave(file = "Figure4(b).jpg", bg = NULL, dpi = 300, width = 15, height = 10)













































































