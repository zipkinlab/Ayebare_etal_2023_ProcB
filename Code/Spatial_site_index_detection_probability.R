### Author: Samuel Ayebare
## Spatial_site_index_detection_probability.r

## This script
             
             # generates the null model distribution and figure for assessing detection probabilities 
             # among species within families - Spatial site index
# Appendix S4: Figure S2 



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

loadfonts()

#-----------------------#
#-Set working directory-#
#-----------------------#
setwd("./Data_indices_figures")


#---------------------#
#-Load Files and data-#
#---------------------#

load("../Data_indices_figures/HCDSM63spp.RData")

#----------------------------------------------#
#-Legend vegetation types - Detection function-#
#----------------------------------------------#

# 1 =  Alpine/sub-alpine i.e Reference class
# 2 = Bamboo forest
# 3 = Secondary bush/shrub 
# 4 = Grassland
# 5 = Hagenia-Hypericum woodland
# 6 = Mature mixed forest
# 7 = Swamp
# 8 = Secondary mixed forest 


### alpha -species specific intercept for the scale parameter 

alpha <- exp(HCDSM_Virunga_63spp$mean$alpha)* 100 ## Multiplied by 100 to account for scaling of the radial distances


## Create a vector for species id
Spp.id <- c("Spp1","Spp2","Spp3","Spp4","Spp5","Spp6","Spp7","Spp8","Spp9","Spp10","Spp11","Spp12","Spp13","Spp14","Spp15","Spp16","Spp17","Spp18","Spp19",
            "Spp20","Spp21","Spp22","Spp23","Spp24","Spp25","Spp26","Spp27","Spp28","Spp29","Spp30","Spp31","Spp32","Spp33","Spp34","Spp35","Spp36",
            "Spp37","Spp38","Spp39","Spp40","Spp41","Spp42","Spp43","Spp44","Spp45","Spp46","Spp47","Spp48","Spp49","Spp50","Spp51","Spp52","Spp53","Spp54",
            "Spp55","Spp56", "Spp57","Spp58","Spp59","Spp60","Spp61","Spp62","Spp63")



#### Create a data.frame for species and scale parameter
sigma.alpha <- data.frame(Spp.id ,alpha)


# Transpose all but the first column (name)
Sigma.alpine <- as.data.frame(t(sigma.alpha[,-1]))
colnames(Sigma.alpine) <- Spp.id
# Check the column types
str(Sigma.alpine) 


## Compute mean and standard deviation between species pairs within families
## To enable estimation of the coefficient of variation

# Lybiidae - mean

Sp3n4 <- mean(c(Sigma.alpine$Spp3, Sigma.alpine$Spp4))  

Sp3n5 <- mean(c(Sigma.alpine$Spp3, Sigma.alpine$Spp5)) 

Sp4n5 <- mean(c(Sigma.alpine$Spp4, Sigma.alpine$Spp5))


# Lybiidae - standard deviation

Sp3n4.sd <-  sd(c(Sigma.alpine$Spp3, Sigma.alpine$Spp4))  

Sp3n5.sd <-  sd(c(Sigma.alpine$Spp3, Sigma.alpine$Spp5)) 

Sp4n5.sd <-  sd(c(Sigma.alpine$Spp4, Sigma.alpine$Spp5))


##Columbidae  - mean
Sp7n8 <- mean(c(Sigma.alpine$Spp7, Sigma.alpine$Spp8)) 

Sp7n9 <- mean(c(Sigma.alpine$Spp7, Sigma.alpine$Spp9)) 

Sp8n9 <- mean(c(Sigma.alpine$Spp8, Sigma.alpine$Spp9))  



##Columbidae - standard deviation
Sp7n8.sd <-  sd(c(Sigma.alpine$Spp7, Sigma.alpine$Spp8)) 

Sp7n9.sd <-  sd(c(Sigma.alpine$Spp7, Sigma.alpine$Spp9)) 

Sp8n9.sd <-  sd(c(Sigma.alpine$Spp8, Sigma.alpine$Spp9))  


# Cuculidae - mean
Sp10n11 <- mean(c(Sigma.alpine$Spp10, Sigma.alpine$Spp11)) 

Sp10n12 <- mean(c(Sigma.alpine$Spp10, Sigma.alpine$Spp12)) 

Sp11n12 <- mean(c(Sigma.alpine$Spp11, Sigma.alpine$Spp12)) 


# Cuculidae  - standard deviation
Sp10n11.sd <-  sd(c(Sigma.alpine$Spp10, Sigma.alpine$Spp11)) 

Sp10n12.sd <-  sd(c(Sigma.alpine$Spp10, Sigma.alpine$Spp12)) 

Sp11n12.sd <-  sd(c(Sigma.alpine$Spp11, Sigma.alpine$Spp12)) 



# Estrildidae - mean
Sp13n14 <-  mean(c(Sigma.alpine$Spp13, Sigma.alpine$Spp14)) 

# Estrildidae - standard deviation
Sp13n14.sd <-  sd(c(Sigma.alpine$Spp13, Sigma.alpine$Spp14)) 



##Fringillidae - mean

Sp15n16 <-  mean(c(Sigma.alpine$Spp15, Sigma.alpine$Spp16)) 

##Fringillidae - standard deviation

Sp15n16.sd <-  sd(c(Sigma.alpine$Spp15, Sigma.alpine$Spp16)) 

## Malaconotidae  - mean
Sp18n19 <-  mean(c(Sigma.alpine$Spp18, Sigma.alpine$Spp19)) 

Sp18n20 <-  mean(c(Sigma.alpine$Spp18, Sigma.alpine$Spp20)) 

Sp18n21 <-  mean(c(Sigma.alpine$Spp18, Sigma.alpine$Spp21)) 

Sp18n22 <-  mean(c(Sigma.alpine$Spp18, Sigma.alpine$Spp22)) 

Sp19n20 <-  mean(c(Sigma.alpine$Spp19, Sigma.alpine$Spp20)) 

Sp19n21 <-  mean(c(Sigma.alpine$Spp19, Sigma.alpine$Spp21)) 

Sp19n22 <-  mean(c(Sigma.alpine$Spp19, Sigma.alpine$Spp22)) 

Sp20n21 <-  mean(c(Sigma.alpine$Spp20, Sigma.alpine$Spp21)) 

Sp20n22 <-  mean(c(Sigma.alpine$Spp20, Sigma.alpine$Spp22)) 

Sp21n22 <-  mean(c(Sigma.alpine$Spp21, Sigma.alpine$Spp22)) 



## Malaconotidae - standard deviation
Sp18n19.sd <-  sd(c(Sigma.alpine$Spp18, Sigma.alpine$Spp19)) 

Sp18n20.sd <-  sd(c(Sigma.alpine$Spp18, Sigma.alpine$Spp20)) 

Sp18n21.sd <-  sd(c(Sigma.alpine$Spp18, Sigma.alpine$Spp21)) 

Sp18n22.sd <-  sd(c(Sigma.alpine$Spp18, Sigma.alpine$Spp22)) 

Sp19n20.sd <-  sd(c(Sigma.alpine$Spp19, Sigma.alpine$Spp20)) 

Sp19n21.sd <-  sd(c(Sigma.alpine$Spp19, Sigma.alpine$Spp21)) 

Sp19n22.sd <-  sd(c(Sigma.alpine$Spp19, Sigma.alpine$Spp22)) 

Sp20n21.sd <-  sd(c(Sigma.alpine$Spp20, Sigma.alpine$Spp21)) 

Sp20n22.sd <-  sd(c(Sigma.alpine$Spp20, Sigma.alpine$Spp22)) 

Sp21n22.sd <-  sd(c(Sigma.alpine$Spp21, Sigma.alpine$Spp22)) 


#Platysteiridae  - mean

Sp24n25 <-  mean (c(Sigma.alpine$Spp24, Sigma.alpine$Spp25)) 



#Platysteiridae  - standard deviation

Sp24n25.sd <-  sd(c(Sigma.alpine$Spp24, Sigma.alpine$Spp25)) 


#Muscicapidae  - mean

Sp26n27 <-  mean(c(Sigma.alpine$Spp26, Sigma.alpine$Spp27)) 

Sp26n60 <-  mean(c(Sigma.alpine$Spp26, Sigma.alpine$Spp60)) 

Sp26n61 <-  mean(c(Sigma.alpine$Spp26, Sigma.alpine$Spp61)) 

Sp27n60 <-  mean(c(Sigma.alpine$Spp27, Sigma.alpine$Spp60)) 

Sp27n61 <-  mean(c(Sigma.alpine$Spp27, Sigma.alpine$Spp61)) 

Sp60n61 <-  mean(c(Sigma.alpine$Spp60, Sigma.alpine$Spp61)) 



#Muscicapidae   - standard deviation

Sp26n27.sd <-  sd(c(Sigma.alpine$Spp26, Sigma.alpine$Spp27)) 

Sp26n60.sd <-  sd(c(Sigma.alpine$Spp26, Sigma.alpine$Spp60)) 

Sp26n61.sd <-  sd(c(Sigma.alpine$Spp26, Sigma.alpine$Spp61)) 

Sp27n60.sd <-  sd(c(Sigma.alpine$Spp27, Sigma.alpine$Spp60)) 

Sp27n61.sd <-  sd(c(Sigma.alpine$Spp27, Sigma.alpine$Spp61)) 

Sp60n61.sd <-  sd(c(Sigma.alpine$Spp60, Sigma.alpine$Spp61))

##Musophagidae - mean

Sp29n30 <-  mean(c(Sigma.alpine$Spp29, Sigma.alpine$Spp30)) 

##Musophagidae  -- standard deviation

Sp29n30.sd <-  sd(c(Sigma.alpine$Spp29, Sigma.alpine$Spp30)) 

#Nectariniidae - mean
Sp31n32<-  mean(c(Sigma.alpine$Spp31, Sigma.alpine$Spp32)) 

Sp31n33<-  mean(c(Sigma.alpine$Spp31, Sigma.alpine$Spp33)) 

Sp31n34<-  mean(c(Sigma.alpine$Spp31, Sigma.alpine$Spp34)) 

Sp31n35<-  mean(c(Sigma.alpine$Spp31, Sigma.alpine$Spp35)) 

Sp31n36<-  mean(c(Sigma.alpine$Spp31, Sigma.alpine$Spp36)) 

Sp32n33<-  mean(c(Sigma.alpine$Spp32, Sigma.alpine$Spp33)) 

Sp32n34 <-  mean(c(Sigma.alpine$Spp32, Sigma.alpine$Spp34)) 

Sp32n35 <-  mean(c(Sigma.alpine$Spp32, Sigma.alpine$Spp35)) 

Sp32n36 <-  mean(c(Sigma.alpine$Spp32, Sigma.alpine$Spp36)) 

Sp33n34<-  mean(c(Sigma.alpine$Spp33, Sigma.alpine$Spp34)) 

Sp33n35 <-  mean(c(Sigma.alpine$Spp33, Sigma.alpine$Spp35)) 

Sp33n36 <-  mean(c(Sigma.alpine$Spp33, Sigma.alpine$Spp36)) 

Sp34n35 <-  mean(c(Sigma.alpine$Spp34, Sigma.alpine$Spp35)) 

Sp34n36 <-  mean(c(Sigma.alpine$Spp34, Sigma.alpine$Spp36)) 

Sp35n36 <-  mean(c(Sigma.alpine$Spp35, Sigma.alpine$Spp36)) 


#Nectariniidae - standard deviation
Sp31n32.sd <-  sd(c(Sigma.alpine$Spp31, Sigma.alpine$Spp32)) 

Sp31n33.sd <-  sd(c(Sigma.alpine$Spp31, Sigma.alpine$Spp33)) 

Sp31n34.sd <-  sd(c(Sigma.alpine$Spp31, Sigma.alpine$Spp34)) 

Sp31n35.sd <-  sd(c(Sigma.alpine$Spp31, Sigma.alpine$Spp35)) 

Sp31n36.sd <-  sd(c(Sigma.alpine$Spp31, Sigma.alpine$Spp36)) 

Sp32n33.sd <-  sd(c(Sigma.alpine$Spp32, Sigma.alpine$Spp33)) 

Sp32n34.sd <-  sd(c(Sigma.alpine$Spp32, Sigma.alpine$Spp34)) 

Sp32n35.sd <-  sd(c(Sigma.alpine$Spp32, Sigma.alpine$Spp35)) 

Sp32n36.sd <-  sd(c(Sigma.alpine$Spp32, Sigma.alpine$Spp36)) 

Sp33n34.sd <-  sd(c(Sigma.alpine$Spp33, Sigma.alpine$Spp34)) 

Sp33n35.sd <-  sd(c(Sigma.alpine$Spp33, Sigma.alpine$Spp35)) 

Sp33n36.sd <-  sd(c(Sigma.alpine$Spp33, Sigma.alpine$Spp36)) 

Sp34n35.sd <-  sd(c(Sigma.alpine$Spp34, Sigma.alpine$Spp35)) 

Sp34n36.sd <-  sd(c(Sigma.alpine$Spp34, Sigma.alpine$Spp36)) 

Sp35n36.sd <-  sd(c(Sigma.alpine$Spp35, Sigma.alpine$Spp36)) 




# Ploceidae - mean

Sp39n40 <-  mean(c(Sigma.alpine$Spp39, Sigma.alpine$Spp40))  

# Ploceidae - standard deviation

Sp39n40.sd <-  sd(c(Sigma.alpine$Spp39, Sigma.alpine$Spp40))  


#Pycnonotidae - mean
Sp42n43 <-  mean(c(Sigma.alpine$Spp42, Sigma.alpine$Spp43)) 

Sp42n44 <-  mean(c(Sigma.alpine$Spp42, Sigma.alpine$Spp44)) 

Sp43n44 <-  mean(c(Sigma.alpine$Spp43, Sigma.alpine$Spp44)) 

#Pycnonotidae - standard deviation
Sp42n43.sd <-  sd(c(Sigma.alpine$Spp42, Sigma.alpine$Spp43)) 

Sp42n44.sd <-  sd(c(Sigma.alpine$Spp42, Sigma.alpine$Spp44)) 

Sp43n44.sd <-  sd(c(Sigma.alpine$Spp43, Sigma.alpine$Spp44)) 

##Cisticolidae - mean
Sp45n46 <-  mean(c(Sigma.alpine$Spp45, Sigma.alpine$Spp46)) 

Sp45n50 <-  mean(c(Sigma.alpine$Spp45, Sigma.alpine$Spp50)) 

Sp45n51 <-  mean(c(Sigma.alpine$Spp45, Sigma.alpine$Spp51)) 

Sp45n54 <-  mean(c(Sigma.alpine$Spp45, Sigma.alpine$Spp54)) 

Sp46n50 <-  mean(c(Sigma.alpine$Spp46, Sigma.alpine$Spp50)) 

Sp46n51 <-  mean(c(Sigma.alpine$Spp46, Sigma.alpine$Spp51)) 

Sp46n54 <-  mean(c(Sigma.alpine$Spp46, Sigma.alpine$Spp54)) 

Sp50n51 <-  mean(c(Sigma.alpine$Spp50, Sigma.alpine$Spp51)) 

Sp50n54 <-  mean(c(Sigma.alpine$Spp50, Sigma.alpine$Spp54)) 

Sp51n54 <-  mean(c(Sigma.alpine$Spp51, Sigma.alpine$Spp54)) 


##Cisticolidae  - standard deviation
Sp45n46.sd <-  sd(c(Sigma.alpine$Spp45, Sigma.alpine$Spp46)) 

Sp45n50.sd <-  sd(c(Sigma.alpine$Spp45, Sigma.alpine$Spp50)) 

Sp45n51.sd <-  sd(c(Sigma.alpine$Spp45, Sigma.alpine$Spp51)) 

Sp45n54.sd <-  sd(c(Sigma.alpine$Spp45, Sigma.alpine$Spp54)) 

Sp46n50.sd <-  sd(c(Sigma.alpine$Spp46, Sigma.alpine$Spp50)) 

Sp46n51.sd <-  sd(c(Sigma.alpine$Spp46, Sigma.alpine$Spp51)) 

Sp46n54.sd <-  sd(c(Sigma.alpine$Spp46, Sigma.alpine$Spp54)) 

Sp50n51.sd <-  sd(c(Sigma.alpine$Spp50, Sigma.alpine$Spp51)) 

Sp50n54.sd <-  sd(c(Sigma.alpine$Spp50, Sigma.alpine$Spp54)) 

Sp51n54.sd <-  sd(c(Sigma.alpine$Spp51, Sigma.alpine$Spp54))


#Acrocephalidae - mean

Sp48n49 <-  mean(c(Sigma.alpine$Spp48, Sigma.alpine$Spp49))



#Acrocephalidae - standard deviation

Sp48n49.sd <-  sd(c(Sigma.alpine$Spp48, Sigma.alpine$Spp49))


#Phylloscopidae - mean
Sp52n53 <-  mean(c(Sigma.alpine$Spp52, Sigma.alpine$Spp53)) 

#Phylloscopidae - standard deviation
Sp52n53.sd <-  sd(c(Sigma.alpine$Spp52, Sigma.alpine$Spp53)) 







### Create a vector of means  - species pairs within the same family

All.spp.sgm.mean <- c(Sp3n4,Sp3n5,Sp4n5,Sp7n8,Sp7n9,Sp8n9,Sp10n11,Sp10n12,Sp11n12,Sp13n14,Sp15n16,Sp18n19,Sp18n20,Sp18n21,
                      Sp18n22,Sp19n20,Sp19n21,Sp19n22,Sp20n21,Sp20n22,Sp21n22,Sp24n25,Sp26n27,Sp26n60,Sp26n61,Sp27n60,Sp27n61,
                      Sp60n61,Sp29n30,Sp31n32,Sp31n33,Sp31n34,Sp31n35,Sp31n36,Sp32n33,Sp32n34,Sp32n35,Sp32n36,Sp33n34,Sp33n35,
                      Sp33n36,Sp34n35,Sp34n36,Sp35n36,Sp39n40,Sp42n43,Sp42n44,Sp43n44,Sp45n46,Sp45n50,Sp45n51,Sp45n54,Sp46n50,
                      Sp46n51,Sp46n54,Sp50n51,Sp50n54,Sp51n54,Sp48n49,Sp52n53)


### Create a vector of standard deviations  - species pairs within the same family
All.spp.sgm.sd <- c(Sp3n4.sd,Sp3n5.sd,Sp4n5.sd,Sp7n8.sd,Sp7n9.sd,Sp8n9.sd,Sp10n11.sd,Sp10n12.sd,Sp11n12.sd,Sp13n14.sd,Sp15n16.sd,Sp18n19.sd,
                    Sp18n20.sd,Sp18n21.sd,Sp18n22.sd,Sp19n20.sd,Sp19n21.sd,Sp19n22.sd,Sp20n21.sd,Sp20n22.sd,Sp21n22.sd,Sp24n25.sd,Sp26n27.sd,
                    Sp26n60.sd,Sp26n61.sd,Sp27n60.sd,Sp27n61.sd,Sp60n61.sd,Sp29n30.sd,Sp31n32.sd,Sp31n33.sd,Sp31n34.sd,Sp31n35.sd,Sp31n36.sd,
                    Sp32n33.sd,Sp32n34.sd,Sp32n35.sd,Sp32n36.sd,Sp33n34.sd,Sp33n35.sd,Sp33n36.sd,Sp34n35.sd,Sp34n36.sd,Sp35n36.sd,Sp39n40.sd,
                    Sp42n43.sd,Sp42n44.sd,Sp43n44.sd,Sp45n46.sd,Sp45n50.sd,Sp45n51.sd,Sp45n54.sd,Sp46n50.sd,Sp46n51.sd,Sp46n54.sd,Sp50n51.sd,Sp50n54.sd,
                    Sp51n54.sd,Sp48n49.sd,Sp52n53.sd)

# Compute mean coefficient of variation for all 60 species pairs
mean(All.spp.sgm.sd /All.spp.sgm.mean)


## Number of samples (each sample = 60 species pairs) selected to generate a null model expectation
n.samples <- 1000 

## Vector for storing standard deviations for 46 species (i.e) 60 species pairs
sd.sigma <- rep(NA, 60)

## Vector for storing means for 46 species (i.e) 60 species pairs
mean.sigma <- rep(NA, 60)

## Vector for storing coefficient of variation for each species pair

cv.sigma <- rep(NA, 60)

## Vector for storing mean coefficient of variation for all species pairs in the community
mean.cv.sigma <- rep(NA, n.samples)



## Computing null distribution for scale parameter (sigma)

for (i in 1:n.samples) {
  for (s in 1:60) { 
    
    # 1. Choose a random species pair from the total 63 species
    scale.parameter.2sp <-  as.numeric(sample(Sigma.alpine, 2,F))
    # 2. Compute the standard deviation
    sd.sigma [s] <-  sd(scale.parameter.2sp)
    # 3. Compute the mean
    mean.sigma [s] <-  mean(scale.parameter.2sp)
    # 4. Compute the coefficient of variation
    cv.sigma[s] <- sd.sigma [s]/mean.sigma [s]
    
  } # j
  
  mean.cv.sigma [i] <- mean(cv.sigma) # 5. Calculate the mean coefficient of variation for the 60 species pairs and store in a vector.
  
  
} # i


#write.csv(mean.cv.sigma, "mean.cv.scale.parameter.csv")

setwd("./Data_spatial_site_index")

# Importing mean coefficient of variation- sigma
cv.parameter <- read.csv("mean.cv.scale.parameter.csv", header = T)

head(cv.parameter)

## Assign col names
colnames( cv.parameter) <- c("sample.id", "mean.var")


### Histogram for null distribution

Appendix.S4.Figure.S2  <- ggplot(cv.parameter, aes(mean.var)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 0.242 ), colour = "black", linetype = "dashed", linewidth = 2 )   ## All species
  

Appendix.S4.Figure.S2 + labs (x=" Mean (Coeffiecient of variation)", y ="Frequency", color = "Legend\n",fontface= "plain") +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = ))+
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




ggsave(file = "det.parameter.cv.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "det.parameter.cv.svg", bg = NULL, dpi = 300, width = 15, height = 10)




