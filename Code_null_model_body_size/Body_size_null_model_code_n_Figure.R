### Author: Samuel Ayebare
## Figures_niche_indices.r

## This script
                # generates the null model distribution and figure for body size variation
                # Appendix S3: Figure S2



#----------------#
#-Load libraries-#
#----------------#

rm(list = ls())
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")

#-----------------------#
#-Set working directory-#
#-----------------------#

setwd("./Data_null_model_body_size")


######################################
###### Appendix S3: Figure S2 ########
######################################

# Importing body sizes for all the 63 species pairs
Community.63Sp <- read.csv("Bodymass_63Species.csv",header = T)
head(Community.63Sp)

# Note the -1 removes the first column
bodymass.63sp <- Community.63Sp[, -1]


### Estimate standard deviations and means for species pairs within families
## To enable estimation of the coefficient of variation

# Lybiidae
Sp3n4 <- mean (c(bodymass.63sp$Sp3, bodymass.63sp$Sp5))
Sp3n5 <- mean (c(bodymass.63sp$Sp3 , bodymass.63sp$Sp5))
Sp4n5 <- mean (c(bodymass.63sp$Sp4 , bodymass.63sp$Sp5))

# Lybiidae  -- standard deviation
Sp3n4.sd <- sd (c(bodymass.63sp$Sp3, bodymass.63sp$Sp5))
Sp3n5.sd <- sd (c(bodymass.63sp$Sp3 , bodymass.63sp$Sp5))
Sp4n5.sd <- sd (c(bodymass.63sp$Sp4 , bodymass.63sp$Sp5))

##Columbidae
Sp7n8 <- mean (c(bodymass.63sp$Sp7 , bodymass.63sp$Sp8))

Sp7n9 <- mean (c(bodymass.63sp$Sp7 , bodymass.63sp$Sp9))

Sp8n9 <- mean (c(bodymass.63sp$Sp8 , bodymass.63sp$Sp9))

##Columbidae -- standard deviation
Sp7n8.sd <- sd (c(bodymass.63sp$Sp7 , bodymass.63sp$Sp8))

Sp7n9.sd <- sd (c(bodymass.63sp$Sp7 , bodymass.63sp$Sp9))

Sp8n9.sd <- sd (c(bodymass.63sp$Sp8 , bodymass.63sp$Sp9))



# Cuculidae - mean
Sp10n11 <- mean (c(bodymass.63sp$Sp10 , bodymass.63sp$Sp11))

Sp10n12 <- mean (c(bodymass.63sp$Sp10 , bodymass.63sp$Sp12))

Sp11n12 <- mean (c(bodymass.63sp$Sp11 , bodymass.63sp$Sp12))

# Cuculidae - standard deviation
Sp10n11.sd <- sd (c(bodymass.63sp$Sp10 , bodymass.63sp$Sp11))

Sp10n12.sd <- sd (c(bodymass.63sp$Sp10 , bodymass.63sp$Sp12))

Sp11n12.sd <- sd (c(bodymass.63sp$Sp11 , bodymass.63sp$Sp12))


# Estrildidae
Sp13n14 <- mean (c(bodymass.63sp$Sp13 , bodymass.63sp$Sp14))

# Estrildidae -- standard deviation
Sp13n14.sd <- sd (c(bodymass.63sp$Sp13 , bodymass.63sp$Sp14))


##Fringillidae  -- mean

Sp15n16.sd <- mean (c(bodymass.63sp$Sp15 , bodymass.63sp$Sp16))

##Fringillidae  -- standard deviation

Sp15n16 <- sd (c(bodymass.63sp$Sp15 , bodymass.63sp$Sp16))

## Malaconotidae
Sp18n19 <- mean (c(bodymass.63sp$Sp18 , bodymass.63sp$Sp19))

Sp18n20 <- mean (c(bodymass.63sp$Sp18 , bodymass.63sp$Sp20))

Sp18n21 <- mean (c(bodymass.63sp$Sp18 , bodymass.63sp$Sp21))

Sp18n22 <- mean (c(bodymass.63sp$Sp18 , bodymass.63sp$Sp22))

Sp19n20 <- mean (c(bodymass.63sp$Sp19 , bodymass.63sp$Sp20))

Sp19n21 <- mean (c(bodymass.63sp$Sp19 , bodymass.63sp$Sp21))

Sp19n22 <- mean (c(bodymass.63sp$Sp19 , bodymass.63sp$Sp22))

Sp20n21 <- mean (c(bodymass.63sp$Sp20 , bodymass.63sp$Sp21))

Sp20n22 <- mean (c(bodymass.63sp$Sp20 , bodymass.63sp$Sp22))

Sp21n22 <- mean (c(bodymass.63sp$Sp21 , bodymass.63sp$Sp22))



## Malaconotidae  -- standard deviation
Sp18n19.sd <- sd (c(bodymass.63sp$Sp18 , bodymass.63sp$Sp19))

Sp18n20.sd <- sd (c(bodymass.63sp$Sp18 , bodymass.63sp$Sp20))

Sp18n21.sd <- sd (c(bodymass.63sp$Sp18 , bodymass.63sp$Sp21))

Sp18n22.sd <- sd (c(bodymass.63sp$Sp18 , bodymass.63sp$Sp22))

Sp19n20.sd <- sd (c(bodymass.63sp$Sp19 , bodymass.63sp$Sp20))

Sp19n21.sd <- sd (c(bodymass.63sp$Sp19 , bodymass.63sp$Sp21))

Sp19n22.sd <- sd (c(bodymass.63sp$Sp19 , bodymass.63sp$Sp22))

Sp20n21.sd <- sd (c(bodymass.63sp$Sp20 , bodymass.63sp$Sp21))

Sp20n22.sd <- sd (c(bodymass.63sp$Sp20 , bodymass.63sp$Sp22))

Sp21n22.sd <- sd (c(bodymass.63sp$Sp21 , bodymass.63sp$Sp22))




#Platysteiridae

Sp24n25 <- mean (c(bodymass.63sp$Sp24 , bodymass.63sp$Sp25))

#Platysteiridae -- standard deviation

Sp24n25.sd <- sd (c(bodymass.63sp$Sp24 , bodymass.63sp$Sp25))

#Muscicapidae -- mean

Sp26n27 <- mean (c(bodymass.63sp$Sp26 , bodymass.63sp$Sp27))

Sp26n60 <- mean (c(bodymass.63sp$Sp26 , bodymass.63sp$Sp60) )

Sp26n61 <- mean (c(bodymass.63sp$Sp26 , bodymass.63sp$Sp61)) 

Sp27n60 <- mean (c(bodymass.63sp$Sp27 , bodymass.63sp$Sp60))

Sp27n61 <- mean (c(bodymass.63sp$Sp27 , bodymass.63sp$Sp61)) 

Sp60n61 <- mean (c(bodymass.63sp$Sp60 , bodymass.63sp$Sp61))

#Muscicapidae  -- standard deviation

Sp26n27.sd <- sd (c(bodymass.63sp$Sp26 , bodymass.63sp$Sp27))

Sp26n60.sd <- sd (c(bodymass.63sp$Sp26 , bodymass.63sp$Sp60) )

Sp26n61.sd <- sd (c(bodymass.63sp$Sp26 , bodymass.63sp$Sp61)) 

Sp27n60.sd <- sd (c(bodymass.63sp$Sp27 , bodymass.63sp$Sp60))

Sp27n61.sd <- sd (c(bodymass.63sp$Sp27 , bodymass.63sp$Sp61)) 

Sp60n61.sd <- sd(c(bodymass.63sp$Sp60 , bodymass.63sp$Sp61))



##Musophagidae - mean

Sp29n30 <- mean (c(bodymass.63sp$Sp29 , bodymass.63sp$Sp30))

##Musophagidae  -- standard deviation

Sp29n30.sd <- sd (c(bodymass.63sp$Sp29 , bodymass.63sp$Sp30))


#Nectariniidae  -- mean
Sp31n32<- mean (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp32))

Sp31n33<- mean (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp33) )

Sp31n34<- mean (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp34))

Sp31n35<- mean (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp35))

Sp31n36<- mean (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp36))

Sp32n33<- mean (c(bodymass.63sp$Sp32 , bodymass.63sp$Sp33))

Sp32n34 <- mean (c(bodymass.63sp$Sp32 , bodymass.63sp$Sp34))

Sp32n35 <- mean (c(bodymass.63sp$Sp32 , bodymass.63sp$Sp35))

Sp32n36 <- mean (c(bodymass.63sp$Sp32 , bodymass.63sp$Sp36))

Sp33n34<- mean (c(bodymass.63sp$Sp33 , bodymass.63sp$Sp34))

Sp33n35 <- mean (c(bodymass.63sp$Sp33 , bodymass.63sp$Sp35))

Sp33n36 <- mean (c(bodymass.63sp$Sp33 , bodymass.63sp$Sp36))

Sp34n35 <- mean (c(bodymass.63sp$Sp34 , bodymass.63sp$Sp35))

Sp34n36 <- mean (c(bodymass.63sp$Sp34 , bodymass.63sp$Sp36)) 

Sp35n36 <- mean (c(bodymass.63sp$Sp35 , bodymass.63sp$Sp36))


#Nectariniidae -- standard deviation
Sp31n32.sd <- sd (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp32))

Sp31n33.sd <- sd (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp33) )

Sp31n34.sd <- sd (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp34))

Sp31n35.sd  <- sd (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp35))

Sp31n36.sd <- sd (c(bodymass.63sp$Sp31 , bodymass.63sp$Sp36))

Sp32n33.sd  <- sd (c(bodymass.63sp$Sp32 , bodymass.63sp$Sp33))

Sp32n34.sd <- sd (c(bodymass.63sp$Sp32 , bodymass.63sp$Sp34))

Sp32n35.sd <- sd (c(bodymass.63sp$Sp32 , bodymass.63sp$Sp35))

Sp32n36.sd <- sd (c(bodymass.63sp$Sp32 , bodymass.63sp$Sp36))

Sp33n34.sd <- sd (c(bodymass.63sp$Sp33 , bodymass.63sp$Sp34))

Sp33n35.sd <- sd (c(bodymass.63sp$Sp33 , bodymass.63sp$Sp35))

Sp33n36.sd <- sd (c(bodymass.63sp$Sp33 , bodymass.63sp$Sp36))

Sp34n35.sd <- sd (c(bodymass.63sp$Sp34 , bodymass.63sp$Sp35))

Sp34n36.sd <- sd (c(bodymass.63sp$Sp34 , bodymass.63sp$Sp36)) 

Sp35n36.sd <- sd (c(bodymass.63sp$Sp35 , bodymass.63sp$Sp36))

# Ploceidae  -- mean

Sp39n40 <- mean (c(bodymass.63sp$Sp39 , bodymass.63sp$Sp40))

# Ploceidae  -- standard deviation

Sp39n40.sd <- sd (c(bodymass.63sp$Sp39 , bodymass.63sp$Sp40))


#Pycnonotidae  -- mean
Sp42n43 <- mean (c(bodymass.63sp$Sp42 , bodymass.63sp$Sp43))

Sp42n44 <- mean (c(bodymass.63sp$Sp42 , bodymass.63sp$Sp44))

Sp43n44 <- mean (c(bodymass.63sp$Sp43 , bodymass.63sp$Sp44))


#Pycnonotidae  -- standard deviation
Sp42n43.sd <- sd (c(bodymass.63sp$Sp42 , bodymass.63sp$Sp43))

Sp42n44.sd <- sd (c(bodymass.63sp$Sp42 , bodymass.63sp$Sp44))

Sp43n44.sd <- sd (c(bodymass.63sp$Sp43 , bodymass.63sp$Sp44))


##Cisticolidae  -- mean
Sp45n46 <- mean (c(bodymass.63sp$Sp45 , bodymass.63sp$Sp46))

Sp45n50 <- mean (c(bodymass.63sp$Sp45 , bodymass.63sp$Sp50))

Sp45n51 <- mean (c(bodymass.63sp$Sp45 , bodymass.63sp$Sp51))

Sp45n54 <- mean (c(bodymass.63sp$Sp45 , bodymass.63sp$Sp54))

Sp46n50 <- mean (c(bodymass.63sp$Sp46 , bodymass.63sp$Sp50))

Sp46n51 <- mean (c(bodymass.63sp$Sp46 , bodymass.63sp$Sp51))

Sp46n54 <- mean (c(bodymass.63sp$Sp46 , bodymass.63sp$Sp54)) 

Sp50n51 <- mean (c(bodymass.63sp$Sp50 , bodymass.63sp$Sp51))

Sp50n54 <- mean (c(bodymass.63sp$Sp50 , bodymass.63sp$Sp54))

Sp51n54 <- mean (c(bodymass.63sp$Sp51 , bodymass.63sp$Sp54))



##Cisticolidae  -- standard deviation
Sp45n46.sd <- sd (c(bodymass.63sp$Sp45 , bodymass.63sp$Sp46))

Sp45n50.sd <- sd (c(bodymass.63sp$Sp45 , bodymass.63sp$Sp50))

Sp45n51.sd <- sd (c(bodymass.63sp$Sp45 , bodymass.63sp$Sp51))

Sp45n54.sd <- sd (c(bodymass.63sp$Sp45 , bodymass.63sp$Sp54))

Sp46n50.sd <- sd (c(bodymass.63sp$Sp46 , bodymass.63sp$Sp50))

Sp46n51.sd <- sd (c(bodymass.63sp$Sp46 , bodymass.63sp$Sp51))

Sp46n54.sd <- sd (c(bodymass.63sp$Sp46 , bodymass.63sp$Sp54)) 

Sp50n51.sd <- sd (c(bodymass.63sp$Sp50 , bodymass.63sp$Sp51))

Sp50n54.sd <- sd (c(bodymass.63sp$Sp50 , bodymass.63sp$Sp54))

Sp51n54.sd <- sd (c(bodymass.63sp$Sp51 , bodymass.63sp$Sp54))



#Acrocephalidae   -- mean

Sp48n49 <- mean (c(bodymass.63sp$Sp48 , bodymass.63sp$Sp49))

#Acrocephalidae   -- standard deviation

Sp48n49.sd <- sd(c(bodymass.63sp$Sp48 , bodymass.63sp$Sp49))


#Phylloscopidae   -- mean
Sp52n53 <- mean (c(bodymass.63sp$Sp52 , bodymass.63sp$Sp53))

#Phylloscopidae -- standard deviation
Sp52n53.sd <- sd(c(bodymass.63sp$Sp52 , bodymass.63sp$Sp53))

### Create a vector of standard deviations for all the 60 species pairs (within families)


All.community.sd <- c(Sp3n4.sd,Sp3n5.sd,Sp4n5.sd,Sp7n8.sd,Sp7n9.sd,Sp8n9.sd,Sp10n11.sd,Sp10n12.sd,Sp11n12.sd,Sp13n14.sd,Sp15n16.sd,Sp18n19.sd,
                      Sp18n20.sd,Sp18n21.sd,Sp18n22.sd,Sp19n20.sd,Sp19n21.sd,Sp19n22.sd,Sp20n21.sd,Sp20n22.sd,Sp21n22.sd,Sp24n25.sd,Sp26n27.sd,
                      Sp26n60.sd,Sp26n61.sd,Sp27n60.sd,Sp27n61.sd,Sp60n61.sd,Sp29n30.sd,Sp31n32.sd,Sp31n33.sd,Sp31n34.sd,Sp31n35.sd,Sp31n36.sd,
                      Sp32n33.sd,Sp32n34.sd,Sp32n35.sd,Sp32n36.sd,Sp33n34.sd,Sp33n35.sd,Sp33n36.sd,Sp34n35.sd,Sp34n36.sd,Sp35n36.sd,Sp39n40.sd,
                      Sp42n43.sd,Sp42n44.sd,Sp43n44.sd,Sp45n46.sd,Sp45n50.sd,Sp45n51.sd,Sp45n54.sd,Sp46n50.sd,Sp46n51.sd,Sp46n54.sd,Sp50n51.sd,Sp50n54.sd,
                      Sp51n54.sd,Sp48n49.sd,Sp52n53.sd)


### Create a vector of means for all the 60 species pairs (within families)

All.community.mean <- c(Sp3n4,Sp3n5,Sp4n5,Sp7n8,Sp7n9,Sp8n9,Sp10n11,Sp10n12,Sp11n12,Sp13n14,Sp15n16,Sp18n19,Sp18n20,Sp18n21,
                        Sp18n22,Sp19n20,Sp19n21,Sp19n22,Sp20n21,Sp20n22,Sp21n22,Sp24n25,Sp26n27,Sp26n60,Sp26n61,Sp27n60,Sp27n61,
                        Sp60n61,Sp29n30,Sp31n32,Sp31n33,Sp31n34,Sp31n35,Sp31n36,Sp32n33,Sp32n34,Sp32n35,Sp32n36,Sp33n34,Sp33n35,
                        Sp33n36,Sp34n35,Sp34n36,Sp35n36,Sp39n40,Sp42n43,Sp42n44,Sp43n44,Sp45n46,Sp45n50,Sp45n51,Sp45n54,Sp46n50,
                        Sp46n51,Sp46n54,Sp50n51,Sp50n54,Sp51n54,Sp48n49,Sp52n53)

#### Compute mean -coefficient of variation (Body size)
mean(All.community.sd /All.community.mean)


### Create a vector of standard deviations for species pairs with disparate elevation niches (within families)

Disp.elev.niches.sd <- c(Sp3n5.sd, Sp4n5.sd, Sp7n9.sd,Sp10n12.sd,Sp13n14.sd,Sp18n19.sd,Sp18n20.sd,Sp19n21.sd,Sp19n22.sd,Sp20n21.sd, Sp20n22.sd,Sp26n27.sd,
                         Sp26n60.sd,Sp26n61.sd,Sp27n61.sd,Sp29n30.sd,Sp31n32.sd,Sp31n34.sd,Sp31n35.sd,Sp31n36.sd,Sp32n33.sd,Sp32n34.sd,Sp32n35.sd,
                         Sp33n34.sd,Sp33n35.sd,Sp33n36.sd,Sp34n36.sd,Sp35n36.sd,Sp39n40.sd,Sp45n54.sd,Sp46n54.sd,Sp50n54.sd,Sp51n54.sd,Sp52n53.sd)


### Create a vector of means for species pairs with disparate elevation niches (within families)

Disp.elev.niches.mean <- c(Sp3n5, Sp4n5, Sp7n9,Sp10n12,Sp13n14,Sp18n19,Sp18n20,Sp19n21,Sp19n22,Sp20n21, Sp20n22,Sp26n27,Sp26n60,Sp26n61,Sp27n61,Sp29n30,Sp31n32,
                           Sp31n34,Sp31n35,Sp31n36,Sp32n33,Sp32n34,Sp32n35,Sp33n34,Sp33n35,Sp33n36,Sp34n36,Sp35n36,Sp39n40,Sp45n54,Sp46n54,Sp50n54,Sp51n54,Sp52n53)

#### Compute mean -coefficient of variation (Body size) for species pairs with disparate elevation niches
mean(Disp.elev.niches.sd / Disp.elev.niches.mean)



### Create a vector of standard deviations for species pairs with similar elevation niches (within families)
Similar.elev.niches.sd <-c(Sp31n33.sd, Sp32n36.sd, Sp34n35.sd, Sp3n4.sd,Sp45n46.sd,Sp45n50.sd,Sp45n51.sd,Sp46n50.sd, Sp46n51.sd,Sp50n51.sd,Sp18n21.sd,
                           Sp19n20.sd,Sp18n22.sd,Sp21n22.sd,Sp10n11.sd,Sp11n12.sd, Sp42n43.sd,Sp42n44.sd, Sp43n44.sd, Sp7n8.sd,Sp8n9.sd,Sp24n25.sd,
                           Sp39n40.sd,Sp15n16.sd,Sp48n49.sd,Sp27n60.sd, Sp60n61.sd)

### Create a vector of means for species pairs with with similar elevation niches (within families)
Similar.elev.niches.mean <-c(Sp31n33, Sp32n36, Sp34n35, Sp3n4,Sp45n46,Sp45n50,Sp45n51,Sp46n50, Sp46n51,Sp50n51,Sp18n21,Sp19n20,Sp18n22,Sp21n22,Sp10n11,Sp11n12,
                             Sp42n43,Sp42n44, Sp43n44, Sp7n8,Sp8n9,Sp24n25,Sp39n40,Sp15n16,Sp48n49,Sp27n60, Sp60n61)

#### Compute mean -coefficient of variation (Body size) for species pairs with similar elevation niches
mean(Similar.elev.niches.sd / Similar.elev.niches.mean)



## Number of samples (each sample = 60 species pairs) selected to generate a null model expectation
n.samples <- 1000 # or something

## Vector for storing standard deviations for 46 species (i.e) 60 species pairs
sd.bodymass <- rep(NA, 60)

## Vector for storing means for 46 species (i.e) 60 species pairs

mean.bodymass <- rep(NA, 60)

## Vector for storing coefficient of variation for each species pair
cv <- rep(NA, 60)

## Vector for storing mean coefficient of variation for each sample (i.e) 60 species pairs
mean.cv.bodymass<- rep(NA, n.samples)

## Computing null distribution for mean coefficient of variation - Body mass


for (i in 1:n.samples) {
  for (s in 1:60) { 
   
    # 
 
    # 1. Choose a random species pair from the total 63 species
    bodymass.2sp <-  as.numeric(sample(bodymass.63sp, 2,F))
    # 2. Compute the standard deviation
    sd.bodymass [s] <-  sd(bodymass.2sp)
    # 3. Compute the mean
    mean.bodymass [s] <-  mean(bodymass.2sp)
    # 4. Compute the coefficient of variation
    cv[s] <- sd.bodymass [s]/mean.bodymass [s]
    
  } # j
  
  mean.cv.bodymass [i] <- mean(cv) #    # 5. Calculate the mean coefficient of variation for the 60 species pairs and store in a vector.
                                        
  
  
} # i



## Create variable for plotiing the mean coefficient of variation null model distribution

body.mass.cv.1 <- mean.cv.bodymass
######################

### Create id for the samples
id.sample <- seq(1:1000)
### Create a dataframe for id and mean coefficient of variation 
body.mass.cv.2 <- data.frame(id.sample,body.mass.cv.1)

head(body.mass.cv.2)

## Assign col names
colnames(body.mass.cv.2) <- c("sample.id", "mean.cv")


### Plot mean coefficient of variation 
##whole community, species with disparate elevation niches, species with similar elevation niches

Figure.S2 <- ggplot(body.mass.cv.2 , aes(mean.cv)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 0.37 ), colour = "black", linetype = "dashed", linewidth = 2 )+
  geom_vline(aes(xintercept= 0.28 ), colour = "red", linetype = "dashed", linewidth= 2) +
  geom_vline(aes(xintercept= 0.47), colour = "blue", linetype = "dashed", linewidth = 2 ) 


Figure.S2 + labs (x=" Mean (Coeffiecient of variation)", y ="Frequency", color = "Legend\n",fontface= "plain") +
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




ggsave(file = "body.mass.cv.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "body.mass.cv.svg", bg = NULL, dpi = 300, width = 15, height = 10)



