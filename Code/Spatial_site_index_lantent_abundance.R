### Author: Samuel Ayebare
## Spatial_site_index_lantent_abundance.r

## This script
              # Estimates the Spatial Site index using latent abundance
              # generates the null model distribution and figure for the Spatial Site index 
              # Appendix S4: Figure S1



#----------------#
#-Load libraries-#
#----------------#

rm(list = ls())
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")
# For calculating highest density interval
library(HDInterval)


#-----------------------#
#-Set working directory-#
#-----------------------#

setwd("./Data_spatial_site_index")


######################################
###### Appendix S4: Figure S1 ########
######################################

# Importing latent abundance for all the 63 species pairs

Community.63sp <- read.csv("Community.63sp.csv",header = T)
head(Community.63sp)

# Note the -1 removes the first column, which was just row indices
dat.scaled <- apply(Community.63sp[, -1], 2, function(a) a / max(a))

# Binarize
dat.bin <- dat.scaled
for (i in 1:ncol(dat.bin)) {
  # The 75% quantile for the current species
  curr.quant <- quantile(dat.bin[, i], 0.75)
  dat.bin[, i] <- ifelse(dat.bin[, i] >= curr.quant, 1, 0)
}

## Create a dataframe
dat.binary <- as.data.frame(dat.bin)



### Estimate Spatial Site index using latent abundance for species pairs within families
# Lybiidae
Sp3n4 <- sum(dat.binary$Spp3 & dat.binary$Spp4) / 
  sum(dat.binary$Spp3 | dat.binary$Spp4)
Sp3n5 <- sum(dat.binary$Spp3 & dat.binary$Spp5) / 
  sum(dat.binary$Spp3 | dat.binary$Spp5)
Sp4n5 <- sum(dat.binary$Spp4 & dat.binary$Spp5) / 
  sum(dat.binary$Spp4 | dat.binary$Spp5)

##Columbidae
Sp7n8 <- sum(dat.binary$Spp7 & dat.binary$Spp8) / 
  sum(dat.binary$Spp7 | dat.binary$Spp8)
Sp7n9 <- sum(dat.binary$Spp7 & dat.binary$Spp9) / 
  sum(dat.binary$Spp7 | dat.binary$Spp9)
Sp8n9 <- sum(dat.binary$Spp8 & dat.binary$Spp9) / 
  sum(dat.binary$Spp8 | dat.binary$Spp9)

# Cuculidae
Sp10n11 <- sum(dat.binary$Spp10 & dat.binary$Spp11) / 
  sum(dat.binary$Spp10 | dat.binary$Spp11)
Sp10n12 <- sum(dat.binary$Spp10 & dat.binary$Spp12) / 
  sum(dat.binary$Spp10 | dat.binary$Spp12)
Sp11n12 <- sum(dat.binary$Spp11 & dat.binary$Spp12) / 
  sum(dat.binary$Spp11 | dat.binary$Spp12)

# Estrildidae
Sp13n14 <- sum(dat.binary$Spp13 & dat.binary$Spp14) / 
  sum(dat.binary$Spp13 | dat.binary$Spp14)

##Fringillidae

Sp15n16 <- sum(dat.binary$Spp15 & dat.binary$Spp16) / 
  sum(dat.binary$Spp15 | dat.binary$Spp16)


## Malaconotidae
Sp18n19 <- sum(dat.binary$Spp18 & dat.binary$Spp19) / 
  sum(dat.binary$Spp18 | dat.binary$Spp19)
Sp18n20 <- sum(dat.binary$Spp18 & dat.binary$Spp20) / 
  sum(dat.binary$Spp18 | dat.binary$Spp20)
Sp18n21 <- sum(dat.binary$Spp18 & dat.binary$Spp21) / 
  sum(dat.binary$Spp18 | dat.binary$Spp21)
Sp18n22 <- sum(dat.binary$Spp18 & dat.binary$Spp22) / 
  sum(dat.binary$Spp18 | dat.binary$Spp22)
Sp19n20 <- sum(dat.binary$Spp19 & dat.binary$Spp20) / 
  sum(dat.binary$Spp19 | dat.binary$Spp20)
Sp19n21 <- sum(dat.binary$Spp19 & dat.binary$Spp21) / 
  sum(dat.binary$Spp19 | dat.binary$Spp21)
Sp19n22 <- sum(dat.binary$Spp19 & dat.binary$Spp22) / 
  sum(dat.binary$Spp19 | dat.binary$Spp22)
Sp20n21 <- sum(dat.binary$Spp20 & dat.binary$Spp21) / 
  sum(dat.binary$Spp20 | dat.binary$Spp21)
Sp20n22 <- sum(dat.binary$Spp20 & dat.binary$Spp22) / 
  sum(dat.binary$Spp20 | dat.binary$Spp22)
Sp21n22 <- sum(dat.binary$Spp21 & dat.binary$Spp22) / 
  sum(dat.binary$Spp21 | dat.binary$Spp22)


#Platysteiridae

Sp24n25 <- sum(dat.binary$Spp24 & dat.binary$Spp25) / 
  sum(dat.binary$Spp24 | dat.binary$Spp25)

#Muscicapidae

Sp26n27 <- sum(dat.binary$Spp26 & dat.binary$Spp27) / 
  sum(dat.binary$Spp26 | dat.binary$Spp27)
Sp26n60 <- sum(dat.binary$Spp26 & dat.binary$Spp60) / 
  sum(dat.binary$Spp26 | dat.binary$Spp60)
Sp26n61 <- sum(dat.binary$Spp26 & dat.binary$Spp61) / 
  sum(dat.binary$Spp26 | dat.binary$Spp61)
Sp27n60 <- sum(dat.binary$Spp27 & dat.binary$Spp60) / 
  sum(dat.binary$Spp27 | dat.binary$Spp60)
Sp27n61 <- sum(dat.binary$Spp27 & dat.binary$Spp61) / 
  sum(dat.binary$Spp27 | dat.binary$Spp61)
Sp60n61 <- sum(dat.binary$Spp60 & dat.binary$Spp61) / 
  sum(dat.binary$Spp60 | dat.binary$Spp61)

##Musophagidae

Sp29n30 <- sum(dat.binary$Spp29 & dat.binary$Spp30) / 
  sum(dat.binary$Spp29 | dat.binary$Spp30)

#Nectariniidae
Sp31n32<- sum(dat.binary$Spp31 & dat.binary$Spp32) / 
  sum(dat.binary$Spp31 , dat.binary$Spp32)
Sp31n33<- sum(dat.binary$Spp31 & dat.binary$Spp33) / 
  sum(dat.binary$Spp31 | dat.binary$Spp33)
Sp31n34<- sum(dat.binary$Spp31 & dat.binary$Spp34) / 
  sum(dat.binary$Spp31 | dat.binary$Spp34)
Sp31n35<- sum(dat.binary$Spp31 & dat.binary$Spp35) / 
  sum(dat.binary$Spp31 | dat.binary$Spp35)
Sp31n36<- sum(dat.binary$Spp31 & dat.binary$Spp36) / 
  sum(dat.binary$Spp31 | dat.binary$Spp36)
Sp32n33<- sum(dat.binary$Spp32 & dat.binary$Spp33) / 
  sum(dat.binary$Spp32 | dat.binary$Spp33)
Sp32n34 <- sum(dat.binary$Spp32 & dat.binary$Spp34) / 
  sum(dat.binary$Spp32 | dat.binary$Spp34)
Sp32n35 <- sum(dat.binary$Spp32 & dat.binary$Spp35) / 
  sum(dat.binary$Spp32 | dat.binary$Spp35)
Sp32n36 <- sum(dat.binary$Spp32 & dat.binary$Spp36) / 
  sum(dat.binary$Spp32 | dat.binary$Spp36)
Sp33n34<- sum(dat.binary$Spp33 & dat.binary$Spp34) / 
  sum(dat.binary$Spp33 | dat.binary$Spp34)
Sp33n35 <- sum(dat.binary$Spp33 & dat.binary$Spp35) / 
  sum(dat.binary$Spp33 | dat.binary$Spp35)
Sp33n36 <- sum(dat.binary$Spp33 & dat.binary$Spp36) / 
  sum(dat.binary$Spp33 | dat.binary$Spp36)
Sp34n35 <- sum(dat.binary$Spp34 & dat.binary$Spp35) / 
  sum(dat.binary$Spp34 | dat.binary$Spp35)
Sp34n36 <- sum(dat.binary$Spp34 & dat.binary$Spp36) / 
  sum(dat.binary$Spp34 | dat.binary$Spp36)
Sp35n36 <- sum(dat.binary$Spp35 & dat.binary$Spp36) / 
  sum(dat.binary$Spp35 | dat.binary$Spp36)


# Ploceidae

Sp39n40 <- sum(dat.binary$Spp39 & dat.binary$Spp40) / 
  sum(dat.binary$Spp39 | dat.binary$Spp40)

#Pycnonotidae
Sp42n43 <- sum(dat.binary$Spp42 & dat.binary$Spp43) / 
  sum(dat.binary$Spp42 | dat.binary$Spp43)
Sp42n44 <- sum(dat.binary$Spp42 & dat.binary$Spp44) / 
  sum(dat.binary$Spp42 | dat.binary$Spp44)
Sp43n44 <- sum(dat.binary$Spp43 & dat.binary$Spp44) / 
  sum(dat.binary$Spp43 | dat.binary$Spp44)

##Cisticolidae
Sp45n46 <- sum(dat.binary$Spp45 & dat.binary$Spp46) / 
  sum(dat.binary$Spp45 | dat.binary$Spp46)
Sp45n50 <- sum(dat.binary$Spp45 & dat.binary$Spp50) / 
  sum(dat.binary$Spp45 | dat.binary$Spp50)
Sp45n51 <- sum(dat.binary$Spp45 & dat.binary$Spp51) / 
  sum(dat.binary$Spp45 | dat.binary$Spp51)
Sp45n54 <- sum(dat.binary$Spp45 & dat.binary$Spp54) / 
  sum(dat.binary$Spp45 | dat.binary$Spp54)
Sp46n50 <- sum(dat.binary$Spp46 & dat.binary$Spp50) / 
  sum(dat.binary$Spp46 | dat.binary$Spp50)
Sp46n51 <- sum(dat.binary$Spp46 & dat.binary$Spp51) / 
  sum(dat.binary$Spp46 | dat.binary$Spp51)
Sp46n54 <- sum(dat.binary$Spp46 & dat.binary$Spp54) / 
  sum(dat.binary$Spp46 | dat.binary$Spp54)
Sp50n51 <- sum(dat.binary$Spp50 & dat.binary$Spp51) / 
  sum(dat.binary$Spp50| dat.binary$Spp51)
Sp50n54 <- sum(dat.binary$Spp50 & dat.binary$Spp54) / 
  sum(dat.binary$Spp50| dat.binary$Spp54)
Sp51n54 <- sum(dat.binary$Spp51 & dat.binary$Spp54) / 
  sum(dat.binary$Spp51| dat.binary$Spp54)

#Acrocephalidae

Sp48n49 <- sum(dat.binary$Spp48 & dat.binary$Spp49) / 
  sum(dat.binary$Spp48| dat.binary$Spp49)

#Phylloscopidae
Sp52n53 <- sum(dat.binary$Spp52 & dat.binary$Spp53) / 
  sum(dat.binary$Spp52| dat.binary$Spp53)


### Create a vector of spatial site index for all the 60 species pairs  (within families)

Community <- c(Sp3n4,Sp7n8,Sp7n9,Sp8n9,Sp10n11,Sp10n12,Sp11n12,Sp15n16,Sp18n19,Sp18n20,Sp18n21,
                           Sp18n22,Sp19n20,Sp19n21,Sp19n22,Sp20n21,Sp20n22,Sp21n22,Sp24n25,Sp26n27,Sp26n60,Sp26n61,Sp27n60,Sp27n61,
                           Sp60n61,Sp29n30,Sp31n32,Sp31n33,Sp31n34,Sp31n35,Sp31n36,Sp32n33,Sp32n34,Sp32n35,Sp32n36,Sp33n34,Sp33n35,
                           Sp33n36,Sp34n35,Sp34n36,Sp35n36,Sp39n40,Sp42n43,Sp42n44,Sp43n44,Sp45n46,Sp45n50,Sp45n51,Sp45n54,Sp46n50,
                           Sp46n51,Sp46n54,Sp50n51,Sp50n54,Sp51n54,Sp48n49,Sp52n53)

mean(Community)


### Create a vector of spatial site index for species pairs with simialar elevation niches (within families)
Similar.elev.niches <-c(Sp31n33, Sp32n36, Sp34n35, Sp3n4,Sp45n46,Sp45n50,Sp45n51,Sp46n50, Sp46n51,Sp50n51,Sp18n21,Sp19n20,Sp18n22,Sp21n22,Sp10n11,Sp11n12,
                        Sp42n43,Sp42n44, Sp43n44, Sp7n8,Sp8n9,Sp24n25,Sp39n40,Sp15n16,Sp48n49,Sp27n60, Sp60n61)
mean(Similar.elev.niches)

### Create a vector of spatial site index for species pairs with disparate elevation niches (within families)
Disp.elev.niches <- c(Sp3n5, Sp4n5, Sp7n9,Sp10n12,Sp13n14,Sp18n19,Sp18n20,Sp19n21,Sp19n22,Sp20n21, Sp20n22,Sp26n27,Sp26n60,Sp26n61,Sp27n61,Sp29n30,Sp31n32,
                      Sp31n34,Sp31n35,Sp31n36,Sp32n33,Sp32n34,Sp32n35,Sp33n34,Sp33n35,Sp33n36,Sp34n36,Sp35n36,Sp39n40,Sp45n54,Sp46n54,Sp50n54,Sp51n54,Sp52n53)
mean(Disp.elev.niches)




## Number of samples (each sample = 60 species pairs) selected to generate a null model expectation
n.samples <- 1000

## Vector for storing npatial site index for 46 species (i.e) 60 species pairs

null.site.indx <- rep(NA, 60)

## Vector for storing mean site index across species
mean.site.indx.samples <- rep(NA, n.samples)

## Computing null distribution for the mean spatial site index - lantent abundance

for (i in 1:n.samples) {
  for (s in 1:60) {
    curr.indx <- sample(1:ncol(dat.binary), 2, FALSE)
    null.site.indx[s]  <- sum(dat.binary[, curr.indx[1]] & dat.binary[, curr.indx[2]]) / 
      sum(dat.binary[, curr.indx[1]] | dat.binary[, curr.indx[2]])
  } # j
  mean.site.indx.samples[i] <- mean(null.site.indx)
} # i



write.csv(mean.site.indx.samples,"mean.site.indx.samples.csv")



###

# Importing mean spatial site index - lantent abundance

mean.site.indx.lantent <- read.csv("mean.site.indx.samples.latent.csv", header = T)

## Assign col names
colnames(mean.site.indx.lantent) <- c("sample.id", "mean.c.o")



### ### Plot mean spatial site index - lantent abundance
##Mean spatial site index (whole community, species with disparate elevation niches, species with similar elevation niches)

Appendix.S4.Figure.S1 <- ggplot(mean.site.indx.samples , aes(mean.c.o)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 0.39 ), colour = "black", linetype = "dashed", linewidth = 2 )+
  geom_vline(aes(xintercept= 0.59 ), colour = "red", linetype = "dashed", linewidth = 2 ) +
  geom_vline(aes(xintercept= 0.22 ), colour = "blue", linetype = "dashed", linewidth = 2) 


Appendix.S4.Figure.S1 + labs (x=" Mean (spatial site index)", y ="Frequency", color = "Legend\n",fontface= "plain") +
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




ggsave(file = "Spatial.site.indice.lantent.abundance.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Spatial.site.indice.lantent.abundance.svg", bg = NULL, dpi = 300, width = 15, height = 10)

