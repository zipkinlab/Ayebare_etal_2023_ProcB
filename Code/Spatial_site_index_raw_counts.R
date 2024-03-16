### Author: Samuel Ayebare
## Spatial_site_index_raw_counts.r

## This script
              # Estimates the Spatial Site index using raw counts
              # Table 1



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
setwd("./Data")


# # Import raw count data

Raw_counts <- read.csv("Raw_counts.csv", header=TRUE)

head(Raw_counts)
dim(Raw_counts)

# Note the -1 removes the first column, which was site ids
Raw_counts.1 <- (Raw_counts[, -1])


# Binarize

dat.binary <- as.data.frame(ifelse(Raw_counts.1 >= 1, 1, 0))

## Species id (i.e, description is in Appendix S5)

### Estimate Spatial Site index using raw counts for species pairs within families

# Lybiidae
Sp3n4 <- sum(dat.binary$Spp3 & dat.binary$Spp4) / 
  sum(dat.binary$Spp3 | dat.binary$Spp4)
Sp3n5 <- sum(dat.binary$Spp3 & dat.binary$Spp5) / 
  sum(dat.binary$Spp3 | dat.binary$Spp5)
Sp4n5 <- sum(dat.binary$Spp4 & dat.binary$Spp5) / 
  sum(dat.binary$Spp4 | dat.binary$Spp5)


Lybiidae.ssi <- c(Sp3n4,Sp3n5,Sp4n5 )
Lybiidae.ssi 

##Columbidae
Sp7n8 <- sum(dat.binary$Spp7 & dat.binary$Spp8) / 
  sum(dat.binary$Spp7 | dat.binary$Spp8)
Sp7n9 <- sum(dat.binary$Spp7 & dat.binary$Spp9) / 
  sum(dat.binary$Spp7 | dat.binary$Spp9)
Sp8n9 <- sum(dat.binary$Spp8 & dat.binary$Spp9) / 
  sum(dat.binary$Spp8 | dat.binary$Spp9)

Columbidae.ssi <- c(Sp7n8,Sp7n9,Sp8n9)
Columbidae.ssi

# Cuculidae
Sp10n11 <- sum(dat.binary$Spp10 & dat.binary$Spp11) / 
  sum(dat.binary$Spp10 | dat.binary$Spp11)
Sp10n12 <- sum(dat.binary$Spp10 & dat.binary$Spp12) / 
  sum(dat.binary$Spp10 | dat.binary$Spp12)
Sp11n12 <- sum(dat.binary$Spp11 & dat.binary$Spp12) / 
  sum(dat.binary$Spp11 | dat.binary$Spp12)

Cuculidae.ssi <- c(Sp10n11,Sp10n12,Sp11n12)
Cuculidae.ssi

# Estrildidae
Sp13n14 <- sum(dat.binary$Spp13 & dat.binary$Spp14) / 
  sum(dat.binary$Spp13 | dat.binary$Spp14)

Estrildidae.ssi <- Sp13n14
Estrildidae.ssi

##Fringillidae
Sp15n16 <- sum(dat.binary$Spp15 & dat.binary$Spp16) / 
  sum(dat.binary$Spp15 | dat.binary$Spp16)

Fringillidae.ssi <- Sp15n16
Fringillidae.ssi 

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

Malaconotidae.ssi <- c(Sp18n19, Sp18n20,Sp18n21,Sp18n22,Sp19n20,Sp19n21,Sp19n22,Sp20n21,Sp20n22,Sp21n22)
Malaconotidae.ssi

#Platysteiridae
Sp24n25 <- sum(dat.binary$Spp24 & dat.binary$Spp25) / 
  sum(dat.binary$Spp24 | dat.binary$Spp25)

Platysteiridae.ssi <- Sp24n25
Platysteiridae.ssi

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

Muscicapidae.ssi <- c(Sp26n27,Sp26n60,Sp26n61,Sp27n60,Sp27n61,Sp60n61)
Muscicapidae.ssi

##Musophagidae
Sp29n30 <- sum(dat.binary$Spp29 & dat.binary$Spp30) / 
  sum(dat.binary$Spp29 | dat.binary$Spp30)

Musophagidae.ssi <- Sp29n30 
Musophagidae.ssi

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

Nectariniidae.ssi <- c(Sp31n32,Sp31n33,Sp31n34,Sp31n35,Sp31n36,Sp32n33,Sp32n34,Sp32n35,
                       Sp32n36,Sp33n34,Sp33n35,Sp33n36,Sp34n35,Sp34n36,Sp35n36)
Nectariniidae.ssi
# Ploceidae

Sp39n40 <- sum(dat.binary$Spp39 & dat.binary$Spp40) / 
  sum(dat.binary$Spp39 | dat.binary$Spp40)

Ploceidae.ssi <- Sp39n40
Ploceidae.ssi

#Pycnonotidae
Sp42n43 <- sum(dat.binary$Spp42 & dat.binary$Spp43) / 
  sum(dat.binary$Spp42 | dat.binary$Spp43)
Sp42n44 <- sum(dat.binary$Spp42 & dat.binary$Spp44) / 
  sum(dat.binary$Spp42 | dat.binary$Spp44)
Sp43n44 <- sum(dat.binary$Spp43 & dat.binary$Spp44) / 
  sum(dat.binary$Spp43 | dat.binary$Spp44)

Pycnonotidae.ssi <- c(Sp42n43,Sp42n44, Sp43n44)
Pycnonotidae.ssi

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

Cisticolidae.ssi <- c(Sp45n46,Sp45n50,Sp45n51,Sp45n54,Sp46n50,Sp46n51,Sp46n54,Sp50n51,Sp50n54,Sp51n54)
Cisticolidae.ssi

#Acrocephalidae

Sp48n49 <- sum(dat.binary$Spp48 & dat.binary$Spp49) / 
  sum(dat.binary$Spp48| dat.binary$Spp49)

Acrocephalidae.ssi <- Sp48n49
Acrocephalidae.ssi

#Phylloscopidae
Sp52n53 <- sum(dat.binary$Spp52 & dat.binary$Spp53) / 
  sum(dat.binary$Spp52| dat.binary$Spp53)

Phylloscopidae.ssi <- Sp52n53
Phylloscopidae.ssi