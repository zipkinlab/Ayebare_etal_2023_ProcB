### Author: Samuel Ayebare
## Detection_probability.r
        ## This script generates:
                      #  Appendix S7:Figure S1. Community mean detection probabilities 
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

loadfonts()


#-----------------------#
#-Set working directory-#
#-----------------------#

#-----------------------#
#-Set working directory-#
#-----------------------#

setwd("./Data_indices_figures")

#---------------------#
#-Load Files and data-#
#---------------------#

load("../Data_indices_figures/HCDSM63spp.RData")

#------------------------#
#-Legend vegetation types-#
#-------------------------#

# 1 =  Alpine/sub-alpine i.e Reference class
# 2 = Bamboo forest, 3 = Secondary bush/shrub 4 = Grassland, 5 = Hagenia-Hypericum woodland
# 6 = Mature mixed forest, 7 = Swamp , 8 = Secondary mixed forest 

### alpine/ sub-alpine
Sigma_alpine <-  exp(HCDSM_Virunga_63spp$mean$mu_d) * 100 ## Multiplied by 100 to account for scaling of the radial distances


## Bamboo forest
Sigma_bamboo <-  exp( HCDSM_Virunga_63spp$mean$mu_d+ HCDSM_Virunga_63spp$mean$gamma[2]) * 100

### Secondary bush/shrub
Sigma_sec_bush_shrub <- exp( HCDSM_Virunga_63spp$mean$mu_d+ HCDSM_Virunga_63spp$mean$gamma[3]) * 100

## grassland
Sigma_grassland <- exp( HCDSM_Virunga_63spp$mean$mu_d+ HCDSM_Virunga_63spp$mean$gamma[4]) * 100

### Hagenia-Hypericum woodland
Sigma_Hagenia_hyp <- exp( HCDSM_Virunga_63spp$mean$mu_d+ HCDSM_Virunga_63spp$mean$gamma[5]) * 100

### Mature mixed forest
Sigma_mixed_forest <- exp( HCDSM_Virunga_63spp$mean$mu_d+ HCDSM_Virunga_63spp$mean$gamma[6]) * 100

## Swamp
Sigma_swamp <- exp( HCDSM_Virunga_63spp$mean$mu_d+ HCDSM_Virunga_63spp$mean$gamma[7]) * 100


## Secondary mixed forest
Sigma_sec_mixed_forest <- exp( HCDSM_Virunga_63spp$mean$mu_d+ HCDSM_Virunga_63spp$mean$gamma[8]) * 100




#Scale parameter per vegetation type

sigma <- c(Sigma_alpine, Sigma_Hagenia_hyp, Sigma_bamboo, Sigma_sec_bush_shrub, Sigma_mixed_forest, 
           Sigma_sec_mixed_forest,  Sigma_swamp, Sigma_grassland)


length(sigma)

## distance simulation ##
##---------------------##
dist.sim <- seq(0, 100, 1)


#Calculate detection probability across distances
distfunc <- matrix(NA, nrow = 8, ncol = length(dist.sim))
for(i in 1:8){
  for(j in 1:length(dist.sim)){
    distfunc[i,j] <- exp(-dist.sim[j]*dist.sim[j]/(2*sigma[i]*sigma[i]))
  }
}

#### Detection probability per vegetation type

Appendix.S7.Figure.S1 <- ggplot() +  
  geom_line(aes(x = dist.sim, y = distfunc[1,], color = "red"), size = 2) +
  geom_line(aes(x = dist.sim, y = distfunc[2,], color = "blue"), size = 1.2) +
  geom_line(aes(x = dist.sim, y = distfunc[3,], color = "green"), size = 1.2) +
  geom_line(aes(x = dist.sim, y = distfunc[4,], color = "black"), size = 2) +
  geom_line(aes(x = dist.sim, y = distfunc[5,], color = "brown"), size = 1.2) +
  geom_line(aes(x = dist.sim, y = distfunc[6,], color = "orange"), size = 1.2) +
  geom_line(aes(x = dist.sim, y = distfunc[7,], color = "grey"), size = 1.2) +
  geom_line(aes(x = dist.sim, y = distfunc[8,], color = "purple"), size = 1.2) +
  
  annotate("text", x = 25, y = 1, hjust = 0, size = 8, family = "Times New Roman", 
           label = "", fontface= "bold")

Appendix.S7.Figure.S1 + labs (x="Distance(m)", y ="Detection probability", color = "Legend\n",fontface= "bold") +
  scale_color_manual(labels = c("Alpine/sub-alpine", "Hagenia-Hypericum woodland","Bamboo forest","Secondary bush/shrub",
                                "Secondary mixed forest", "Mature mixed forest", "Swamp","Grassland"), 
                     values=c( "red"= "red", "blue"="blue", "green"= "green", "black"="black",
                                    "brown"= "brown", "orange"= "orange", "grey"= "grey", "purple"= "purple" ),name = "Vegetation categories") +
                                      
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
        legend.text=element_text(size=25),
        legend.position = c(0.7,0.7), legend.title=element_text(size=25, color="black"))


ggsave(file = "Appendix.S7.Figure.S1.jpg", bg = NULL, dpi = 300, width = 15, height = 10)




