### Author: Samuel Ayebare
## Rank_abundance_plot.r

## This script
              # generates
                         # Appendix S1: Figures S1 - S2




######################################
######Appendix S1: Figure S1 ########
#####################################




#----------------#
#-Load libraries-#
#----------------#
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")


#-----------------------#
#-Set working directory-#
#-----------------------#

setwd("./Data_Rank_abundance_plots")


# Importing species relative abundance (i.e., This study & Derhé et al. (2020))
Spp_relative_abundance <- read.csv("Rank.sp51.csv",header = T)

head(Spp_relative_abundance)

## Rank relative abundance based on the Derhé et al. (2020) multi-year surveys
## Note: to enable direct comparison between our single survey observed counts (i.e., 2005) and 
## the  Derhé et al. (2020) multi-year surveys, we divided the reported relative 
## abundance values by 10 (i.e.,5 years, 2013 – 2018, twice each year)

sp.rank <- rank(desc(Spp.Rltv.abc.Virunga.massif$Derhe.et.al.2020))
length(sp.rank)

## Create a data frame
Virunga.volcanoes.rank <- data.frame(Spp_relative_abundance,sp.rank )

## Rank abundance plot (Derhé et al. (2020))

 Figure.S1 <-  ggplot(Virunga.volcanoes.rank, aes(x=sp.rank)) + 
  geom_point(aes(y = Derhe.et.al.2020), color="blue", size =3) +
  geom_line(aes(y = Derhe.et.al.2020), color="blue", linetype="twodash", size = 1) +
   
## Plot relative abundance of the same species observed in this study on the rank abundance plot
  geom_point(aes(y = This.study), color = "black", size =3)+
  geom_line(aes(y = This.study), color = "black", size = 1) 
  
  
   Figure.S1 + labs (x=" Rank Abundance (Derhe.et.al.2020) ", y ="Relative abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(name = " This study", 
                     
                     breaks=c('This study',  'Derhe.et.al.2020'),
                     values=c( "black"= "black", "blue" = "blue")) +
                        scale_x_continuous(breaks=seq(1,50,4)) +
  
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




ggsave(file = "Rank.abundance.plot.svg", bg = NULL, dpi = 300, width = 15, height = 10)



#####################################
######Appendix S1: Figure S2 ########
#####################################


################################
#### Parc National des Volans ##
################################
## Comparing the relative abundance per species from our data set considering only Parc National des Volcans


# Importing species relative abundance (i.e., This study & Derhé et al. (2020))

Spp_relative_abundance.Volc <- read.csv("Rank.sp44.csv",header = T)
head(Spp_relative_abundance.Volc)

## Rank relative abundance based on the Derhé et al. (2020) multi-year surveys
## Note: to enable direct comparison between our single survey observed counts (i.e., 2005) and 
## the  Derhé et al. (2020) multi-year surveys, we divided the reported relative 
## abundance values by 10 (i.e.,5 years, 2013 – 2018, twice each year)

sp.rank.2 <- rank(desc(Spp_relative_abundance.Volc$Derhe.et.al.2020))
length(sp.rank.2)

## Create a data frame
Virunga.volcanoes.rank.2 <- data.frame(Spp_relative_abundance.Volc,sp.rank.2 )

## Rank abundance plot (Derhé et al. (2020))

Figure.S2 <-  ggplot(Virunga.volcanoes.rank.2, aes(x=sp.rank.2)) + 
  geom_point(aes(y = Derhe.et.al.2020), color="blue", size =3) +
  geom_line(aes(y = Derhe.et.al.2020), color="blue", linetype="twodash", size = 1) +
  
  ## Plot relative abundance of the same species observed in this study (ie., only Parc National des Volcans) on the rank abundance plot
  geom_point(aes(y = This.study), color = "black", size =3)+
  geom_line(aes(y = This.study), color = "black", size = 1) 


Figure.S2 + labs (x=" Rank Abundance (Derhe.et.al.2020) ", y ="Relative abundance", color = "Legend\n",fontface= "plain") +
  scale_color_manual(name = " This study", 
                     
                     breaks=c('This study',  'Derhe.et.al.2020'),
                     values=c( "black"= "black", "blue" = "blue")) +
  scale_x_continuous(breaks=seq(1,44,3)) +
  
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




ggsave(file = "Rank.abundance.plot.volcanoes.svg", bg = NULL, dpi = 300, width = 15, height = 10)




