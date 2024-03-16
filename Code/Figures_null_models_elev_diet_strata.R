### Author: Samuel Ayebare
## Figures_null_models_elev_diet_strata.r

## This script
                # generates figures for null model distributions (i.e., elevation, diet, strata)
                # Appendix S3: Figure S1




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

setwd("./Data")


##############################
###### Figure S1 (a)########
##############################

# Importing weak elevation niche partitioning indices
Null.elev.w <- read.csv("prop.weak.overlap.samples.elev.csv",header = T)

## Assign col names
colnames(Null.elev.w) <- c("sample.id", "prop.w.ov")

head(Null.elev.w)


### Observed weak elevation niche partitioning at community level
# 55%
#	We considered elevation niche partitioning weak when the index was < 0.5 

# P(null > observed ) 
# = 0
sum(Null.elev.w$prop.w.ov > 55)/1000


### Plot Weak elevation niche overlap
Null.model.elev.dist <- ggplot(Null.elev.w , aes(prop.w.ov)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 55 ), colour = "black", linetype = "dashed", linewidth = 2 )


Null.model.elev.dist + labs (x=" Proportion (weak elevation niche overlap (%))", y ="Frequency", color = "Legend\n",fontface= "plain") +
  scale_x_continuous(breaks=seq(0,60,5)) +
  
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




ggsave(file = "Weak.elevation.ap.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Weak.elevation.ap.svg", bg = NULL, dpi = 300, width = 15, height = 10)







##############################
###### Figure S1 (b)########
##############################

# Importing strong elevation niche partitioning indices

Null.elev.s <- read.csv("prop.strong.overlap.samples.elev.csv",header = T)

## Assign col names
colnames(Null.elev.s) <- c("sample.id", "prop.s.ov")

head(Null.elev.s)


### Observed strong elevation niche partitioning at community level
# 45%
#	We considered elevational niche partitioning strong when the index was ≥ 0.5

# P(null < observed ) 
# = 0
sum(Null.elev.w$prop.s.ov < 45)/1000




### Plot Strong elevation niche overlap
Null.model.elev.s <- ggplot(Null.elev.s , aes(prop.s.ov)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 45 ), colour = "black", linetype = "dashed", linewidth = 2 )


Null.model.elev.s + labs (x=" Proportion (strong elevation niche overlap(%))", y ="Frequency", color = "Legend\n",fontface= "plain") +
  scale_x_continuous(breaks=seq(0,100,5)) +
  
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




ggsave(file = "Strong.elevation.ap.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Strong.elevation.ap.svg", bg = NULL, dpi = 300, width = 15, height = 10)



#### Diet

##############################
###### Figure S1 (c)########
##############################

# Importing weak diet niche partitioning indices

Null.diet.w <- read.csv("prop.weak.overlap.samples.d.csv",header = T)

## Assign col names
colnames(Null.diet.w) <- c("sample.id", "prop.d.ov")

head(Null.diet.w)


### Observed weak diet niche partitioning at community level
# 0
#	We considered diet niche partitioning weak when the index was ≤ 0.6 



### Plot weak diet niche overlap

Null.model.diet.w <- ggplot(Null.diet.w , aes(prop.d.ov)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 0 ), colour = "black", linetype = "dashed", linewidth = 2 )


Null.model.diet.w + labs (x=" Proportion (weak diet niche overlap(%))", y ="Frequency", color = "Legend\n",fontface= "plain") +
  scale_x_continuous(breaks=seq(0,60,5)) +
  
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




ggsave(file = "Weak.diet.ap.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Weak.diet.ap.svg", bg = NULL, dpi = 300, width = 15, height = 10)



##############################
###### Figure S1 (d)########
##############################

# Importing strong diet niche partitioning indices

Null.diet.s <- read.csv("prop.strong.overlap.samples.d.csv",header = T)


## Assign col names
colnames(Null.diet.s) <- c("sample.id", "prop.d.ov")

head(Null.diet.s)
### Observed strong diet niche partitioning at community level
# 1
#We considered diet niche partitioning weak strong when the index was > 0.6. 


### Plot strong diet niche overlap

Null.model.diet.s <- ggplot(Null.diet.s , aes(prop.d.ov)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 100 ), colour = "black", linetype = "dashed", linewidth = 2 )


Null.model.diet.s + labs (x=" Proportion (strong diet niche overlap(%))", y ="Frequency", color = "Legend\n",fontface= "plain") +
  scale_x_continuous(breaks=seq(0,104,5)) +
  
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




ggsave(file = "Strong.diet.ap.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Strong.diet.ap.svg", bg = NULL, dpi = 300, width = 15, height = 10)




#### Forest strata

##############################
###### Figure S1 (e)########
##############################

# Importing weak strata niche partitioning indices

Null.strata.w <- read.csv("prop.weak.overlap.samples.s.csv",header = T)

## Assign col names
colnames(Null.strata.w) <- c("sample.id", "prop.s.ov")

head(Null.strata.w)


### Observed weak strata niche partitioning at community level
# 37
#	We considered diet niche partitioning weak when the index was ≤ 0.6 
#P(observed < null) = 0.025) 
sum(Null.strata.w$prop.s.ov < 37)/1000
## 0.025


# ### Plot weak strata niche overlap
Null.model.strata.w <- ggplot(Null.strata.w , aes(prop.s.ov)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 37 ), colour = "black", linetype = "dashed", linewidth = 2 )


Null.model.strata.w + labs (x=" Proportion (weak strata niche overlap(%))", y ="Frequency", color = "Legend\n",fontface= "plain") +
  scale_x_continuous(breaks=seq(0,70,5)) +
  
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
        axis.title.x = element_text(size = 30, angle = 00))



ggsave(file = "Weak.strata.ap.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Weak.strata.ap.svg", bg = NULL, dpi = 300, width = 15, height = 10)





##############################
###### Figure S1 (d)########
##############################

# Importing strong strata niche partitioning indices

Null.strata.s <- read.csv("prop.strong.overlap.samples.s.csv",header = T)

## Assign col names
colnames(Null.strata.s) <- c("sample.id", "prop.s.ov")

head(Null.strata.s)


### Observed strong strata niche partitioning at community level
# 63
#	We considered strata niche partitioning strong when the index was > 0.6 
#P(observed > null) = 0.025) 
sum(Null.strata.s$prop.s.ov > 63)/1000
## 0.025


### Plot strong strata niche overlap
Null.model.strata.w <- ggplot(Null.strata.s , aes(prop.s.ov)) +
  geom_histogram( bins = 15) +
  geom_vline(aes(xintercept= 63 ), colour = "black", linetype = "dashed", size = 2 )


Null.model.strata.w + labs (x=" Proportion (strong strata niche overlap(%))", y ="Frequency", color = "Legend\n",fontface= "plain") +
  scale_x_continuous(breaks=seq(0,70,5)) +
  
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
        axis.title.x = element_text(size = 30, angle = 00))



ggsave(file = "Strong.strata.ap.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Strong.strata.ap.svg", bg = NULL, dpi = 300, width = 15, height = 10)



























