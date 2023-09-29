# Ayebare_etal_2023_ProcB

An environmental habitat gradient and within-habitat segregation enable co-existence of ecologically similar bird species

# Data/Code
https://doi.org/10.5061/dryad.fttdz08z8

https://doi.org/10.5281/zenodo.7951611

# Abstract
Niche theory predicts that ecologically similar species can coexist through multidimensional niche partitioning. However, owing to the challenges of accounting for both abiotic and biotic processes in ecological niche modelling, the underlying mechanisms that facilitate coexistence of competing species are poorly understood. In this study, we evaluated potential mechanisms underlying the coexistence of ecologically similar bird species in a biodiversity-rich transboundary montane forest in east-central Africa by computing niche overlap indices along an environmental elevation gradient, diet, forest strata, activity patterns and within-habitat segregation across horizontal space. We found strong support for abiotic environmental habitat niche partitioning, with 55% of species pairs having separate elevation niches. For the remaining species pairs that exhibited similar elevation niches, we found that within-habitat segregation across horizontal space and to a lesser extent vertical forest strata provided the most likely mechanisms of species coexistence. Coexistence of ecologically similar species within a highly diverse montane forest was determined primarily by abiotic factors (e.g. environmental elevation gradient) that characterize the Grinnellian niche and secondarily by biotic factors (e.g. vertical and horizontal segregation within habitats) that describe the Eltonian niche. Thus, partitioning across multiple levels of spatial organization is a key mechanism of coexistence in diverse communities.

------------------------
# General description: Data
------------------------
This dataset (seven folders) consists of bird point counts, elevation values, diet categories, foraging vertical strata categories, body size variation, proportion of weak and strong niche overlap indices (i.e., elevation, diet, strata), rank abundances input files, and spatial site index input files. The datasets were used to assess potential mechanisms underlying the co-existence of ecologically similar bird species along abiotic (environmental elevation gradient), biotic (diet, vertical foraging strata, body size, and within-habitat segregation across horizontal space- spatial site index) factors, and generating null model distributions in the Virunga volcanoes (East-central Africa).

All the datasets are in .csv format and .Rdata format (i.e., output from R programing software).

-----------------------------------------
# General description: Code/Software 
--------------------------------------------

Software code for running a hierarchical community distance sampling model (i.e., 63species), 
extension of the a hierarchical community distance sampling model  (i.e., warblers (5 species)- Cisticolidae), 
niche overlap indices and null model distributions is available at Zenodo.

Parameters for our hierarchical community distance sampling models were estimated using a Bayesian approach 
with the programs R (R Core Team 2020) and JAGS (Plummer 2003) using the jagsUI package (Kellner 2021).

All analysis was carried out using program R (R Core Team 2020).


*********************************************
# Data_*  
********************************************
The folders contain data used in the analysis

##############################For the HCDSM63spp.RData (284mbs) ########################

Contains parameter estimates from the hierarchical community distance sampling model (63 bird species of birds)

Download directly from dryad - link provided above or contact at first author at samwiry@gmail.com
*********************************************
# Code_*
********************************************
The folders contain code for the analysis

*********************************************
# Appendix S5- species list
********************************************
A list of all 63 bird species (from 32 families) that were included in our hierarchical community distance sampling model.

# Link to manuscript
https://royalsocietypublishing.org/doi/pdf/10.1098/rspb.2023.0467

