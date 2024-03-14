# Data
1. **`HCDSM_raw_data.csv `**: input file to fit a hierarchical community distance sampling model for 63 species
   
     + **Variables** 
  * pt: unique identification for each sampling point.
  * family: taxanomic rank of birds observed at each sampling point.
  * spp_id: unique identification for bird species.
  * dist: radial distances (meters) at which observations (birds) were made at sampling points.
  * counts: observed counts (birds) at each sampling point.
  * elev: elevation data at each sampling point.

2. **`Virunga_covariates.csv`**: input file to fit a joint species distribution model for 5 species of warblers (Cisticolidae).
    
     + **Variables**  
  * pt: unique identification for each sampling point.
  * elev: elevation data at each sampling point.
  * family: taxanomic rank of birds observed at sampling points.
  * nha8: unique identification for habitat types recorded at sampling points.
