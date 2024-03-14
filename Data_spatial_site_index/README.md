# Data - spatial site index

1. **`Community.63sp.csv`**: File contains lantent abundance of birds along an elevation gradient – generated from a hierarchical community distance sampling model.

   + **Variables**
      * column 1: sampling points.
      * Subsequent columns- Unique identification for each species (i.e., latent abundance at each sampling points in the study area).

2. **`Raw_counts.csv`**: File contains raw counts to estimate the spatial site index.
    + **Variables**
       * column 1: sampling points.
       * Subsequent columns- Unique identification for each species (i.e., raw counts at each sampling point in the study area).
     

3. **`mean.cv.scale.parameter.csv`**: File contains data on the mean coefficient of variation (null distribution) for the detection scale parameter (σ_js)  for species pairs across families. 
    + **Variables**
      * col[1] - number of samples (each sample = 60 species pairs) used to generate a null model expectation.
      * col[2] - mean coefficient of variation (each sample = 60 species pairs).
     
 4. **`mean.site.indx.samples_latent.csv`**: File contains data to estimate the spatial site index based on mean latent abundance (null distribution). 
    + **Variables**
      * col[1] - number of samples (each sample = 60 species pairs) used to generate a null model expectation.
      * col[2] - mean latent abundance (each sample = 60 species pairs).
