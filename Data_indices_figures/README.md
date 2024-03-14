# Data to estimate niche overlap indices

1. **` *_diet.csv `**: (15 files)- files contain preferred diet categories to estimate diet niche overlap indices for bird species pairs within a family.

    + **Variables** 
      * Species    : scientific name of a bird.
      * Diet-Fruit : Estimated percent use of fruits by a bird species in its diet.
      * Diet-Inv   : Estimated percent use of invertebrates by a bird species in its diet.
      * Diet-Nect  : Estimated percent use of nectar by a bird species  in its diet.
      * Diet-PlantO: Estimated percent use of plant materials by a bird species in its diet.
      * Diet-Seed  : Estimated percent use of seeds by a bird species  in its diet.
      * Diet-Vect  : Estimated percent use of reptiles, snakes, amphibians and/or salamanders by a bird species in its diet.
      * Diet-Vend  : Estimated percent use of mammals by a bird species in its diet.
      * Diet-Vunk  : Estimated percent use of vertebrates (i.e., general or unknown) by a bird species in its diet.
    
2. **`*_strata.csv`**: (15 files)- files contain preferred vertical foraging forest strata categoriesto estimate vertical foraging forest strata niche overlap indices for bird species pairs within a family.
    
    + **Variables** 
      * Species             : scientific name of a bird.
      * ForStrat-ground     : Estimated percent foraging on the ground by a bird species in a forest.
      * ForStrat-understory : Estimated percent foraging below 2m by a bird species in a forest.
      * ForStrat-midhigh    : Estimated percent foraging the middle strata by a bird species  in a forest.
      * ForStrat-canopy     : Estimated percent foraging in a forest canopy by a bird species.
      * ForStrat-aerial     : Estimated percent foraging above the forest canopy by a bird species.
     
3. **`Diet_categories_63species.csv `**: (1 file)- Conntains the preferred diet categories (i.e., Diet-Fruit, Diet-Inv, Diet-PlantO, Diet-Vect, Diet-Vunk) for all the 63 bird species in our bird community. Data to generate a null distribution (i.e., niche overlap) along the diet niche dimension for species pairs across families (i.e., community).

    + **Variables**
      * Diet-Fruit   : Estimated percent use of fruits by a bird species in its diet.
      * Diet-Inv     : Estimated percent use of invertebrates by a bird species in its diet.
      * Diet-Nect    : Estimated percent use of nectar by a bird species  in its diet.
      * Diet-PlantO  : Estimated percent use of plant materials by a bird species in its diet.
      * Diet-Seed    : Estimated percent use of seeds by a bird species  in its diet.
      * Diet-Vect    : Estimated percent use of reptiles, snakes, amphibians and/or salamanders by a bird species in its diet.
      * Diet-Vend    : Estimated percent use of mammals by a bird species in its diet.
      * Diet-Vunk    : Estimated percent use of vertebrates (i.e., general or unknown) by a bird species in its diet.
      * Sp_*         : Unique identification for species (63) within the community (Full species names are provided in Appendix S5).
    
  4. **`strata_categories_63species.csv`**: (1 file)- contains preferred vertical foraging forest strata categories for each bird species by family to generate a null distribution (i.e., niche overlap) along the vertical foraging forest strata niche dimension for species pairs across families (i.e., community).
   
     + **Variables**
       * ForStrat-ground     : Estimated percent foraging on the ground by a bird species in a forest.
       * ForStrat-understory : Estimated percent foraging below 2m by a bird species in a forest.
       * ForStrat-midhigh    : Estimated percent foraging the middle strata by a bird species in a forest.
       * ForStrat-canopy     : Estimated percent foraging in a forest canopy by a bird species.
       * ForStrat-aerial     : Estimated percent foraging above the forest canopy by a bird species.
       * Sp_*                : Unique identification for species (63) within the community (Full species names are provided in Appendix S5).
      
  5. **`mean.cv.scale.parameter.csv`**: (1 file)- Contains the mean coefficient of variation (null distribution) values for the detection scale parameter for species pairs across families.
  
      + **Variables**
        
        * col[1] : number of samples (each sample = 60 species pairs) used to generate a null model expectation
        * col[2] : mean coefficient of variation (each sample = 60 species pairs)
       
  6. **`Virunga_covariates.csv`**: (1 file)-  File used as an input to generate expected abundance.

     + **Variables**
       * pt: unique identification for each sampling point
       * elev: elevation data at each sampling point
       * nha8: unique identification for habitat types recorded at sampling points
      

      
