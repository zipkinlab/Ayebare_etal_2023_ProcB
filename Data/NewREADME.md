# Data - modified hierarchical community distance sampling model

1.  **`Cisticolidae_raw_data.csv`**: input file to fit a joint species
    distribution model for 5 species of warblers (Cisticolidae).

    -   **Variables**\

-   pt: unique identification for each sampling point.
-   family: taxanomic rank of birds observed at each sampling point.
-   spp_id: unique identification for bird species.
-   dist: radial distances (meters) at which observations (birds) were
    made at sampling points.
-   counts: observed counts (birds) at each sampling point.
-   elev: elevation data at each sampling point.

2.  **`Virunga_covariates.csv`**: input file to fit a joint species
    distribution model for 5 species of warblers (Cisticolidae).

    -   **Variables**\

-   pt: unique identification for each sampling point.
-   elev: elevation data at each sampling point.
-   family: taxanomic rank of birds observed at sampling points.
-   nha8: unique identification for habitat types recorded at sampling
    points.

# Data - hierarchical community distance sampling model

1.  **`HCDSM_raw_data.csv`**: input file to fit a hierarchical community
    distance sampling model for 63 species

    -   **Variables**

-   pt: unique identification for each sampling point.
-   family: taxanomic rank of birds observed at each sampling point.
-   spp_id: unique identification for bird species.
-   dist: radial distances (meters) at which observations (birds) were
    made at sampling points.
-   counts: observed counts (birds) at each sampling point.
-   elev: elevation data at each sampling point.

2.  **`Virunga_covariates.csv`**: input file to fit a joint species
    distribution model for 5 species of warblers (Cisticolidae).

    -   **Variables**\

-   pt: unique identification for each sampling point.
-   elev: elevation data at each sampling point.
-   family: taxanomic rank of birds observed at sampling points.
-   nha8: unique identification for habitat types recorded at sampling
    points.

# Data to estimate niche overlap indices

1.  **`*_diet.csv`**: (15 files)- files contain preferred diet
    categories to estimate diet niche overlap indices for bird species
    pairs within a family.

    -   **Variables**
        -   Species : scientific name of a bird.
        -   Diet-Fruit : Estimated percent use of fruits by a bird
            species in its diet.
        -   Diet-Inv : Estimated percent use of invertebrates by a bird
            species in its diet.
        -   Diet-Nect : Estimated percent use of nectar by a bird
            species in its diet.
        -   Diet-PlantO: Estimated percent use of plant materials by a
            bird species in its diet.
        -   Diet-Seed : Estimated percent use of seeds by a bird species
            in its diet.
        -   Diet-Vect : Estimated percent use of reptiles, snakes,
            amphibians and/or salamanders by a bird species in its diet.
        -   Diet-Vend : Estimated percent use of mammals by a bird
            species in its diet.
        -   Diet-Vunk : Estimated percent use of vertebrates (i.e.,
            general or unknown) by a bird species in its diet.

2.  **`*_strata.csv`**: (15 files)- files contain preferred vertical
    foraging forest strata categoriesto estimate vertical foraging
    forest strata niche overlap indices for bird species pairs within a
    family.

    -   **Variables**
        -   Species : scientific name of a bird.
        -   ForStrat-ground : Estimated percent foraging on the ground
            by a bird species in a forest.
        -   ForStrat-understory : Estimated percent foraging below 2m by
            a bird species in a forest.
        -   ForStrat-midhigh : Estimated percent foraging the middle
            strata by a bird species in a forest.
        -   ForStrat-canopy : Estimated percent foraging in a forest
            canopy by a bird species.
        -   ForStrat-aerial : Estimated percent foraging above the
            forest canopy by a bird species.

3.  **`Diet_categories_63species.csv`**: (1 file)- Conntains the
    preferred diet categories (i.e., Diet-Fruit, Diet-Inv, Diet-PlantO,
    Diet-Vect, Diet-Vunk) for all the 63 bird species in our bird
    community. Data to generate a null distribution (i.e., niche
    overlap) along the diet niche dimension for species pairs across
    families (i.e., community).

    -   **Variables**
        -   Diet-Fruit : Estimated percent use of fruits by a bird
            species in its diet.
        -   Diet-Inv : Estimated percent use of invertebrates by a bird
            species in its diet.
        -   Diet-Nect : Estimated percent use of nectar by a bird
            species in its diet.
        -   Diet-PlantO : Estimated percent use of plant materials by a
            bird species in its diet.
        -   Diet-Seed : Estimated percent use of seeds by a bird species
            in its diet.
        -   Diet-Vect : Estimated percent use of reptiles, snakes,
            amphibians and/or salamanders by a bird species in its diet.
        -   Diet-Vend : Estimated percent use of mammals by a bird
            species in its diet.
        -   Diet-Vunk : Estimated percent use of vertebrates (i.e.,
            general or unknown) by a bird species in its diet.
        -   Sp\_\* : Unique identification for species (63) within the
            community (Full species names are provided in Appendix S5).

4.  **`strata_categories_63species.csv`**: (1 file)- contains preferred
    vertical foraging forest strata categories for each bird species by
    family to generate a null distribution (i.e., niche overlap) along
    the vertical foraging forest strata niche dimension for species
    pairs across families (i.e., community).

    -   **Variables**
        -   ForStrat-ground : Estimated percent foraging on the ground
            by a bird species in a forest.
        -   ForStrat-understory : Estimated percent foraging below 2m by
            a bird species in a forest.
        -   ForStrat-midhigh : Estimated percent foraging the middle
            strata by a bird species in a forest.
        -   ForStrat-canopy : Estimated percent foraging in a forest
            canopy by a bird species.
        -   ForStrat-aerial : Estimated percent foraging above the
            forest canopy by a bird species.
        -   Sp\_\* : Unique identification for species (63) within the
            community (Full species names are provided in Appendix S5).

5.  **`mean.cv.scale.parameter.csv`**: (1 file)- Contains the mean
    coefficient of variation (null distribution) values for the
    detection scale parameter for species pairs across families.

    -   **Variables**

        -   col\[1\] : number of samples (each sample = 60 species
            pairs) used to generate a null model expectation
        -   col\[2\] : mean coefficient of variation (each sample = 60
            species pairs)

6.  **`Virunga_covariates.csv`**: (1 file)- File used as an input to
    generate expected abundance.

    -   **Variables**
        -   pt: unique identification for each sampling point
        -   elev: elevation data at each sampling point
        -   nha8: unique identification for habitat types recorded at
            sampling points

7.  **`HCDSM_warblers_elev.RData`**: (1 file)- Model results (with
    covariate) for the warbler (Cisticolidae) community model extension
    used to estimate residual correlations among species pairs.

8.  **`HCDSM_warblers_null.RData`**: (1 file)- Model results (null
    model) for the warbler (Cisticolidae) community model extension used
    to estimate residual correlations among species pairs.

9.  **`Species_list.csv`**: (1 file)- A list of all 63 bird species
    (from 32 families) that were included in our hierarchical community
    distance sampling model to estimate abundance variation along an
    elevation gradient in the Virunga volcanoes.

# Data - body mass (63 bird species)

1.  **`Bodymass_63species.csv`**: File contains data used to calculate
    the coefficient of variation for 60 random species pairs from a
    community of 63 species.

    -   **Variables**
        -   BodyMass: describes the average body mass for each bird
            species.
        -   Sp\_\* : Unique identification for species (63) within the
            community (Full species names are provided in Appendix S5).

# Data - null model distributions ( elevation, diet, forest strata)

**Folder contents**: contains 4 files (i.e.,
prop.weak.overlap.samples.elev, prop.strong.overlap.samples.elev,
prop.weak.overlap.samples.d, prop.strong.overlap.samples.d,
prop.weak.overlap.samples.s, prop.strong.overlap.samples.s ).

**Purpose**: Generated to assess whether the observed niche overlap
indices for the 60 species pairs were significantly different from
random null expectations (i.e., across families) for elevation, diet and
foraging vertical strata niche dimensions.

1.  **`prop.weak.overlap.samples.elev.csv`**

    -   **Variables**
        -   col 1: Proportion of weak elevation niche overlap indices
            for 60 species pairs. Note: elevation niche partitioning is
            considered weak when the index is \< 0.5
        -   Sp\_\* : Unique identification for bird species (63) within
            the community (full species names are provided in Appendix
            S5).

2.  **`prop.strong.overlap.samples.elev.csv`**

    -   **Variables**
        -   col 1: Proportion of strong elevation niche overlap indices
            for 60 species pairs. Note: elevation niche partitioning is
            considered strong when the index is ≥ 0.5.
        -   Sp\_\* : Unique identification for bird species (63) within
            the community (full species names are provided in Appendix
            S5).

3.  **`prop.weak.overlap.samples.d.csv`**

    -   **Variables**
        -   col 1: Proportion of weak diet niche overlap indices
            considering 60 species pairs. Note: diet niche partitioning
            is considered weak when index is \< 0.5.\
        -   Sp\_\* : Unique identification for bird species (63) within
            the community (full species names are provided in Appendix
            S5).

4.  **`prop.strong.overlap.samples.d.csv`**

    -   **Variables**
        -   col 1: Proportion of strong diet niche overlap indices
            considering 60 species pairs. Note: diet niche partitioning
            is considered strong when index is \< 0.5.
        -   Sp\_\* : Unique identification for bird species (63) within
            the community (full species names are provided in Appendix
            S5).

5.  **`prop.weak.overlap.samples.s.csv`**

    -   **Variables**
        -   col 1: Proportion of weak vertical foraging forest strata
            niche overlap indices considering 60 species pairs. Note:
            diet niche partitioning is considered weak when index is \<
            0.5.
        -   Sp\_\* : Unique identification for species (63) within the
            community (Full species names are provided in Appendix S5).

6.  **`prop.strong.overlap.samples.s.csv`**

    -   **Variables**
        -   col 1: Proportion of strong vertical foraging forest strata
            niche overlap indices considering 60 species pairs. Note:
            vertical foraging forest strata niche partitioning is
            considered strong when index is \< 0.5
        -   Sp\_\* : Unique identification for species (63) within the
            community (Full species names are provided in Appendix S5).

# Data - rank abundance

1.  **`Rank.sp44.csv`**: Relative abundance per species - derived from
    this study which comes from Parc National des Volcans in Rwanda and
    Derhé et al. 2020 using a multi-year data set collected in Parc
    National des Volcans.

    -   **Variables**

-   Species: scientific name of a bird.
-   This.study: Relative abundance of birds (only Parc National des
    Volcans).
-   Derhe.et.al.2020: Relative abundance of birds derived from Derhé et
    al. 2020.
-   Total.no.spp-5yrs: Relative abundance of birds from Derhé et
    al. 2020 across five years (i.e 10 sampling occasion).

2.  **`Rank.sp51.csv`**: Relative abundance per species - derived from
    this study which comes from three national parks (i.e., Mgahinga
    Gorilla National Park in Uganda, Parc National des Volcans in
    Rwanda, Parc National des Virunga in the Democratic Republic of
    Congo). and Derhé et al. 2020 using a multi-year data set collected
    in Parc National des Volcans.

    -   **Variables**

-   Species: scientific name of a bird.
-   This.study: Relative abundance of birds (only Parc National des
    Volcans).
-   Derhe.et.al.2020: Relative abundance of birds derived from Derhé et
    al. 2020.
-   Total.no.spp-5yrs: Relative abundance of birds from Derhé et
    al. 2020 across five years (i.e 10 sampling occasion).

# Data - spatial site index

1.  **`Community.63sp.csv`**: File contains lantent abundance of birds
    along an elevation gradient -- generated from a hierarchical
    community distance sampling model.

    -   **Variables**
        -   column 1: sampling points.
        -   Subsequent columns- Unique identification for each species
            (i.e., latent abundance at each sampling points in the study
            area).

2.  **`Raw_counts.csv`**: File contains raw counts to estimate the
    spatial site index.

    -   **Variables**
        -   column 1: sampling points.
        -   Subsequent columns- Unique identification for each species
            (i.e., raw counts at each sampling point in the study area).

3.  **`mean.cv.scale.parameter.csv`**: File contains data on the mean
    coefficient of variation (null distribution) for the detection scale
    parameter (σ_js) for species pairs across families.

    -   **Variables**
        -   col\[1\] - number of samples (each sample = 60 species
            pairs) used to generate a null model expectation.
        -   col\[2\] - mean coefficient of variation (each sample = 60
            species pairs).

4.  **`mean.site.indx.samples_latent.csv`**: File contains data to
    estimate the spatial site index based on mean latent abundance (null
    distribution).

    -   **Variables**
        -   col\[1\] - number of samples (each sample = 60 species
            pairs) used to generate a null model expectation.
        -   col\[2\] - mean latent abundance (each sample = 60 species
            pairs).
