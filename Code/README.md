# R scripts to fit hierarchical community distance sampling models

1.  `Virunga_HCDSM63.R`: Contains code to format data and to fit a
    hierarchical community distance sampling model for 63 bird species
    relating their abundance along an elevation gradient.
2.  `Virunga_warblers_jsdm_elevation.R`: Contains code to format data
    and to fit a hierarchical community distance sampling model to
    estimate residual correlations among five warbler species
    (Cisticolidae) after accounting for the effect of elevation.
3.  `Virunga_warblers_jsdm_null.R`: Contains code to format data and to
    fit a hierarchical community distance sampling model to estimate
    residual correlations among five warbler species (Cisticolidae) -
    Null model.

# Figures, and computation of niche overlap indices

1.  `Detection_probability.R`: Contains code to generate community mean
    detection probabilities for birds in each vegetation type (Appendix
    S7:Figure S1).
2.  `Figures_niche_indices.R`: Contains code to generate figures showing
    results from niche overlap indices, and to compute elevation, diet
    and foraging vertical strata niche overlap indices.
3.  `JSDM_output.R`: Contains code to generate figures (4a,b) showing
    results from the community model extension used to estimate residual
    correlations among species pairs of warblers.

# Body size variation

1.  `Body_size_null_model_code_n_Figure.R`: Contains code to generate
    the null model distribution and figure for body size variation
    (Appendix S3: Figure S2).

# Null model distributions

1.  `Null_model_elev.diet.strata.R`: Contains code to generate niche
    overlap (Pianka) null distributions for a community of birds (63)
    along three niche axis: i) Elevation gradient, ii) Diet, and iii)
    Forest strata.

# Supplementary Material - Appendix S1: Rank abundance plots

1.  `Rank_abundance_plots.R`: Contains code to generate rank abundance
    plots to assess robustness of our single visit point count data
    (i.e., estimating abundance) in relation to a multi-year (i.e., 5
    years) and longer duration point counts (10-min point counts).

# Spatial site index

1.  `Spatial_site_index_detection_probability.R`: Contains code to
    generate the null model distribution to assess whether detection
    probabilities for species within families are similar - Spatial site
    index.
2.  `Spatial_site_index_lantent_abundance.R`: Contains code to estimate
    the Spatial Site index using latent abundance
3.  `Spatial_site_index_raw_counts.R`: Contains code to estimate the
    Spatial Site index using raw counts(Table 1 - main text).
