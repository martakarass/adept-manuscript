# adept-manuscript

This repository contains code scripts and data needed to reproduce all results (figures, tables) presented in the manuscript **"Adaptive empirical pattern transformation (ADEPT) with application to walking stride segmentation"** (revised version under consideration in *Biostatistics*, 2019). 

The session info output, including versions of all packages used to generate the results, can be found at the end of this README doc. In particular, the results were generated with the use of `R` packages: 

- `adept` version `1.1.2`,
- `adeptdata` version `1.0.1`. 

# Directory guide 

### `data`

Contains CSV files with raw data used in computing some of the manuscript results. 

-  **`strides-df-anonymized.csv`** - Accelerometry vector magnitude data of manually presegmented walking strides. Contains 65,458 observations of 7 variables. 

### `data-results`

Contains CSV files with results obtained in manuscript application example, validation and additional experiments included in the Supplementary Material.

### `figures` 

Contains PNG files of the manuscript figures. 

### `figures_eps` 

Contains EPS files of the 5 manuscript figures from the main manuscript body.

### `R`

Contains `R` scripts used to generate manuscript results (stored in `/data-results`) and figures (stored in `/figures`, except from `Fig1.eps`,...,`Fig5.eps` which are stored in `/figures_eps`). 

- **`figure-adept-concept.R`** - Code to generate manuscript figures: 
    - `intro_translation_and_scaling.png` / `Fig2.eps`
    - `intro_stride_notation.png` <br/><br/>
 
- **`figure-algorithm-steps.R`** - Code to generate manuscript figures: 
    - `methods_smoothing_w_param_effect.png`
    - `methods_adept_similarity_matrix.png`
    - `methods_fine_tuning_part1.png`
    - `methods_fine_tuning_part2.png`<br/><br/>

- **`figure-empirical-pattern-estimation.R`** - Code to generate manuscript figures: 
    - `methods_manually_segmented_strides.png`
    - `methods_manually_segmented_strides_norm.png`
    - `methods_strides_clustered_2_groups.png`
    - `methods_strides_clustered_3_groups.png`<br/><br/>

- **`figure-raw-data-xyz-v.R`** - Code to generate manuscript figures: 
  - `intro_3d_1d_acc.png` / `Fig1.eps` <br/><br/>

- **`figure-results-application-example.R`** - Code to generate manuscript figures: 
    - `results_estimated_empirical_patterns.png`
    - `results_cadence_estimates_heatmap.png`
    - `results_stride_duration_bubble_plot.png` / `Fig3.eps`
    - `results_individual_strides_3_participants.png` / `Fig5.eps`<br/><br/>

- **`figure-suplmat-effect-of-neighborhood-width.R`** - Code to generate manuscript figures: 
    - `suplmat_effect_of_neighborhood_width.png` <br/><br/>

- **`figure-suplmat-effect-of-smoothing.R`** - Code to generate manuscript figures: 
    - `suplmat_effect_of_smoothing_A.png`
    - `suplmat_effect_of_smoothing_B.png`
    - `suplmat_effect_of_smoothing_C.png`<br/><br/>

- **`figure-suplmat-effect-of-template-number.R`** - Code to generate manuscript figures: 
    - `suplmat_effect_of_template_number.png`  <br/><br/>

- **`figure-suplmat-effect-of-tuning.R`** - Code to generate manuscript figures: 
    - `suplmat_effect_of_tuning_part1.png`
    - `suplmat_effect_of_tuning_part2_A.png`<br/><br/>

- **`figure-validation-consistency-with-manual-segmentation.R`** - Code to generate manuscript figures: 
    - `validation_consistency_with_manual_segmentation_all.png`
    - `validation_consistency_with_manual_segmentation_1.png` / `Fig4.eps`<br/><br/>

- **`run-application-example.R`** 
    - Code to perform stride pattern segmentation from application example in the manuscript. Results are stored at `data-results/application-example-segmentation.csv`. 
    - Code to compute a table of differences between ADEPT-segmented and manually segmented strides. Results are stored at `data-results/application-example-segmentation-vs-manual.csv`. <br/><br/>

- **`run-suplmat-effect-of-nighborhood-width.R`**
    - Code to perform analysis of the effect of neighborhood width parameter on  ADEPT segmentation. Analysis results are stored at `data-results/suplmat-effect-of-neighborhood-width.csv`. <br/><br/>

- **`run-suplmat-effect-of-smoothing.R`**
    - Code to perform analysis of the effect of smoothing parameter on ADEPT segmentation. Analysis results are stored at `data-results/suplmat-effect-of-smoothing.csv`. <br/><br/>

- **`run-suplmat-effect-of-template-number.R`**
    - Code to perform analysis of the effect of a number of stride template functions on ADEPT segmentation. Analysis results are stored at `data-results/suplmat-effect-of-template-number.csv`. <br/><br/>

- **`run-suplmat-effect-of-tuning.R`** 
    - Code to perform analysis of the effect of tuning procedure on ADEPT segmentation. Analysis results are stored at `data-results/suplmat-effect-of-tuning.csv`. <br/><br/>

- **`run-validation-consistency-across-sensors.R`**
    - Code to perform analysis of the consistency of ADEPT segmentation across sensor location. Analysis results are stored at `data-results/validation-consistency-across-sensors.csv`. <br/><br/>

- **`run-validation-consistency-with-manual-segmentation.R`**
    - Code to perform analysis of the consistency between ADEPT-based and manual segmentation. Results are stored at `data-results/validation-consistency-with-manual-segmentation.csv`. 
    - Code to compute a table of differences between ADEPT-segmented and manually segmented strides. Results are stored at `data-results/validation-consistency-with-manual-matched.csv`.<br/><br/>

- **`table-results-application-example.R`**  - Code to generate manuscript tables: 
    - Table 1: Summary of a number of segmented strides per person out of 32 study participants, grouped by sensor location.
    - Table 2: Summary of stride duration time (in seconds) out of all segmented strides from 32 study participants, grouped by sensor location.<br/><br/>

- **`table-validation-consistency-across-sensors.R`** - Code to generate manuscript tables: 
    - Table 3: Coefficient of variation (CV), expressed in percentage, of strides estimation for each study participant across four sensor locations. <br/><br/>

- **`table-validation-consistency-with-manual-segmentation.R`** - Code to generate manuscript tables: 
    - Table 4: The mean and standard deviation of estimated stride start time and end time difference values between manually and automatically segmented strides. ADEPT segmentation results were obtained as described in a validation procedure in Section 6.2 in the Manuscript.
    - Table 5: The mean and standard deviation of estimated stride start time and end time difference values between manually and automatically segmented strides. ADEPT segmentation results were obtained with stride patterns estimated based on all study participants.<br/><br/>

- **`util.R`** - Collection of utility functions. 


# Mapping of manuscript figures/tables to repository files

### Manuscript - main text

- Fig. 1 - stored at `figures_eps/Fig1.eps`, generated with code `R/figure-raw-data-xyz-v.R`
- Fig. 2 - stored at `figures_eps/Fig2.eps`, generated with code `R/figure-adept-concept.R`
- Fig. 3 - stored at `figures_eps/Fig3.eps`, generated with code `R/figure-results-application-example.R`
- Fig. 4 - stored at `figures_eps/Fig4.eps`, generated with code `R/figure-validation-consistency-with-manual-segmentation.R`
- Fig. 5 - stored at `figures_eps/Fig5.eps`, generated with code `R/figure-results-application-example.R`

### Supplementary Material - main text

- Fig. 1 - stored at `figures/suplmat_effect_of_template_number.png`, generated with code `R/figure-suplmat-effect-of-template-number.R`
- Fig. 2 - stored at `figures/suplmat_effect_of_smoothing_A.png`, generated with code `R/figure-suplmat-effect-of-smoothing.R`
- Fig. 3 - stored at `figures/suplmat_effect_of_smoothing_B.png`, generated with code `R/figure-suplmat-effect-of-smoothing.R`
- Fig. 4 - stored at `figures/suplmat_effect_of_smoothing_C.png`, generated with code `R/figure-suplmat-effect-of-smoothing.R`
- Fig. 5 - stored at `figures/suplmat_effect_of_tuning_part2_A.png`, generated with code `R/figure-suplmat-effect-of-tuning.R`
- Fig. 6 - stored at `figures/suplmat_effect_of_tuning_part1.png`, generated with code `R/figure-suplmat-effect-of-tuning.R`
- Fig. 7 - stored at `figures/suplmat_effect_of_neighborhood_width.png`, generated with code `R/figure-suplmat-effect-of-neighborhood-width.R`

### Supplementary Material - Appendix A: Figures referenced in the Manuscript

- Fig. 9 - stored at `figures/intro_stride_notation.png`, generated with code `R/figure-adept-concept.R`
- Fig. 10a - stored at `figures/methods_manually_segmented_strides.png`, generated with code `R/figure-empirical-pattern-estimation.R`
- Fig. 10b - stored at `methods_manually_segmented_strides_norm.png`, generated with code `R/figure-empirical-pattern-estimation.R`
- Fig. 11a - stored at `figures/methods_strides_clustered_2_groups.png`, generated with code `R/figure-empirical-pattern-estimation.R`
- Fig. 11b - stored at `methods_strides_clustered_3_groups.png`, generated with code `R/figure-empirical-pattern-estimation.R`
- Fig. 12 - stored at `figures/methods_smoothing_w_param_effect.png`, generated with code `R/figure-algorithm-steps.R`
- Fig. 13 - stored at `figures/methods_adept_similarity_matrix.png`, generated with code `R/figure-algorithm-steps.R`
- Fig. 14a - stored at `figures/methods_fine_tuning_part1.png`, generated with code `R/figure-algorithm-steps.R`
- Fig. 14b - stored at `methods_fine_tuning_part2.png`, generated with code `R/figure-algorithm-steps.R`
- Fig. 15 - stored at `figures/results_estimated_empirical_patterns.png`, generated with code `R/figure-results-application-example.R`
- Fig. 16 - stored at `figures/results_cadence_estimates_heatmap.png`, generated with code `R/figure-results-application-example.R`
- Fig. 17 - stored at `figures/validation_consistency_with_manual_segmentation_all.png`, generated with code `R/figure-validation-consistency-with-manual-segmentation.R`

### Supplementary Material - Appendix B: Tables referenced in the Manuscript

- Table 1 - generated and printed inline with code `R/table-results-application-example.R`
- Table 2 - generated and printed inline with code `R/table-results-application-example.R`
- Table 3 - generated and printed inline with code `R/table-validation-consistency-across-sensors.R`
- Table 4 - generated and printed inline with code `R/table-validation-consistency-with-manual-segmentation.R`
- Table 5 - generated and printed inline with code `R/table-validation-consistency-with-manual-segmentation.R`



# Output of `devtools::session_info()` `R` function

```
> devtools::session_info()
─ Session info ────────────────────────────────────────────────────────────────────────
 setting  value                       
 version  R version 3.5.2 (2018-12-20)
 os       macOS Mojave 10.14.2        
 system   x86_64, darwin15.6.0        
 ui       RStudio                     
 language (EN)                        
 collate  en_US.UTF-8                 
 ctype    en_US.UTF-8                 
 tz       Europe/Zurich               
 date     2019-07-30                  

─ Packages ────────────────────────────────────────────────────────────────────────────
 package          * version  date       lib source        
 adept            * 1.1.2    2019-06-18 [1] CRAN (R 3.5.2)
 adeptdata        * 1.0.1    2019-03-30 [1] CRAN (R 3.5.2)
 assertthat         0.2.1    2019-03-21 [1] CRAN (R 3.5.2)
 backports          1.1.4    2019-04-10 [1] CRAN (R 3.5.2)
 callr              3.2.0    2019-03-15 [1] CRAN (R 3.5.2)
 class              7.3-15   2019-01-01 [1] CRAN (R 3.5.2)
 cli                1.1.0    2019-03-19 [1] CRAN (R 3.5.2)
 cluster          * 2.0.9    2019-05-01 [1] CRAN (R 3.5.2)
 clv                0.3-2.1  2013-11-11 [1] CRAN (R 3.5.0)
 cobs             * 1.3-3    2017-03-31 [1] CRAN (R 3.5.0)
 codetools          0.2-16   2018-12-24 [1] CRAN (R 3.5.2)
 colorspace         1.4-1    2019-03-18 [1] CRAN (R 3.5.2)
 crayon             1.3.4    2017-09-16 [1] CRAN (R 3.5.0)
 crosstalk          1.0.0    2016-12-21 [1] CRAN (R 3.5.0)
 data.table       * 1.12.2   2019-04-07 [1] CRAN (R 3.5.2)
 DBI                1.0.0    2018-05-02 [1] CRAN (R 3.5.0)
 desc               1.2.0    2018-05-01 [1] CRAN (R 3.5.0)
 devtools           2.0.2    2019-04-08 [1] CRAN (R 3.5.2)
 digest             0.6.19   2019-05-20 [1] CRAN (R 3.5.2)
 dplyr            * 0.8.1    2019-05-14 [1] CRAN (R 3.5.2)
 dtw                1.20-1   2018-05-18 [1] CRAN (R 3.5.0)
 dvmisc           * 1.1.3    2019-03-05 [1] CRAN (R 3.5.2)
 fs                 1.3.1    2019-05-06 [1] CRAN (R 3.5.2)
 future             1.13.0   2019-05-08 [1] CRAN (R 3.5.2)
 ggplot2          * 3.2.0    2019-06-16 [1] CRAN (R 3.5.2)
 globals            0.12.4   2018-10-11 [1] CRAN (R 3.5.0)
 glue               1.3.1    2019-03-12 [1] CRAN (R 3.5.2)
 gridExtra        * 2.3      2017-09-09 [1] CRAN (R 3.5.0)
 gtable             0.3.0    2019-03-25 [1] CRAN (R 3.5.2)
 htmltools          0.3.6    2017-04-28 [1] CRAN (R 3.5.0)
 htmlwidgets        1.3      2018-09-30 [1] CRAN (R 3.5.0)
 httpuv             1.5.1    2019-04-05 [1] CRAN (R 3.5.2)
 ifultools          2.0-5    2019-03-04 [1] CRAN (R 3.5.2)
 jsonlite           1.6      2018-12-07 [1] CRAN (R 3.5.0)
 KernSmooth         2.23-15  2015-06-29 [1] CRAN (R 3.5.2)
 knitr              1.23     2019-05-18 [1] CRAN (R 3.5.2)
 later              0.8.0    2019-02-11 [1] CRAN (R 3.5.2)
 latex2exp        * 0.4.0    2015-11-30 [1] CRAN (R 3.5.0)
 lattice            0.20-38  2018-11-04 [1] CRAN (R 3.5.2)
 lazyeval           0.2.2    2019-03-15 [1] CRAN (R 3.5.2)
 listenv            0.7.0    2018-01-21 [1] CRAN (R 3.5.0)
 locpol             0.7-0    2018-05-24 [1] CRAN (R 3.5.0)
 longitudinalData   2.4.1    2016-02-16 [1] CRAN (R 3.5.0)
 magrittr           1.5      2014-11-22 [1] CRAN (R 3.5.0)
 manipulateWidget   0.10.0   2018-06-11 [1] CRAN (R 3.5.0)
 MASS               7.3-51.4 2019-03-31 [1] CRAN (R 3.5.2)
 Matrix             1.2-17   2019-03-22 [1] CRAN (R 3.5.2)
 MatrixModels       0.4-1    2015-08-22 [1] CRAN (R 3.5.0)
 memoise            1.1.0    2017-04-21 [1] CRAN (R 3.5.0)
 mime               0.7      2019-06-11 [1] CRAN (R 3.5.2)
 miniUI             0.1.1.1  2018-05-18 [1] CRAN (R 3.5.0)
 misc3d             0.8-4    2013-01-25 [1] CRAN (R 3.5.0)
 mitools            2.4      2019-04-26 [1] CRAN (R 3.5.2)
 munsell            0.5.0    2018-06-12 [1] CRAN (R 3.5.0)
 mvtnorm            1.0-10   2019-03-05 [1] CRAN (R 3.5.2)
 pdc              * 1.0.3    2015-09-28 [1] CRAN (R 3.5.0)
 pillar             1.4.1    2019-05-28 [1] CRAN (R 3.5.2)
 pkgbuild           1.0.3    2019-03-20 [1] CRAN (R 3.5.2)
 pkgconfig          2.0.2    2018-08-16 [1] CRAN (R 3.5.0)
 pkgload            1.0.2    2018-10-29 [1] CRAN (R 3.5.0)
 plyr               1.8.4    2016-06-08 [1] CRAN (R 3.5.0)
 prettyunits        1.0.2    2015-07-13 [1] CRAN (R 3.5.0)
 processx           3.3.1    2019-05-08 [1] CRAN (R 3.5.2)
 promises           1.0.1    2018-04-13 [1] CRAN (R 3.5.0)
 proxy              0.4-23   2019-03-05 [1] CRAN (R 3.5.2)
 ps                 1.3.0    2018-12-21 [1] CRAN (R 3.5.0)
 purrr              0.3.2    2019-03-15 [1] CRAN (R 3.5.2)
 quantreg           5.40     2019-06-03 [1] CRAN (R 3.5.2)
 R6                 2.4.0    2019-02-14 [1] CRAN (R 3.5.2)
 rbenchmark       * 1.0.0    2012-08-30 [1] CRAN (R 3.5.0)
 Rcpp               1.0.1    2019-03-17 [1] CRAN (R 3.5.2)
 remotes            2.0.4    2019-04-10 [1] CRAN (R 3.5.2)
 reshape2         * 1.4.3    2017-12-11 [1] CRAN (R 3.5.0)
 rgl                0.100.19 2019-03-12 [1] CRAN (R 3.5.2)
 rlang              0.3.4    2019-04-07 [1] CRAN (R 3.5.2)
 rprojroot          1.3-2    2018-01-03 [1] CRAN (R 3.5.0)
 rstudioapi         0.10     2019-03-19 [1] CRAN (R 3.5.2)
 runstats         * 1.0.1    2019-03-13 [1] CRAN (R 3.5.2)
 scales           * 1.0.0    2018-08-09 [1] CRAN (R 3.5.0)
 sessioninfo        1.1.1    2018-11-05 [1] CRAN (R 3.5.0)
 shiny              1.3.2    2019-04-22 [1] CRAN (R 3.5.2)
 SparseM            1.77     2017-04-23 [1] CRAN (R 3.5.0)
 splus2R            1.2-2    2016-09-02 [1] CRAN (R 3.5.0)
 stargazer        * 5.2.2    2018-05-30 [1] CRAN (R 3.5.0)
 stringi            1.4.3    2019-03-12 [1] CRAN (R 3.5.2)
 stringr            1.4.0    2019-02-10 [1] CRAN (R 3.5.2)
 survey             3.36     2019-04-27 [1] CRAN (R 3.5.2)
 survival           2.44-1.1 2019-04-01 [1] CRAN (R 3.5.2)
 testthat         * 2.1.1    2019-04-23 [1] CRAN (R 3.5.2)
 tibble             2.1.3    2019-06-06 [1] CRAN (R 3.5.2)
 tidyr            * 0.8.3    2019-03-01 [1] CRAN (R 3.5.2)
 tidyselect         0.2.5    2018-10-11 [1] CRAN (R 3.5.0)
 TSclust          * 1.2.4    2017-10-16 [1] CRAN (R 3.5.0)
 usethis            1.5.0    2019-04-07 [1] CRAN (R 3.5.2)
 webshot            0.5.1    2018-09-28 [1] CRAN (R 3.5.0)
 withr              2.1.2    2018-03-15 [1] CRAN (R 3.5.0)
 wmtsa            * 2.0-3    2017-12-06 [1] CRAN (R 3.5.0)
 xfun               0.7      2019-05-14 [1] CRAN (R 3.5.2)
 xtable             1.8-4    2019-04-21 [1] CRAN (R 3.5.2)
 yaml               2.2.0    2018-07-25 [1] CRAN (R 3.5.0)

[1] /Library/Frameworks/R.framework/Versions/3.5/Resources/library
```