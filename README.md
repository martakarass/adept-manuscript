# adept-manuscript

# Directory guide 

## `data`

Contains CSV files with raw data used in computing the manuscript results (which are not attached to `adeptdata` package). 

## `data-results`

Contains CSV files with precomputed manuscript results. 

## `figures` 

Contains PNG files of the manuscript figures. 

## `figures_eps` 

Contains eps files of the 5 manuscript figures from the main manuscript body.

## `R`

Contains `R` scripts used to generate manuscript results and figures. 

- `figure-adept-concept.R` - Code to generate manuscript figures: 
    - "intro_translation_and_scaling.png" / "Fig2.eps"
    - "intro_stride_notation.png" <br/><br/>
 
- `figure-algorithm-steps.R` - Code to generate manuscript figures: 
    - "methods_smoothing_w_param_effect.png"
    - "methods_adept_similarity_matrix.png"
    - "methods_fine_tuning_part1.png"
    - "methods_fine_tuning_part2.png"<br/><br/>

- `figure-empirical-pattern-estimation.R` - Code to generate manuscript figures: 
    - "methods_manually_segmented_strides.png"
    - "methods_manually_segmented_strides_norm.png"
    - "methods_strides_clustered_2_groups.png"
    - "methods_strides_clustered_3_groups.png"<br/><br/>

- `figure-raw-data-xyz-v.R` - Code to generate manuscript figures: 
  - "intro_3d_1d_acc.png / "Fig1.eps"

- `figure-results-application-example.R` - Code to generate manuscript figures: 
    - "results_estimated_empirical_patterns.png"
    - "results_cadence_estimates_heatmap.png"
    - "results_stride_duration_bubble_plot.png" / "Fig3.eps"
    - "results_individual_strides_3_participants.png" / "Fig5.eps"<br/><br/>

- `figure-suplmat-effect-of-neighborhood-width.R` - Code to generate manuscript figures: 
    - "suplmat_effect_of_neighborhood_width.png"

- `figure-suplmat-effect-of-smoothing.R` - Code to generate manuscript figures: 
    - "suplmat_effect_of_smoothing_A.png"
    - "suplmat_effect_of_smoothing_B.png"
    - "suplmat_effect_of_smoothing_C.png"<br/><br/>

- `figure-suplmat-effect-of-template-number.R` - Code to generate manuscript figures: 
    - "suplmat_effect_of_template_number.png"

- `figure-suplmat-effect-of-tuning.R` - Code to generate manuscript figures: 
    - "suplmat_effect_of_tuning_part1.png"
    - "suplmat_effect_of_tuning_part2_A.png"<br/><br/>

- `figure-validation-consistency-with-manual-segmentation.R` - Code to generate manuscript figures: 
    - "validation_consistency_with_manual_segmentation_all.png"
    - "validation_consistency_with_manual_segmentation_1.png" / "Fig4.eps"<br/><br/>

- `run-application-example.R` 
    - Code to perform stride pattern segmentation from Application example in the manuscript. Results are stored at "data-results/application-example-segmentation.csv". 
    - Code to compute a table of differences between ADEPT-segmented and manually segmented strides. Results are stored at "data-results/application-example-segmentation-vs-manual.csv". <br/><br/>

- `run-suplmat-effect-of-nighborhood-width.R` 
    - Code to perform analysis of the effect of neighborhood width parameter on  ADEPT segmentation. Analysis results are stored at "data-results/suplmat-effect-of-neighborhood-width.csv". <br/><br/>

- `run-suplmat-effect-of-smoothing.R` 
    - Code to perform analysis of the effect of smoothing parameter on ADEPT segmentation. Analysis results are stored at "data-results/suplmat-effect-of-smoothing.csv". <br/><br/>

- `run-suplmat-effect-of-template-number.R` 
    - Code to perform analysis of the effect of a number of stride template functions on ADEPT segmentation. Analysis results are stored at "data-results/suplmat-effect-of-template-number.csv". <br/><br/>

- `run-suplmat-effect-of-tuning.R` 
    - Code to perform analysis of the effect of tuning procedure on ADEPT segmentation. Analysis results are stored at "data-results/suplmat-effect-of-tuning.csv". <br/><br/>

- `run-validation-consistency-across-sensors.R` 
    - Code to perform analysis of the consistency of ADEPT segmentation across sensor location. Analysis results are stored at "data-results/validation-consistency-across-sensors.csv". <br/><br/>

- `run-validation-consistency-with-manual-segmentation.R` 
    - Code to perform analysis of the consistency between ADEPT-based and manual segmentation. Results are stored at "data-results/validation-consistency-with-manual-segmentation.csv". 
    - Code to compute a table of differences between ADEPT-segmented and manually segmented strides. Results are stored at "data-results/validation-consistency-with-manual-matched.csv".<br/><br/>

- `table-results-application-example.R`  - Code to generate manuscript tables: 
    - Table 1: Summary of a number of segmented strides per person out of 32 study participants, grouped by sensor location.
    - Table 2: Summary of stride duration time (in seconds) out of all segmented strides from 32 study participants, grouped by sensor location.<br/><br/>

- `table-validation-consistency-across-sensors.R` - Code to generate manuscript tables: 
    - Table 3: Coefficient of variation (CV), expressed in percentage, of strides estimation for each study participant across four sensor locations. <br/><br/>

- `table-validation-consistency-with-manual-segmentation.R` - Code to generate manuscript tables: 
    - Table 4: The mean and standard deviation of estimated stride start time and end time difference values between manually and automatically segmented strides. ADEPT segmentation results were obtained as described in a validation procedure in Section 6.2 in the Manuscript.
    - Table 5: The mean and standard deviation of estimated stride start time and end time difference values between manually and automatically segmented strides. ADEPT segmentation results were obtained with stride patterns estimated based on all study participants.<br/><br/>

- `util.R` - Collection of utility functions. 