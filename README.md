# adept-manuscript

# Directory guide 

## `data`

## `figures` 

Contains PNG files of the manusript figures. 

## `R`

- `derive-stride-template.R` - Contains code to estimate stride pattern templates based on a data set of of manually pre-segmented strides. The code was used to generate `stride_template` object attached to the `adeptdata` package. 

- `figure-raw-data-xyz-v.R` - Contains code to generate manusript figures: 
    - Fig. 1: (a): 3-dimmensional acceleration time series from 5 seconds of walking for two differentstudy participants. 
    - Fig. 1: (b): Same as (a), but showing the vector magnitude. <br/><br/>
    
- `theme-publication.R` - Function to create `ggplot2` theme used for generating figures in the manuscript. Utilizes [HanjoStudy](https://github.com/HanjoStudy)'s code available on GitHub at [here](https://github.com/HanjoStudy/quotidieR/blob/master/R/theme_publication.R) (commit 1f9c329 on Jun 25, 2018; accessed on May 30, 2019) under the MIT License, with very minor modifications to the original version. 

