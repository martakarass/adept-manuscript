# adept-manuscript

# Directory guide 

## `data`

## `figures` 

Contains PNG files of the manuscript figures. 

## `R`

- `estimate-empirical-pattern.R` - Contains code to estimate stride pattern templates based on a data set of of manually pre-segmented strides. The code was used to generate `stride_template` object attached to the `adeptdata` package. 

- `figure-adept-concept.R` - Contains code to generate manuscript figures: 
    - Figure 2: visualization of the translation and scaling operations on the data. 
    - Figure 4: acceleration time series for two subsequent strides, where the beginning of a stride is marked. 
    
- `figure-empirical-pattern-estimation.R` - Contains code to generate manuscript figures: 
    - Figure 5a: 200 manually segmented strides from right ankle. 
    - Figure 5b: 200 manually segmented strides from right ankle after linear interpolation and normalization to have meanzero and variance one.
    - Figure 6a: 642 normalized strides clustered into two groups based on their correlation similarity. 
    - Figure 6b: 642 normalized strides clustered into three groups based on their correlation similarity. <br/><br/>

- `figure-raw-data-xyz-v.R` - Contains code to generate manuscript figures: 
    - Figure 1a: 3-dimmensional acceleration time series from 5 seconds of walking for two different study participants. 
    - Figure 1b: Same as above, but showing 1-dimmensional vector magnitude. <br/><br/>
    
- `util.R` - Collection of utility `R` functions. 

