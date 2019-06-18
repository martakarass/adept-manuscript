# adept-manuscript

# Directory guide 

## `data`

## `data-results`

## `figures` 

Contains PNG files of the manuscript figures. 

## `R`

- `run-application-example.R` - Code to perform stride pattern segmentation from Application example in the manuscript.  Segmentation results are stored at "data-results/application-example-segmentation.csv".

- `data-estimate-empirical-pattern.R` - Code to estimate stride pattern templates based on a data set of of manually pre-segmented strides. The code was used to generate `stride_template` object attached to the `adeptdata` package. 

- `figure-adept-concept.R` - Code to generate manuscript figures: 
    - Figure 2: visualization of the translation and scaling operations on the data. 
    - Figure 4: acceleration time series for two subsequent strides, where the beginning of a stride is marked. <br/><br/>
    
- `figure-algorithm-steps.R` - Code to generate manusript figures: 
    - Figure 7: demonstration of window size effect on smoothing. 
    - Figure 8: visualization of the ADEPT covariance matrix. 
    - Figure 9: visualization of the algorithm tuning step.<br/><br/>
    
- `figure-empirical-pattern-estimation.R` - Code to generate manuscript figures: 
    - Figure 5a: 200 manually segmented strides from right ankle. 
    - Figure 5b: 200 manually segmented strides from right ankle after linear interpolation and normalization to have meanzero and variance one.
    - Figure 6a: 642 normalized strides clustered into two groups based on their correlation similarity. 
    - Figure 6b: 642 normalized strides clustered into three groups based on their correlation similarity. <br/><br/>

- `figure-raw-data-xyz-v.R` - Code to generate manuscript figures: 
    - Figure 1a: 3-dimmensional acceleration time series from 5 seconds of walking for two different study participants. 
    - Figure 1b: Same as above, but showing 1-dimmensional vector magnitude. <br/><br/>
    
- `figure-results-empirical-patterns.R` - Code to generate manuscript figures: 
    - igure 10: Sensor location-specific sets of estimated empirical patterns of a stride. <br/><br/>
    
- `util.R` - Collection of utility `R` functions. 

