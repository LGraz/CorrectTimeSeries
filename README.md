## What is this
This `R` package can be used to correct and interpolate timeseries if additional data is aviable. 

The theory is discribed in the Master Thesis of Lukas Graz which can be found [here](https://github.com/LGraz/MasterThesis-Documentation) and the related codebase [here](https://github.com/LGraz/MasterThesis-Code).

### Example: NDVI Interpolation
The Sentinel 2 satelites make every 5 days an image of a specific location (in our example a Farm). We can use 2 of the 10 spectral bands to calcualte the NDVI. However, due to atmospheric phenomena like clouds certain images might be not trustworthy. Thus we can correct the time series, using the additional information and interpolating, by giving uncertain points less weight. 

## How To Install
`> devtools::install_github("Greeenstone/CorrectTimeSeries")`

## How To Use: Example
```r
library(CorrectTimeSeries)

# load a list of dataframes, each one describes one pixel with the covariates and the response
data(timeseries_list) 
str(timeseries_list[[1]])

# Train/Load  RF
train_model_myself <- TRUE
if (train_model_myself){
    # Add "true" NDVI (or generally the response), by Out-Of-Bag estimation
    timeseries_list <- lapply(timeseries_list, function(df) {
        df$oob_ndvi <- OOB_est(df$gdd, df$ndvi_observed) # gdd is the time-axis
        df
    })
    # Train correction model
    formula <- "oob_ndvi ~ B02+B03+B04+B05+B06+B07+B08+B8A+B11+B12+scl_class"
    RF <- train_RF_with_fromula(formula, timeseries_list, robustify=TRUE)
} else {
    data(RF_for_NDVI)
    RF <- RF_for_NDVI
}

# ADD CORRECTION
timeseries_list <- lapply(timeseries_list, function(df) {
    df$corrected_ndvi <- randomForest:::predict.randomForest(RF, df)
    df
})

# Get interpolation for each timeseries
newx <- 1:1000
lapply(timeseries_list, function(df){
    ss <- smoothing_spline(df$gdd, df$corrected_ndvi)
    predict(ss, newx)$y
})
```
