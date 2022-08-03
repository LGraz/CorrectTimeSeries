


```
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
