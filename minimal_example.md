
```r
library(CorrectTimeSeries)
data(timeseries_list) # load NDVI-TS data

# Train RF
  # Add "true" NDVI (or generally the response), by Out-Of-Bag estimation
    timeseries_list <- lapply(timeseries_list, function(df) {
      df$oob_ndvi <- OOB_est(df$gdd, df$ndvi_observed) # gdd is the time-axis
      df})
  # Train correction model
    formula <- "oob_ndvi ~ B02+B03+B04+B05+B06+B07+B08+B8A+B11+B12+scl_class"
    RF <- train_RF_with_fromula(formula, timeseries_list, robustify=TRUE)
# ADD CORRECTION
  timeseries_list <- lapply(timeseries_list, function(df) {
    df$corrected_ndvi <- randomForest:::predict.randomForest(RF, df)
    df})

# Get interpolation for each timeseries
  lapply(timeseries_list, function(df){
    ss <- smoothing_spline(df$gdd, df$corrected_ndvi)
    predict(ss, 1:1000)$y})
```
