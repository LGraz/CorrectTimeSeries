#' Fit smoothing splines with robustifying
#'
#' @export
smoothing_spline <- function(x, y, robustify = TRUE, w = NULL, ...) {
    ss <- stats::smooth.spline(x, y = y, w = w, ...)
    if (robustify) {
        res <- stats::residuals(ss)
        scaled_res <- res / (stats::median(abs(res)) * 6)
        wnew <- (1 - scaled_res^2)^2 * (abs(scaled_res) < 1)
        if (!is.null(w)) {
            wnew <- w * wnew
        }
        # print(wnew)
        ss <- stats::smooth.spline(x, y = y, w = wnew, ...)
    }
    ss
}

#' perform an out-of-bag estimation using smoothing splines (with robustifying)
#'
#' @export
OOB_est <- function(x, y, robustify = TRUE, w = NULL, ...) {
    stopifnot(length(x) == length(y))
    OOB_estim <- c()
    for (i in seq_along(x)) {
        predicted  <- try({
            ss <- smoothing_spline(x[-i], y[-i], robustify = robustify, w = w[-i], ...)
            predict(ss, x[i])$y
        })
        OOB_estim[i] <- predicted
        # print(OOB_estim[i])
    }
    # print("------------")
    OOB_estim
}

#' train an RandomForest using `formula`
#'
#' @export
train_RF_with_fromula <- function(formula, timeseries_list, robustify = TRUE) {
    stopifnot(is.list(timeseries_list))
    stopifnot(is.data.frame(timeseries_list[[1]]))
    RF <- randomForest::randomForest(as.formula(formula),
        data = {
            RF_training_data <- data.frame()
            lapply(timeseries_list, function(df) {
                # this is SLOW, can be implemented in O(n)
                RF_training_data <<- rbind(RF_training_data, df)
                NULL
            })
            RF_training_data
    })
    rm(RF_training_data) # free memory
    RF
}


#' A function to get the corrected TimeSeries
#'
#' Uses the trained Random Forest to predict the corrected response
#'
#' @param df a Dataframe with all covariates used in RF
#' @param RF an object of class "randomForest" which predicts the response based on covariates
#' @return A vector with the corrected Response
#' @export
CorrectResponse <- function(df, RF) {
    randomForest:::predict.randomForest(RF, df)
}



# # Get example Data-set
# if (FALSE) {
#     pixels <- read.csv("../code/data/yieldmapping_data/yearly_train_test_sets/2019_WW_covariates_test.csv", header = TRUE)
#     pixels$scl_class <- factor(pixels$scl_class, seq(2, 11))
#     pixels$ndvi_observed <- (pixels$B08 - pixels$B04) / (pixels$B08 + pixels$B04)
#     set.seed(4321)
#     pix_ids <- sample(unique(pixels$coord_id), 30)
#     keep_collumns <- c(
#         "B02",
#         "B03",
#         "B04",
#         "B05",
#         "B06",
#         "B07",
#         "B08",
#         "B8A",
#         "B11",
#         "B12",
#         "gdd",
#         "scl_class",
#         "ndvi_observed"
#     )
#     timeseries_list <- vector("list", 0)
#     for (id in pix_ids) {
#         df <- pixels[pixels$coord_id == id, ]
#         timeseries_list[[id]] <- df[keep_collumns]
#     }
#     usethis::use_data(timeseries_list)
# RF_for_NDVI <- readRDS("/home/lukas/Documents/ETH/MASTER_THESIS/code/data/computation_results/ml_models/R_small/ml_randomForest_randomForest_rf_randomForest_ss_noex_rob_rew_1.rds", refhook = function(x) NULL)
# RF_for_NDVI_uncertainty <- readRDS("/home/lukas/Documents/ETH/MASTER_THESIS/code/data/computation_results/ml_models/R_small/ml_randomForest_randomForest_rf_res_randomForest_ss_noex_rob_rew_1.rds", refhook = function(x) NULL)
#     usethis::use_data(RF_for_NDVI)
# usethis::use_data(RF_for_NDVI_uncertainty)
# }


############################################
##  EXAMPLE
############################################
# load a list of dataframes, each one describes one pixel with the covariates and the response
data(timeseries_list)
str(timeseries_list[[1]])

# Train/Load  RF
train_model_myself <- FALSE
if (train_model_myself) {
    # Add "true" NDVI (or generally the response), by Out-Of-Bag estimation
    timeseries_list <- lapply(timeseries_list, function(df) {
        df$oob_ndvi <- OOB_est(df$gdd, df$ndvi_observed) # gdd is the time-axis
        df
    })
    # Train correction model
    formula <- "oob_ndvi ~ B02+B03+B04+B05+B06+B07+B08+B8A+B11+B12+scl_class"
    RF <- train_RF_with_fromula(formula, timeseries_list, robustify = TRUE)
    stop("also need to train model for uncertainty")
} else {
    data(RF_for_NDVI)
    data(RF_for_NDVI_uncertainty)
    RF <- RF_for_NDVI
    RF_uncert <- RF_for_NDVI_uncertainty
}

# ADD CORRECTION
timeseries_list <- lapply(timeseries_list, function(df) {
    df$corrected_ndvi <- randomForest:::predict.randomForest(RF, df)
    df
})

# Get interpolation for each timeseries
newx <- 1:1000
lapply(timeseries_list, function(df) {
    linkfun <- function(x) {
        1 / x
    }
    w <-  linkfun(randomForest:::predict.randomForest(RF_uncert, df))
    ss <- smoothing_spline(df$gdd, df$corrected_ndvi, w = w)
    predict(ss, newx)$y
})
