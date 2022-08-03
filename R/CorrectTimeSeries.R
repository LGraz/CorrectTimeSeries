#' A function to get the corrected TimeSeries
#'
#' Uses the trained Random Forest to predict the corrected response
#'
#' @param df a Dataframe with all covariates used in RF
#' @param RF an object of class "randomForest" which predicts the response based on covariates
#' @return A vector with the corrected Response
#' @export
CorrectData <- function(df, RF) {
    randomForest:::predict.randomForest(RF, df)
}
