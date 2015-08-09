#' Get predicted probabilities of class membership based on an ensemble of Kohonen self organizing maps
#' @export
#' @param truth the known classification
#' @param pattern the patterns of the known classification
#' @param newdata the pattern to get predictions for
#' @param n.ensemble Number of kohonen SOM's to use
#' @import kohonen
ensemble.predict <- function(truth, pattern, newdata, n.ensemble = 10){
  predictions <- lapply(seq_len(n.ensemble), function(i){
    model <- trainrda(pattern, truth)
    predict(model, newdata = newdata)$prediction
  })
  predictions <- Reduce("+", predictions) / n.ensemble
  predictions <- cbind(
    predictions,
    "other:" = 1 - rowSums(predictions)
  )
  list(
    predictions = predictions,
    weights = trainrda(pattern, truth)$weights
  )
}
