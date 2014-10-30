#' select a random pulse
#' @export
#' @param predictions A set of predictions
sample.pulse <- function(predictions){
  selected.type <- sample(names(predictions$weights), 1, prob = predictions$weights)
  sample(nrow(predictions$predictions), 1, prob = predictions$predictions[, selected.type])
}
