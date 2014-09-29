#' Calculate the next power of 2
#' @param x the number of which to calculate the next power of 2
#' @export
next.power.2 <- function(x){
  2 ^ ceiling(log2(x))
}
