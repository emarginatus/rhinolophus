#' Calculate the next power of 2
#' @param x
#' @export
next.power.2 <- function(x){
  2 ^ ceiling(log2(x))
}
