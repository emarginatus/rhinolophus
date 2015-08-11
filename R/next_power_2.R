#' Calculate the next power of 2
#' @param x the number of which to calculate the next power of 2
#' @export
next_power_2 <- function(x){
  if (!inherits(x, c("numeric", "integer"))) {
    stop("x is not numeric")
  }
  if (any(x <= 1)) {
    stop("x must be > 1")
  }
  2 ^ ceiling(log2(x))
}
