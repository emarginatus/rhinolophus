#' Calculate the next power of 2
#' @param x the number of which to calculate the next power of 2
#' @export
#' @examples
#' next_power_2(3)
#' next_power_2(12345)
#' next_power_2(c(7, 9))
next_power_2 <- function(x){
  if (!inherits(x, c("numeric", "integer"))) {
    stop("x is not numeric")
  }
  if (any(x <= 1)) {
    stop("x must be > 1")
  }
  2 ^ ceiling(log2(x))
}

#' Test if a number is a power of 2
#' @param x the value to test
#' @export
#' @importFrom assertthat assert_that is.number "on_failure<-"
is_power_2 <- function(x){
  assert_that(is.number(x))
  log2(x) %% 1 < 1e-8
}
on_failure(is_power_2) <- function(call, env){
  paste0(deparse(call$x), " is not a power of 2")
}
