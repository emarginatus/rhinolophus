#' Generate a colour ramp true a midpoint
#' @export
#' @param x a RasterLayer object
#' @param mid.point the midpoint of the colour ramp
#' @param n the approximate number of breaks
#' @importFrom assertthat assert_that is.number is.count
#' @importFrom grDevices colorRampPalette
#' @importFrom utils head tail
midpoint_ramp <- function(x, mid.point = c(10, 20), n = 20){
  assert_that(inherits(x, "RasterLayer"))
  assert_that(is.number(mid.point))
  assert_that(is.count(n))

  delta <- max(
    mid.point - x@data@min,
    x@data@max - mid.point
  )
  breaks <- pretty(c(0, delta), n = n / 2)
  positive <- colorRampPalette(c("white", "blue"))(length(breaks) - 1)
  negative <- head(colorRampPalette(c("red", "white"))(length(breaks)), -1)
  breaks <- c(
    rev(-tail(breaks, -1)) + mid.point,
    breaks + mid.point
  )
  colours <- c(negative, positive)
  too.small <- sum(breaks < x@data@min)
  if (too.small > 1) {
    breaks <- tail(breaks, -too.small + 1)
    colours <- tail(colours, -too.small + 1)
  }
  too.large <- sum(breaks > x@data@max)
  if (too.large > 1) {
    breaks <- head(breaks, -too.large + 1)
    colours <- head(colours, -too.large + 1)
  }
  return(
    list(
      Breaks = breaks,
      Colour = colours
    )
  )
}
