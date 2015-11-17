#' Generate labels for the pulse
#' @export
#' @param borders a SpatialPolygons object
#' @importFrom sp bbox
pulse_label <- function(borders){
  assert_that(inherits(borders, "SpatialPolygons"))

  pulse.label <- sapply(
    borders@polygons,
    function(x){
      x@ID
    }
  )
  value <- sapply(
    borders@polygons,
    function(x){
      bbox(x)[, "min"]
    }
  ) * c(1e3, 1e-3)
  names(pulse.label) <- sprintf("%0.f ms - %0.f kHz", value[1, ], value[2,])
  pulse.label[order(value[1, ])]
}
