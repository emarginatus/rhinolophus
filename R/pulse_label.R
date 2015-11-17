#' Generate labels for the pulse
#' @export
#' @param borders a SpatialPolygonsDataFrame object
#' @importFrom sp bbox
pulse_label <- function(borders){
  assert_that(inherits(borders, "SpatialPolygonsDataFrame"))
  assert_that(has_name(borders, "Fingerprint"))
  assert_that(has_name(borders, "Label"))

  pulse.label <- borders@data$Fingerprint
  names(pulse.label) <- borders@data$Label
  pulse.label[order(borders$Xmin, borders$Ymin)]
}
