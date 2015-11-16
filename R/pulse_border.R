#' Calculate the bounding box of a pulse
#' @param pulse a dataframe with a single pulse
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% select_ transmute_ bind_rows arrange_ group_by_ summarise_ rowwise
#' @importFrom sp Polygon Polygons SpatialPolygons
#' @export
pulse_border <- function(pulse){
  assert_that(inherits(pulse, "batPulse"))
  assert_that(length(unique(pulse@PulseMetadata$Spectrogram)) == 1)

  base <- pulse@PulseMetadata %>%
    select_(~Fingerprint, ~Xmin, ~Xmax, ~Ymin, ~Ymax)
  base$Xmin <- pulse@Spectrogram[[1]]$t[base$Xmin]
  base$Xmax <- pulse@Spectrogram[[1]]$t[base$Xmax]
  base$Ymin <- pulse@Spectrogram[[1]]$f[base$Ymin]
  base$Ymax <- pulse@Spectrogram[[1]]$f[base$Ymax]

  poly <- bind_rows(
    base %>%
      transmute_(~Fingerprint, X = ~Xmin, Y = ~Ymin, Order = ~1),
    base %>%
      transmute_(~Fingerprint, X = ~Xmin, Y = ~Ymax, Order = ~2),
    base %>%
      transmute_(~Fingerprint, X = ~Xmax, Y = ~Ymax, Order = ~3),
    base %>%
      transmute_(~Fingerprint, X = ~Xmax, Y = ~Ymin, Order = ~4),
    base %>%
      transmute_(~Fingerprint, X = ~Xmin, Y = ~Ymin, Order = ~5)
  ) %>%
    arrange_(~Fingerprint, ~Order) %>%
    group_by_(~Fingerprint) %>%
    summarise_(poly = ~list(Polygon(coords = cbind(X, Y)))) %>%
    rowwise() %>%
    mutate_(poly = ~list(Polygons(srl = list(poly), ID = Fingerprint)))
  SpatialPolygons(poly$poly)
}
