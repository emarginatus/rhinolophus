#' Calculate the bounding box of a pulse
#' @param pulse a dataframe with a single pulse
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% select_ transmute_ bind_rows arrange_
#' @export
pulse_border <- function(pulse){
  assert_that(inherits(pulse, "data.frame"))
  assert_that(has_name(pulse, "Xmin"))
  assert_that(has_name(pulse, "Xmax"))
  assert_that(has_name(pulse, "Ymin"))
  assert_that(has_name(pulse, "Ymax"))
#   assert_that(has_name(pulse, "DeltaTime"))
#   assert_that(has_name(pulse, "DeltaFrequency"))
  assert_that(has_name(pulse, "Fingerprint"))

  pulse <- pulse %>%
    select_(~Fingerprint, ~Xmin, ~Xmax, ~Ymin, ~Ymax)
  bind_rows(
    pulse %>%
      transmute_(~Fingerprint, X = ~Xmin, Y = ~Ymin, Order = ~1),
    pulse %>%
      transmute_(~Fingerprint, X = ~Xmin, Y = ~Ymax, Order = ~2),
    pulse %>%
      transmute_(~Fingerprint, X = ~Xmax, Y = ~Ymax, Order = ~3),
    pulse %>%
      transmute_(~Fingerprint, X = ~Xmax, Y = ~Ymin, Order = ~4),
    pulse %>%
      transmute_(~Fingerprint, X = ~Xmin, Y = ~Ymin, Order = ~5)
  ) %>%
    arrange_(~Fingerprint, ~Order)
}
