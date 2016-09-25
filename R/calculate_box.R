#' Calcalute the box around a pulse
#' @param pulses a batPulse object
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% mutate_ select_
calculate_box <- function(pulses){
  assert_that(inherits(pulses, what = "batPulse"))
  resolution <- 1 / c(0.5, 0.5)

  box <- pulses@PulseMetadata %>%
    mutate_(
      S = ~(Xmax - Xmin) * resolution[1],
      S = ~pmin(next_power_2(S), 256),
      SY = ~ ceiling(Ymax * resolution[2] / S) - floor(Ymin * resolution[2] / S)
    )
  while (any(box$SY > 1)) {
    box <- box %>%
      mutate_(
        S = ~ifelse(SY == 1, S, S * 2),
        SY = ~ ceiling(Ymax * resolution[2] / S) - floor(Ymin * resolution[2] / S)
      )
  }
  box %>%
    mutate_(
      BXmin = ~Xpeak - ((Xpeak - Xmin) / (Xmax - Xmin)) * S / resolution[1],
      BXmax = ~BXmin + S / resolution[1],
      BYmin = ~floor(Ymin * resolution[2] / S) * S / resolution[2],
      BYmax = ~ceiling(Ymax * resolution[2] / S) * S / resolution[2]
    ) %>%
    select_(~-SY)
}
