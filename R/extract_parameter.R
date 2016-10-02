#' Extract the coefficients of the pattern from a batPulse object
#' @param object a batPulse object
#' @param n.part the number of Fourier dimensions
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr %>% mutate_ select_ rowwise do_ inner_join
#' @importFrom tidyr unnest_
extract_parameter <- function(object, n.part = 5){
  assert_that(inherits(object, "batPulse"))
  assert_that(is.count(n.part))

  params <- object@PulseMetadata %>%
    rowwise() %>%
    do_(
      Parameter = ~extract_pulse(
        fingerprint = .$Fingerprint,
        object = object,
        n.part = n.part
      )
    )
  if (nrow(params) == 0) {
    return(NULL)
  }
  params %>%
    unnest_("Parameter") %>%
    inner_join(
      x = calculate_box(object) %>%
        mutate_(Spectrogram = ~levels(Spectrogram)[Spectrogram]) %>%
        select_(
          ~-Xmin, ~-Xmax, ~-Ymin, ~-Ymax, ~-Xpeak, ~-Ypeak, ~-MaxAmplitude,
          ~-BXmin, ~-BXmax, ~-BYmax
        ),
      by = "Fingerprint"
    )
}

#' Extract the Fourier components of a pulse
#' @param fingerprint the fingerprint of the pulse
#' @param object the batPulse object
#' @param n.part the number of Fourier components
#' @importFrom dplyr %>% mutate_ select_ filter_ arrange_
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather_ spread_
#' @export
extract_pulse <- function(fingerprint, object, n.part){
  fft <- object@PulseFourier[[fingerprint]] %>%
    as.data.frame()
  fft %>%
    rownames_to_column(var = "X") %>%
    gather_(key_col = "Y", value_col = "Complex", colnames(fft)) %>%
    mutate_(
      X = ~as.integer(X),
      Y = ~gsub("^V", "", Y) %>%
        as.integer(),
      Re = ~Re(Complex),
      Im = ~Im(Complex),
      Level = ~pmax(X, Y)
    ) %>%
    filter_(~Level <= n.part) %>%
    gather_(
      key_col = "Part",
      value_col = "Size",
      gather_cols = c("Re", "Im"),
      factor_key = TRUE
    ) %>%
    filter_(~X != 1 | Y != 1 | Part != "Im") %>%
    mutate_() %>%
    arrange_(~Level, ~X, ~Y, ~Part) %>%
    mutate_(
      Name = ~sprintf("%s%02i_%02i_%02i", Part, Level, X, Y),
      Name = ~factor(Name, levels = Name),
      Fingerprint = ~fingerprint
    ) %>%
    select_(~Fingerprint, ~Name, ~Size) %>%
    spread_(key_col = "Name", value_col = "Size")
}
