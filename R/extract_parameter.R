#' Extract the coefficients of the pattern from a batPulse object
#' @param object a batPulse object
#' @param n.part the number of Fourier dimensions
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr %>% add_rownames mutate_ left_join transmute_ inner_join select_
#' @importFrom tidyr gather_
extract_pattern <- function(object, n.part = 5){
  assert_that(inherits(object, "batPulse"))
  assert_that(is.count(n.part))

  complete <- expand.grid(
    X = seq_len(n.part),
    Y = seq_len(n.part)
  )
  fft <- t(sapply(
    names(object@PulseFourier),
    function(fingerprint){
      this.dims <- pmin(n.part, dim(object@PulseFourier[[fingerprint]]))
      long <- object@PulseFourier[[
        fingerprint
      ]][
        seq_len(this.dims[1]),
        seq_len(this.dims[2])
      ] %>%
        as.data.frame() %>%
        add_rownames("X") %>%
        gather_("Y", "Complex", sprintf("V%i", seq_len(this.dims[2]))) %>%
        mutate_(X = ~as.integer(X), Y = ~as.integer(gsub("^V", "", Y)))
      long <- complete %>%
        left_join(long, by = c("X", "Y")) %>%
        transmute_(
          NameRe = ~sprintf("Re_%02i_%02i", X, Y),
          NameIm = ~sprintf("Im_%02i_%02i", X, Y),
          Re = ~ifelse(is.na(Complex), 0, Re(Complex)),
          Im = ~ifelse(is.na(Complex), 0, Im(Complex))
        )
      result <- c(long$Re, long$Im)
      names(result) <- c(long$NameRe, long$NameIm)
      return(result)
    }
  ))
  object@PulseMetadata %>%
    transmute_(
      ~Fingerprint,
      ~Ratio,
      ~AmplitudeMin,
      ~AmplitudeMax,
      Pulsduration = ~(Xmax - Xmin) * DeltaTime,
      MinFrequency = ~Xmin * DeltaFrequency,
      DeltaFrequency = ~(Xmax - Xmin) * DeltaFrequency
    ) %>%
    inner_join(
      fft %>%
        as.data.frame() %>%
        add_rownames("Fingerprint"),
      by = "Fingerprint"
    ) %>%
    select_(~-Im_01_01)
}
