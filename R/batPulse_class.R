#' The batPulse class
#' @name batPulse-class
#' @rdname batPulse-class
#' @exportClass batPulse
#' @aliases batPulse-class
#' @importFrom methods setClass
#' @docType class
#' @include batSpectrogram_class.R
setClass(
  "batPulse",
  representation = representation(
    PulseMetadata = "data.frame"
  ),
  contains = "batSpectrogram",
  prototype = prototype(
    PulseMetadata = data.frame(
      Fingerprint = character(0),
      Spectrogram = factor(character(0)),
      Xmin = integer(0),
      Xmax = integer(0),
      Ymin = integer(0),
      Ymax = integer(0),
      Ratio = numeric(0),
      AmplitudeMin = numeric(0),
      AmplitudeMax = numeric(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "batPulse",
  function(object){
    assert_that(inherits(object@PulseMetadata$Fingerprint, "character"))
    assert_that(inherits(object@PulseMetadata$Spectrogram, "factor"))
    assert_that(inherits(object@PulseMetadata$Xmin, "integer"))
    assert_that(inherits(object@PulseMetadata$Xmax, "integer"))
    assert_that(inherits(object@PulseMetadata$Ymin, "integer"))
    assert_that(inherits(object@PulseMetadata$Ymax, "integer"))
    assert_that(inherits(object@PulseMetadata$Ratio, "numeric"))
    assert_that(inherits(object@PulseMetadata$AmplitudeMin, "numeric"))
    assert_that(inherits(object@PulseMetadata$AmplitudeMax, "numeric"))

    assert_that(anyDuplicated(object@PulseMetadata$Fingerprint) == 0)
    assert_that(all(nchar(object@PulseMetadata$Fingerprint) == 40))
    assert_that(all(is.finite(object@PulseMetadata$Ratio)))
    assert_that(all(object@PulseMetadata$Ratio > 0))

    assert_that(
      all(
        levels(object@PulseMetadata$Spectrogram) %in%
          object@SpectrogramMetadata$Fingerprint
      )
    )
    return(TRUE)
  }
)
