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
    PulseMetadata = "data.frame",
    PulseFourier = "list",
    PulseVoxel = "data.frame"
  ),
  contains = "batSpectrogram",
  prototype = prototype(
    PulseMetadata = data.frame(
      Fingerprint = character(0),
      Spectrogram = factor(character(0)),
      Xmin = numeric(0),
      Xmax = numeric(0),
      Ymin = numeric(0),
      Ymax = numeric(0),
      MaxAmplitude = numeric(0),
      DeltaAmplitude = numeric(0),
      stringsAsFactors = FALSE
    ),
    PulseFourier = list(),
    PulseVoxel = data.frame(
      Fingerprint = factor(character(0)),
      x = integer(0),
      y = integer(0),
      dB = integer(0),
      n = integer(0)
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
    assert_that(inherits(object@PulseMetadata$Xmin, "numeric"))
    assert_that(inherits(object@PulseMetadata$Xmax, "numeric"))
    assert_that(inherits(object@PulseMetadata$Ymin, "numeric"))
    assert_that(inherits(object@PulseMetadata$Ymax, "numeric"))
    assert_that(inherits(object@PulseMetadata$MaxAmplitude, "numeric"))
    assert_that(inherits(object@PulseMetadata$DeltaAmplitude, "numeric"))

    assert_that(anyDuplicated(object@PulseMetadata$Fingerprint) == 0)
    assert_that(anyDuplicated(names(object@PulseFourier)) == 0)

    assert_that(all(nchar(object@PulseMetadata$Fingerprint) == 40))
    assert_that(all(object@PulseMetadata$DeltaAmplitude > 0))

    assert_that(
      all(
        levels(object@PulseMetadata$Spectrogram) %in%
          object@SpectrogramMetadata$Fingerprint
      )
    )
    assert_that(
      all(
        names(object@PulseFourier$Spectrogram) %in%
          object@PulseMetadata$Fingerprint
      )
    )
    assert_that(
      all(
        levels(object@PulseVoxel$Fingerprint) %in%
          object@PulseMetadata$Fingerprint
      )
    )
    return(TRUE)
  }
)
