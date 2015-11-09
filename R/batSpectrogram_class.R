#' The batSpectrogram class
#' @name batSpectrogram-class
#' @rdname batSpectrogram-class
#' @exportClass batSpectrogram
#' @aliases batSpectrogram-class
#' @importFrom methods setClass
#' @docType class
#' @include batMetadata_class.R
setClass(
  "batSpectrogram",
  representation = representation(
    SpectrogramMetadata = "data.frame",
    Spectrogram = "list"
  ),
  contains = "batMetadata",
  prototype = prototype(
    SpectrogramMetadata = data.frame(
      Fingerprint = character(0),
      File = factor(character(0)),
      Window = numeric(0),
      stringsAsFactors = FALSE
    ),
    Spectrogram = list()
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "batSpectrogram",
  function(object){
    assert_that(inherits(object@SpectrogramMetadata$Fingerprint, "character"))
    assert_that(inherits(object@SpectrogramMetadata$File, "factor"))
    assert_that(inherits(object@SpectrogramMetadata$Window, "numeric"))
    assert_that(all(sapply(object@Spectrogram, inherits, what = "specgram")))

    assert_that(anyDuplicated(object@SpectrogramMetadata$Fingerprint) == 0)
    assert_that(anyDuplicated(names(object@Spectrogram)) == 0)
    assert_that(all(nchar(object@SpectrogramMetadata$Fingerprint) == 40))
    assert_that(all(object@SpectrogramMetadata$Window > 0))

    assert_that(
      all(names(object@Spectrogram) %in% object@SpectrogramMetadata$Fingerprint)
    )
    assert_that(
      all(
        levels(object@SpectrogramMetadata$File) %in% object@Metadata$Fingerprint
      )
    )
    return(TRUE)
  }
)
