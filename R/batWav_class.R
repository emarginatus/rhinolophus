#' The batWav class
#' @name batWav-class
#' @rdname batWav-class
#' @exportClass batWav
#' @aliases batWav-class
#' @importFrom methods setClass
#' @docType class
#' @include batMetadata_class.R
setClass(
  "batWav",
  representation = representation(
    Wav = "list"
  ),
  contains = "batMetadata",
  prototype = prototype(
    Wav = list()
  )
)

#' @importFrom methods setValidity
setValidity(
  "batWav",
  function(object){
    assert_that(all(sapply(object@Wav, inherits, what = "integer")))
    assert_that(length(object@Wav) == nrow(object@Metadata))
    assert_that(all(names(object@Wav) %in% object@Metadata$Fingerprint))
    return(TRUE)
  }
)
