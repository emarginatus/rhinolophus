#' The batMetadata class
#' @name batMetadata-class
#' @rdname batMetadata-class
#' @exportClass batMetadata
#' @aliases batMetadata-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "batMetadata",
  representation = representation(
    Metadata = "data.frame"
  ),
  prototype = prototype(
    Metadata = data.frame(
      Fingerprint = character(0),
      Filename = character(0),
      Channel = factor(character(0), levels = c("left", "right")),
      TimeExpansionFactor = integer(0),
      SamplingRate = numeric(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
setValidity(
  "batMetadata",
  function(object){
    assert_that(has_name(object@Metadata, "Fingerprint"))
    assert_that(has_name(object@Metadata, "Filename"))
    assert_that(has_name(object@Metadata, "Channel"))
    assert_that(has_name(object@Metadata, "TimeExpansionFactor"))
    assert_that(has_name(object@Metadata, "SamplingRate"))

    assert_that(inherits(object@Metadata$Fingerprint, "character"))
    assert_that(inherits(object@Metadata$Filename, "character"))
    assert_that(inherits(object@Metadata$Channel, "factor"))
    assert_that(inherits(object@Metadata$TimeExpansionFactor, "integer"))
    assert_that(inherits(object@Metadata$SamplingRate, "numeric"))

    assert_that(all(nchar(object@Metadata$Fingerprint) == 40L))
    assert_that(all(levels(object@Metadata$Channel) == c("left", "right")))
    return(TRUE)
  }
)
