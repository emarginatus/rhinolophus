#' Extract the coefficients of the pattern from a batPulse object
#' @param object a batPulse object
#' @param n.part the number of Fourier dimensions
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr %>% mutate_ select_ rowwise do_ inner_join semi_join filter_
extract_parameter <- function(object, n.level = 5){
  assert_that(inherits(object, "batPulse"))
  assert_that(is.count(n.level))

  location <- calculate_box(object) %>%
    filter_(~log2(S) >= n.level) %>%
    transmute_(
      Spectrogram = ~levels(Spectrogram)[Spectrogram],
      ~Fingerprint,
      ~MaxAmplitude,
      ~DeltaAmplitude,
      Level = ~log2(S),
      ~BYmin
    )

  if (nrow(location) == 0) {
    return(list(location = location, parameters = NULL))
  }

  params <- object@PulseMetadata %>%
    semi_join(location, by = "Fingerprint") %>%
    rowwise() %>%
    do_(
      Fingerprint = ~.$Fingerprint,
      Parameter = ~object@PulseMatrix[[.$Fingerprint]] %>%
        reduce_matrix()
    ) %>%
    filter_(
      ~length(Parameter) >= n.level
    )
  cname <- params$Fingerprint
  params <- params$Parameter %>%
    lapply(
      function(x){
        x[seq_len(n.level)]
      }
    ) %>%
    lapply(do.call, what = "rbind") %>%
    do.call(what = "cbind")
  colnames(params) <- cname

  list(location = location, parameters = params)
}

#' Reduce matrix
#' @param base.matrix A matrix
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>%
#' @importFrom Matrix Matrix
reduce_matrix <- function(base.matrix){
  assert_that(nrow(base.matrix) == ncol(base.matrix))
  assert_that(is_power_2(nrow(base.matrix)))

  selection <- seq_len(nrow(base.matrix) / 2) * 2
  reduced <- base.matrix[selection - 1, selection - 1] +
    base.matrix[selection, selection - 1] +
    base.matrix[selection - 1, selection] +
    base.matrix[selection, selection]
  if (length(selection) == 1) {
    current <- list(
      reduced / 4 %>%
        Matrix(ncol = 1)
    )
  } else {
    current <- reduce_matrix(reduced / 4)
  }
  current %>%
    c(
      rbind(
        base.matrix[selection - 1, selection] %>%
          as.vector() %>%
          Matrix(ncol = 1),
        base.matrix[selection, selection - 1] %>%
          as.vector() %>%
          Matrix(ncol = 1),
        base.matrix[selection, selection] %>%
          as.vector() %>%
          Matrix(ncol = 1)
      ) %>%
        list()
    )
}
