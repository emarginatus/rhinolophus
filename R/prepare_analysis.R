#' Prepare all pulse parameters for analysis
#' @param path the path of the pulses
#' @inheritParams extract_parameter
#' @export
#' @importFrom dplyr %>% bind_rows
prepare_analysis <- function(path, n.level = 5){
  assert_that(is.string(path))
  assert_that(is.count(n.level))

  params <- list.files(
    path = path,
    pattern = "\\.wav$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    gsub(pattern = "\\.wav$", replacement = ".rds", ignore.case = TRUE) %>%
    lapply(
      function(filename){
        message(filename)
        readRDS(file = filename) %>%
          extract_parameter(n.level = n.level)
      }
    )
  list(
    pca = params %>%
      lapply(
        function(x){
          x$parameters
        }
      ) %>%
      do.call(what = cbind) %>%
      as.matrix() %>%
      t() %>%
      prcomp(center = FALSE),
    meta = params %>%
      lapply(
        function(x){
          x$location
        }
      )  %>%
      bind_rows() %>%
      mutate_(
        Spectrogram = ~factor(Spectrogram)
      )
  ) %>%
    saveRDS(file = sprintf("%s/_parameter_%02i.rds", path, n.level))
}
