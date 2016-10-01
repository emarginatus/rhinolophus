#' Prepare all pulse parameters for analysis
#' @param path the path of the pulses
#' @inheritParams extract_parameter
#' @export
#' @importFrom dplyr %>% bind_rows
prepare_analysis <- function(path, n.part = 30){
  list.files(
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
        pulses <- readRDS(file = filename)
        extract_parameter(pulses, n.part = n.part)
      }
    ) %>%
    bind_rows() %>%
    saveRDS(paste0(path, "/_parameter.rds"))
}
