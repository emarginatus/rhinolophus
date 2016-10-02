#' Get validation summary for raw data
#' @param path the path of the _truth.rds and _parameter.rds files
#' @export
#' @importFrom  dplyr %>% filter_ inner_join group_by_ mutate_ n arrange_ ungroup transmute_
#' @importFrom kohonen xyf map
#' @importFrom class somgrid
validate_truth <- function(path){
  truth <- paste0(path, "/_truth.rds") %>%
    normalizePath() %>%
    readRDS()
  parameter <- paste0(path, "/_parameter.rds") %>%
    normalizePath() %>%
    readRDS()

  dataset <- truth %>%
    filter_(~!is.na(Species)) %>%
    inner_join(
      parameter,
      by = "Fingerprint"
    ) %>%
    group_by_(~Species, ~Type) %>%
    mutate_(
      Combination = ~ifelse(
        n() < 10,
        "other::other",
        sprintf("%s::%s", Species, Type)
      )
    ) %>%
    filter_(~Combination != "other::other") %>%
    arrange_(~Species, ~Type, ~File, ~Fingerprint) %>%
    mutate_(Set = ~row_number(Fingerprint) %% 10)
  Y <- model.matrix(~ 0 + Combination, data = dataset)
  n.dim <- ceiling(nrow(dataset) ^ (1/4))
  usable <- c(
     4,  6, 10, 14, 18,  22,  26,  30,  34,  38,
    42, 46, 50, 54, 58,  62,  66,  70,  78,  78,
    82, 86, 90, 94, 98, 102, 106, 110, 114, 118
  ) %>%
    cumsum() %>%
    '<='(nrow(dataset) * 0.09) %>%
    sum() %>%
    seq_len() %>%
    sprintf(fmt = "%02i")
  relevant <- gsub("^(Re|Im)([[:digit:]]{2})_.*", "\\2", colnames(dataset)) %in%
    c("DeltaAmplitude", "S", "BYmin", usable)
  dataset_matrix <- as.matrix(dataset[, relevant])
  dataset_matrix[is.na(dataset_matrix)] <- 0
  models <- lapply(
    0:9,
    function(i){
      xyf(
        dataset_matrix[dataset$Set != i, ],
        Y[dataset$Set != i, ],
        somgrid(n.dim, n.dim, "hexagonal")
      )
    }
  )
  misclassification <- sapply(
    0:9,
    function(i){
      mapped <- map(models[[i + 1]], newdata = dataset_matrix)$unit.classif
      models[[i + 1]]$codes$Y[mapped, ] %>%
        '-'(Y) %>%
        pmax(0) %>%
        rowSums()
    }
  ) %>%
    rowMeans() %>%
    round(4)

  confusion.matrix <- lapply(
    0:9,
    function(i){
      mapped <- map(models[[i + 1]], newdata = dataset_matrix)$unit.classif
      crossprod(Y, models[[i + 1]]$codes$Y[mapped, ])
    }
  ) %>%
    Reduce(f = "+") %>%
    round(2)
  colnames(confusion.matrix) <- gsub("^Combination", "", colnames(confusion.matrix))
  rownames(confusion.matrix) <- gsub("^Combination", "", rownames(confusion.matrix))
  result <- dataset %>%
    ungroup() %>%
    transmute_(
      ~Fingerprint,
      ~Species,
      ~Type,
      Misclassification = ~misclassification
    ) %>%
    inner_join(
      truth %>%
        select_(~File, ~Fingerprint),
      by = "Fingerprint"
    )
  attr(result, "confusion.matrix") <- confusion.matrix
  return(result)
}
