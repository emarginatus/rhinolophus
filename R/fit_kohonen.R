#' Get validation summary for raw data
#' @param path the path of the _truth.rds and _parameter.rds files
#' @export
#' @importFrom  dplyr %>% filter_ inner_join group_by_ mutate_ n arrange_ ungroup transmute_
#' @importFrom kohonen xyf map
#' @importFrom class somgrid
fit_kohonen <- function(path){
  n.fold <- 10
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
    mutate_(Set = ~1 + row_number(Fingerprint) %% n.fold)
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
    seq_len(n.fold),
    function(i){
      xyf(
        dataset_matrix[dataset$Set != i, ],
        Y[dataset$Set != i, ],
        somgrid(n.dim, n.dim, "hexagonal")
      )
    }
  )
  prediction <- lapply(
    models,
    function(model){
      model$codes$Y[
        map(model, newdata = dataset_matrix)$unit.classif,
      ]
    }
  )
  misclassification <- sapply(
    prediction,
    function(i){
      i %>%
        '-'(Y) %>%
        pmax(0) %>%
        rowSums()
    }
  ) %>%
    rowMeans() %>%
    round(4)
  confusion.matrix <- lapply(
    prediction,
    function(i){
      crossprod(Y, i)
    }
  ) %>%
    Reduce(f = "+") %>%
    round(2)

  to.check <- misclassification %>%
    order(decreasing = TRUE) %>%
    head(100)
  prediction <- lapply(
      prediction,
      function(i) {
        i[to.check, ]
      }
    ) %>%
      Reduce(f = '+') %>%
      '/'(n.fold)
  classification <- colnames(prediction) %>%
    gsub(pattern = "Combination(.*)::(.*)", replacement = "\\1 - \\2") %>%
    '['(apply(prediction, 1, which.max)) %>%
    sprintf(
      fmt = "%s (%2.1f%%)",
      apply(prediction, 1, max) %>%
        '*'(100)
    )

  anomaly <- dataset[to.check, ] %>%
    ungroup() %>%
    transmute_(
      ~Fingerprint,
      ~Species,
      ~Type,
      Classification = ~classification,
      Misclassification = ~misclassification[to.check]
    ) %>%
    inner_join(
      truth %>%
        select_(~File, ~Fingerprint),
      by = "Fingerprint"
    )
  attr(models, "anomaly") <- anomaly
  attr(models, "confusion.matrix") <- confusion.matrix
  return(models)
}
