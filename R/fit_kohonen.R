#' Get validation summary for raw data
#' @param path the path of the _truth.rds and _parameter.rds files
#' @export
#' @importFrom  dplyr %>% filter_ inner_join group_by_ mutate_ n arrange_ ungroup select_ summarise_ bind_rows sample_frac min_rank
#' @importFrom kohonen xyf map
#' @importFrom class somgrid
#' @importFrom utils head
fit_kohonen <- function(path, anomalies = 10){
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
    mutate_(Set = ~1 + row_number(Fingerprint) %% n.fold) %>%
    ungroup()
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
  crossvalidation <- lapply(
    seq_len(n.fold),
    function(i){
      prediction <- predict_kohonen(
        models = models[i],
        dataset_matrix = dataset_matrix[dataset$Set == i, ]
      )
      confusion <- dataset %>%
        filter_(~Set == i) %>%
        select_(~Fingerprint, ~Species, ~Combination) %>%
        mutate_(
          PredSpecies = ~prediction$Dominant$DominantSpecies,
          PredCombination = ~gsub(
            "^Combination",
            "",
            prediction$Dominant$Category
          ),
          PropSpecies = ~prediction$Dominant$DominantProportion,
          PropCombination = ~prediction$Dominant$Proportion,
          CorrectSpecies = ~ifelse(
            Species == PredSpecies,
            PropSpecies,
            0
          ),
          CorrectCombination = ~ifelse(
            Combination == PredCombination,
            PropCombination,
            0
          ),
          Text = ~prediction$Dominant$Text
        )
      list(
        SpeciesConfusion = confusion %>%
          group_by_(~Species, ~PredSpecies) %>%
          summarise_(
            N = ~n(),
            Proportion = ~sum(PropSpecies)
          ),
        CombinationConfusion = confusion %>%
          group_by_(~Combination, ~PredCombination) %>%
          summarise_(
            N = ~n(),
            Proportion = ~sum(PropCombination)
          ),
        Quality = confusion %>%
          select_(
            ~PredSpecies,
            ~PredCombination,
            ~Fingerprint,
            ~CorrectSpecies,
            ~CorrectCombination,
            ~Text
          )
      )
    }
  )
  cv.species <- lapply(
    crossvalidation,
    function(x){
      x$SpeciesConfusion
    }
  ) %>%
    bind_rows() %>%
    group_by_(~Species, ~PredSpecies) %>%
    summarise_(
      N = ~sum(N),
      Proportion = ~sum(Proportion)
    )
  cv.combination <- lapply(
    crossvalidation,
    function(x){
      x$CombinationConfusion
    }
  ) %>%
    bind_rows() %>%
    group_by_(~Combination, ~PredCombination) %>%
    summarise_(
      N = ~sum(N),
      Proportion = ~sum(Proportion)
    )
  cv.quality <- lapply(
    crossvalidation,
    function(x){
      x$Quality
    }
  ) %>%
    bind_rows() %>%
    inner_join(
      dataset %>%
        select_(~Fingerprint, ~Spectrogram),
      by = "Fingerprint"
    )
  anomaly <- cv.quality %>%
    group_by_(~PredSpecies) %>%
    filter_(~min_rank(CorrectSpecies) <= anomalies) %>%
    sample_frac() %>%
    slice_(~seq_len(anomalies)) %>%
    ungroup() %>%
    select_(~Fingerprint, ~Text, Quality = ~CorrectSpecies) %>%
    bind_rows(
      cv.quality %>%
        group_by_(~PredCombination) %>%
        filter_(~min_rank(CorrectCombination) <= anomalies) %>%
        sample_frac() %>%
        slice_(~seq_len(anomalies)) %>%
        ungroup() %>%
        select_(~Fingerprint, ~Text, Quality = ~CorrectCombination)
    ) %>%
    group_by_(~Fingerprint, ~Text) %>%
    summarise_(Quality = ~min(Quality)) %>%
    arrange_(~Fingerprint)

  attr(models, "anomaly") <- anomaly
  attr(models, "cv_species") <- cv.species
  attr(models, "cv_combination") <- cv.combination
  return(models)
}
