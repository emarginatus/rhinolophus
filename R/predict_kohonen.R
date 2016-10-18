#' Predict the classification according to a set of supervised Kohonen's self-organising maps
#' @param models a list of supervised Kohonen's self-organising maps
#' @param dataset_matrix a matrix with the parameter values of the new observations
#' @export
#' @importFrom kohonen map
#' @importFrom dplyr %>% mutate_ group_by_ summarise_ arrange_ desc slice_ filter_
#' @importFrom tidyr extract_ gather_
predict_kohonen <- function(models, dataset_matrix){
  predictions <- lapply(
    models,
    function(model){
      model$codes$Y[
        map(model, newdata = dataset_matrix)$unit.classif,
      ]
    }
  ) %>%
    Reduce(f = "+") %>%
    '/'(length(models))
  dominant <- data.frame(
    Category = colnames(predictions) %>%
      '['(apply(predictions, 1, which.max)),
    Proportion = apply(predictions, 1, max)
  ) %>%
    extract_(
      col = "Category",
      into = c("Species", "Type"),
      regex = "^Combination([[:alnum:]]+)::([[:alnum:]]+)$",
      remove = FALSE
    )

  predictions.long <- predictions %>%
    as.data.frame() %>%
    mutate_(
      ID = ~row_number()
    ) %>%
    gather_(
      "Category",
      "Proportion",
      colnames(predictions)
    ) %>%
    extract_(
      col = "Category",
      into = c("Species", "Type"),
      regex = "^Combination([[:alnum:]]+)::([[:alnum:]]+)$"
    )
  dominant.species <- predictions.long %>%
    group_by_(~ID, ~Species) %>%
    summarise_(Proportion = ~sum(Proportion)) %>%
    group_by_(~ID) %>%
    arrange_(~desc(Proportion)) %>%
    slice_(1)
  dominant[dominant.species$ID, "DominantSpecies"] <- dominant.species$Species
  dominant[dominant.species$ID, "DominantProportion"] <-
    dominant.species$Proportion

  detail <-  predictions.long %>%
    filter_(~Proportion >= 0.2) %>%
    mutate_(
      Text = ~sprintf("%s %.0f%%", Type, 100 * Proportion)
    ) %>%
    arrange_(~desc(Proportion)) %>%
    group_by_(~ID, ~Species) %>%
    summarise_(
      Proportion = ~sum(Proportion),
      Text = ~sprintf("%s (%s)", Species[1], paste(Text, collapse = ", "))
    ) %>%
    arrange_(~desc(Proportion)) %>%
    group_by_(~ID) %>%
    summarise_(
      Text = ~paste(Text, collapse = ", ")
    )
  dominant$Text[detail$ID] <- detail$Text
  return(
    list(
      Predictions = predictions,
      Dominant = dominant
    )
  )
}
