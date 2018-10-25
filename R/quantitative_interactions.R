#' User function
#' @param d A data frame from \code{\link{prep_data}}. If you want to prepare
#'   your data on your own, use \code{prep_data(..., no_prep = TRUE)}.
#' @param outcome Name of the column to predict. It must match the
#'   outcome provided to \code{\link{prep_data}}.
#' @export
get_interactions <- function(d, outcome) {
  if (missing(d))
    stop("`d` is not provided")
  if (missing(outcome))
    stop("`outomce` is not provided")

  outcome <- rlang::enquo(outcome)
  d_quo <- rlang::enquo(d)
  if (is.null(attr(d, "recipe")))
    stop("`d` doesn't contain a recipe. Prep your data with `prep_data(",
         rlang::quo_name(d_quo), ", ", rlang::quo_name(outcome), ")`")

  ignored_columns <- d %>% attr("recipe") %>% attr("ignored_columns")

  if (!is.null(ignored_columns))
    d <- d %>% select(-ignored_columns)

  formula <- paste0(rlang::quo_name(outcome), " ~ .")

  d[[rlang::quo_name(outcome)]] <-
    ifelse(d[[rlang::quo_name(outcome)]] == "Y", 1, 0)      #TODO: make this tidy

  suppressWarnings(
    m <- gbm::gbm(formula = as.formula(formula), data = d, interaction.depth = 4,
                  n.minobsinnode = 6, distribution = "bernoulli") # TODO: Specify which distribution based on recipe, also, make sure depth and minobsinnode is good.
  )

  important_features <- rownames(summary(m, plotit = FALSE))[ summary(m, plotit = FALSE)$rel.inf > 0.20] # TODO: There has to be a better way to write this.

  interactions <- list(combinations = list(), significance = c())
  interactions <- top_interactions(m, d, map(important_features, ~{.x}),
                                   important_features, interactions, 1)
  interactions <- un_prep_interactions(d, interactions)
  return(interactions)
}

#' Finds the top interactions, this has the possibility of being O(n^n). I have
#' capped it at O(n^5). This might already be too much. This function checks
#' for all interactions through recursion.
#' @param gbm_model a gbm model object. gbm::gbm()
#' @param data
#' @param testing_interactions charaacter vector that specifies past
#'   interactions. Items in this vector should represent column names. Each item
#'   will be considered for further inter
#' @noRd
top_interactions <- function(gbm_model, data, testing_interactions,
                             important_features, interactions, cur_order) {
                                                                                #TODO: check that testing_interactions and important_features are possible?
                                                                                #TODO: add a parameter for caps on interaction depths, could test it with mocking interact.gbm
  if (cur_order < 4) {
    for (i in 1:length(testing_interactions)) {
      interactions <- top_interactions_helper(gbm_model, data,
                                              testing_interactions[[i]],
                                              important_features,
                                              interactions, cur_order + 1)
    }
  }
  return(interactions)
}

#' Helper recursive function for  \code{top_interactions}.
#' @noRd
top_interactions_helper <- function(gbm_model, data, testing_interaction,
                                    important_features, interactions, cur_order) {
  interaction_significance <- NULL
  interaction_combinations <- list()
  interested <- NULL
  for (i in 1:length(important_features)) {
    # get the interaction significance for each combination of variables, i.var
    # finds the h-statistic for interaction significance for all items given
    interaction_combinations[[i]] <- c(testing_interaction,
                                       important_features[i])
    interaction_combinations[[i]] <- interaction_combinations[[i]][
      order(interaction_combinations[[i]])
    ]

    interaction_significance[i] <-
      gbm::interact.gbm(gbm_model, data, i.var = c(interaction_combinations[[i]]))


    # If an interaction is 1 it is most likely the same variable. If it is less
    # than .05 then we don't want to spend time on it.
    interested[i] <-
      interaction_significance[i] > .05 & interaction_significance[i] != 1      # TODO: Since we aren't using tree based, I think every time that a variable is with itself it is 1 or what is was previously. Do a study to make sure.


    # Remove the interaction if it is reflexive
    if (length(interactions$combinations) > 0)
      if (interested[i] & any(map_lgl(interactions$combinations, ~{
        identical(.x, interaction_combinations[[i]])
      })))
        interested[i] <- FALSE

    # If a previous interaction's significance is equal to this interaction's
    # significance, there is no knew information.
    if (important_features[i] %in% testing_interaction & interested[i]) {       # TODO: check reflexive combinations. Don't keep them if they are the same. They shouldn't contain new information. I could easily reorder them and then check for duplicates with unique
      loc_match <- map_lgl(interactions$combinations, ~{
        identical(.x, testing_interaction)
      })
      interested[i] <- !identical(interactions$significance[loc_match],
                                  interaction_significance[i])
    }
  }

  # Reorder combinantions to find matches in the future
  interactions$combinations <- c(interactions$combinations,
                                 interaction_combinations[interested])
  interactions$significance <- c(interactions$significance,
                                 interaction_significance[interested])

  # Only continue recursion if there are no more possible interactions. If there
  # is only one, it might find an interaction with itself when we switch to
  # a tree based function.
  if (sum(interested) > 1) {
    interactions <- top_interactions(gbm_model, data,
                                     interaction_combinations[interested],
                                     important_features[interested],
                                     interactions, cur_order)
  }

  return(interactions)
}

#' set_obs makes sure that cur_observation contains all the columns necessary. It
#' will include any columns not provided
#' @export
set_obs <- function(d, cur_observation) { # TODO: make sure that cur_observation is actually a list. Also make sure that isn't negative
  original_data <- attr(d, "recipe")$template

  if (any(!(names(cur_observation) %in% names(original_data))))
    stop("Columns in cur_observation are not in original_data")

  missing_cols <-
    if (purrr::is_empty(cur_observation)) {
      names(original_data)
    } else {
      cur_observation <- tibble::as.tibble(cur_observation)
      !(names(original_data) %in% names(cur_observation))
    }


  original_data <-
    original_data %>%
    select(names(original_data)[missing_cols]) %>%
    mutate_if(~is.character(.x) || is.factor(.x), Mode) %>%
    mutate_if(is.numeric, ~mean(.x, na.rm = TRUE)) %>%
    slice(1)

  cbind(cur_observation, original_data)
}

#' plots interactions
#' @export
plot_interactions <- function(interactions, model, obs, top_n = 6, print = TRUE,
                              font_size = 11) {                        # make this an S3 class function, maybe plot.interactions? We will need to provide model object..
  #best_interactions <- interactions$combinations[order(interactions$significance,
  #                                                     decreasing = TRUE)]      # might want to limit this[1:10]
                                                                                # TODO: We will also want to create a method that can understand the importance of interactions
  #browser()
  best_interactions <- order_interactions(interactions)
  d <- attr(model, "recipe")$template

  top_n_interactions <- unique(unlist(best_interactions))[1:top_n]
  plot_list <- list()

  pred_outcome <- paste0("predicted_", attr(model, "target"))
  for (i in 1:length(top_n_interactions)) {
    unique_values <- unique(d[[top_n_interactions[i]]])
    vary_list <- NULL
    vary_list[[top_n_interactions[i]]] <- unique_values[order(unique_values)]
    tmp_d <- tibble(obs[[top_n_interactions[i]]], predict(model, obs)[[pred_outcome]])
    names(tmp_d) <- c(top_n_interactions[i], pred_outcome)
    plot_list[[i]] <-
      explore(model, hold = obs, vary_list) %>%
      plot(n_use = 1, print = FALSE, font_size = font_size) +                           # TODO: the hold needs to be the original data before being prepped
      ylim(0, 1) +
      geom_point(data = tmp_d, aes_string(x = top_n_interactions[i], y = pred_outcome))
  }

  p <- cowplot::plot_grid(plotlist = plot_list)#map(plts, ~plot(.x, n_use = 1) + ylim(0, 1.25)))

  if (print)
    print(p)
  return(invisible(p))
}

#'unprep data
#'
un_prep_interactions <- function(d, interactions) {
  if (is.null(attr(d, "recipe")))
    stop("`d` must contain a recipe object")

  if (is.null(get_recipe_step(d, "step_dummy_hcai")))                           #TODO: make get_recipe_step throw error possibly? #Possibly update tidy.step_date_hcai
    stop("`d` must contain a recipe object")

  # Get the dummies
  factor_dummies <- get_recipe_step(d, "step_dummy_hcai")$dummies

  # Get date columns that changed
  original_date_cols <- tidy(get_recipe_step(d, "step_date_hcai"))$terms        #TODO: Can't call terms and dummies on empty objects

  date_dummies <-
    if (!is.null(original_date_cols)) {
      date_dummies <- map(original_date_cols, function(y){
        names(d)[map_lgl(names(d), ~{str_detect(.x, as.character(y))})]
      })
      date_dummies_length <- map_int(date_dummies, ~length(.x))
      tibble(
        feature = rep(original_date_cols, date_dummies_length),
        dummy = unlist(date_dummies),
        ref = NA
      )
    }

  # Combine factor and date dummies
  all_dummies <- rbind(factor_dummies, date_dummies)

  # Replace each dummy interaction with feature name
  tmp_combinations <- map(interactions$combinations, function(x){
    # replace each dummy individually
    x <- map_chr(x, function(y){
      # Search all_dummies to see which feature it is associated with
      loc_dummy_match <- all_dummies$dummy == y
      if (sum(loc_dummy_match) >= 2)
        stop("The interactions provided match multiple dummy columns")
      else if (sum(loc_dummy_match) == 1)
        y <- all_dummies$feature[loc_dummy_match]
      return(y)
    })
    return(x)
  })

  # Reorder all combinations so that they can be easily compared
  ordered_combinations <- map(tmp_combinations, ~.x[order(.x)])

  # find the matches
  mapped_matches <- map(ordered_combinations, function(x) {
    map_lgl(ordered_combinations, ~identical(x, .x))
  })

  # Add the significance with duplicates
  total_significance <- map_dbl(mapped_matches, ~{
    sum(interactions$significance[.x])
  })

  # Remove duplicates
  interactions$significance <- total_significance[!duplicated(ordered_combinations)]
  interactions$combinations <- unique(ordered_combinations)

  return(interactions)
}

#' Orders interactions
#' @interactions
#' @export
order_interactions <- function(interactions) {
  interaction_orders <- map_int(interactions$combinations, length)

  interaction_info <- tibble(
    features = unlist(interactions$combinations),
    significance = rep(interactions$significance, interaction_orders),
    orders = rep(interaction_orders, interaction_orders)
  )

  ordered_interactions <-
    interaction_info %>%
    group_by(features) %>%
    summarise(total_significance = sum(significance), average_order = mean(orders) - 1) %>%
    arrange(desc(average_order), desc(total_significance))

  return(ordered_interactions)
}
