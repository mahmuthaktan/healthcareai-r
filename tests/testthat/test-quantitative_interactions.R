context("testing quantitative interactions")

# Setup ------------------------------------------------------------------------
# d <- nycflights13::flights[1:500,]
# d <- d %>%
#   select(-time_hour)
#
#
# d$carrier <- ifelse(d$carrier %in% c("UA", "B6"), "Y", "N")
# d <- prep_data(d, outcome = carrier, remove_near_zero_variance = TRUE)
# d$carrier <- ifelse(d$carrier == "Y", 1, 0)
# m <- gbm::gbm(carrier ~ ., data = d, interaction.depth = 4, n.minobsinnode = 6)
# important_vars <- rownames(summary(m))[summary(m)$rel.inf > 0.20]
#
# tmp_d <-
#   d %>%
#   mutate_if(is.character, as.factor)

set.seed(500)
pima <- pima_diabetes[1:100,]
prepped_pima <- prep_data(pima, patient_id, outcome = diabetes)
gbm_prepped_pima <-
  prepped_pima %>%
  mutate(diabetes = ifelse(diabetes == "Y", 1, 0))
suppressWarnings(
  m <- gbm::gbm(diabetes ~ ., data = gbm_prepped_pima, interaction.depth = 4,
                n.minobsinnode = 6, distribution = "bernoulli")

)
important_vars <- rownames(summary(m, plotit = FALSE))[ summary(m, plotit = FALSE)$rel.inf > 0.20]

# Test -------------------------------------------------------------------------

test_that("test top_interaction_helper", {
  #Note: not a user function, so assuming correct inputs
  interactions <- list(combinations = list(), significance = c())
  important_vars <- c("plasma_glucose")
  interactions <- top_interactions_helper(m, gbm_prepped_pima, "pregnancies",
                                              important_vars, interactions, 1)
  expect_equal(length(interactions$combinations), 1)
  expect_equal(length(interactions$significance), 1)
})

test_that("test top_interactions finds interactions", {
  #Note: not a user function, so assuming correct inputs
  interactions <- list(combinations = list(), significance = c())
  important_vars <- c("plasma_glucose")
  interactions <- top_interactions(m, gbm_prepped_pima, list("pregnancies", "age"),
                                   important_vars, interactions, 1)
  expect_equal(length(interactions$combinations), 2)
  expect_equal(length(interactions$significance), 2)
})

test_that("test no reflexive replications", {
  #Note: not a user function, so assuming correct inputs
  interactions <- list(combinations = list(), significance = c())
  important_vars <- c("plasma_glucose", "age")
  interactions <- top_interactions(m, gbm_prepped_pima, list("age", "plasma_glucose"),
                                   important_vars, interactions, 1)
  expect_equal(length(interactions$combinations), 1)
  expect_equal(length(interactions$significance), 1)
})

test_that("get_interactions returns informative error", {
  expect_error(get_interactions(pima, diabetes), "prep_data\\(pima, diabetes\\)")
  expect_error(get_interactions(d), "`outomce` is not provided")
  expect_error(get_interactions(), "`d` is not provided")
})

test_that("get_interactions finds interactions", {
  interactions <- get_interactions(prepped_pima, diabetes)
  expect_true(length(interactions$combinations) > 0)
  expect_true(length(interactions$significance) > 0)
})

# test_that("test get_interactions", {
#   interactions <- get_interactions(d, outcome = carrier)
#   m <- flash_models(d, outcome = carrier, models = "xgb")
#   obs <- set_obs(d, list(distance = 3000, air_time = 200, origin = "JFK"))
#   plot_interactions(interactions, m, obs)
#   browser()
# })

#gbm::interact.gbm(m, tmp_d, c("dest_ORD", "flight"))

# d2 <- nycflights13::flights[1:20000,]
# d2$carrier <- ifelse(d2$carrier %in% c("UA", "B6"), 1, 0)
# d2 <- d2 %>%
#   select(-time_hour, -tailnum) %>%
#   mutate_if(is.character, as.factor)
#
# prepped_d2 <- prep_data(d2)
# model2 <- flash_models(prepped_d2, outcome = carrier)
#
#
# best_interactions <- interactions2$combinations[order(interactions2$significance,
#                                                      decreasing = TRUE)]
# top_6 <- unique(unlist(best_interactions))[1:6]

# Setup un_prep_interactions ---------------------------------------------------

# Test un_prep_interactions ----------------------------------------------------
# test_that("un_prep_interactions ", {
#   # test both dummy type variables and recipe
# })
