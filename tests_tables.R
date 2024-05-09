#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")

library(broom)

# tenho que mudar esse aqui pq
# do modo que tá, ele tá fazendo multivariada com wave e sexo
for_glm <-
  data %>%
  mutate(diagnosis = case_when(
    diagnosis == 2 ~ 1,
    .default = 0
  ),
  risk_gp_new = ntile(adjusted_PRS, 3),
  risk_gp_new = as.factor(case_when(
    risk_gp_new == 1 ~ "low",
    risk_gp_new == 2 ~ "medium",
    risk_gp_new == 3 ~ "high")),
  risk_gp_old = ntile(original_PRS, 3),
  risk_gp_old = as.factor(case_when(
    risk_gp_old == 1 ~ "low",
    risk_gp_old == 2 ~ "medium",
    risk_gp_old == 3 ~ "high"))) %>%
  inner_join(., data_pt, by = "IID") %>%
  select(-IID)

## Looping to get multiple univariated models
library(purrr)
uni_models <-
  colnames(for_glm)[!colnames(for_glm) == "diagnosis"] %>%
  paste("diagnosis ~ ", .) %>%
  map(.f = ~glm(formula = as.formula(.x),
    family = "binomial", data = for_glm)) %>%
  map(.f = ~tidy(.x, exponentiate = TRUE, conf.int = TRUE)) %>%
  bind_rows() %>%
  mutate(
    across(where(is.numeric), round, digits = 3),
    p.value = format(p.value, scientific = TRUE))
View(uni_models)
colnames(for_glm)[!colnames(for_glm) == "diagnosis"] %>%

# gostei
library(gtsummary)

uni_tab <-
for_glm %>%
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = diagnosis,                          ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

uni_tab