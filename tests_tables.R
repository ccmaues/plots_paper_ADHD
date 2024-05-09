#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

library(broom)

## For all observations (no time var)
no_duplicate <-
  data %>%
  select(-age, -wave) %>%
  unique() %>%
  select(-IID)
no_duplicate$new_risk <- add_risk(no_duplicate, "adjusted_PRS")
no_duplicate$old_risk <- add_risk(no_duplicate, "original_PRS")

## For time comparison
W0 <-
  data %>%
  filter(wave == "W0") %>%
  select(-W0) %>%
  select(-IID)
W0$new_risk <- add_risk(W0, "adjusted_PRS")
W0$old_risk <- add_risk(W0, "original_PRS")

W1 <-
  data %>%
  filter(wave == "W1") %>%
  select(-W1) %>%
  select(-IID)
W1$new_risk <- add_risk(W1, "adjusted_PRS")
W1$old_risk <- add_risk(W1, "original_PRS")

W2 <-
  data %>%
  filter(wave == "W2") %>%
  select(-W2) %>%
  select(-IID)
W2$new_risk <- add_risk(W2, "adjusted_PRS")
W2$old_risk <- add_risk(W2, "original_PRS")

## Get models
all_time_model <- uni_model_cal(no_duplicate)
W0_model <- uni_model_cal(W0)
W1_model <- uni_model_cal(W1)
W2_model <- uni_model_cal(W2)

## Get Odds Ratio
all_time_OR <- uni_model_OR(no_duplicate)
W0_OR <- uni_model_OR(W0)
W1_OR <- uni_model_OR(W1)
W2_OR <- uni_model_OR(W2)