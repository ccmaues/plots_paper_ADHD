#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

## For all observations (no time var)
no_duplicate <-
  data %>%
  select(-age, -wave) %>%
  unique() %>%
  mutate(
    new_risk = add_risk(., "adjusted_PRS"),
    old_risk = add_risk(., "original_PRS")) %>%
  select(-IID)

## For time comparison
W0 <-
  data %>%
  filter(wave == "W0") %>%
  select(-wave) %>%
  mutate(
    new_risk = add_risk(., "adjusted_PRS"),
    old_risk = add_risk(., "original_PRS")) %>%
  select(-IID)

W1 <-
  data %>%
  filter(wave == "W1") %>%
  select(-wave) %>%
  mutate(
    new_risk = add_risk(., "adjusted_PRS"),
    old_risk = add_risk(., "original_PRS")) %>%
  select(-IID)

W2 <-
  data %>%
  filter(wave == "W2") %>%
  select(-wave) %>%
  mutate(
    new_risk = add_risk(., "adjusted_PRS"),
    old_risk = add_risk(., "original_PRS")) %>%
  select(-IID)

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

all_time_OR %>%
as_flex_table() %>%
flextable::save_as_docx(fileext = ".docx", path = "all_time_model.docx")

W0_OR %>%
as_flex_table() %>%
flextable::save_as_docx(fileext = ".docx", path = "W0_model.docx")

W1_OR %>%
as_flex_table() %>%
flextable::save_as_docx(fileext = ".docx", path = "W1_model.docx")

W2_OR %>%
as_flex_table() %>%
flextable::save_as_docx(fileext = ".docx", path = "W2_model.docx")
# knitr::kable()