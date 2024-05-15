#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")


# summary(fit.ex4.1)$adj.r.squared

## For all observations (no time var)
# I argue that because all samples appear 3 times,
# it won't be an issue ni the GLM, cuz its almost
# controled...? I mean, in ALL samples.
all_time <-
  data %>%
  select(-IID)

## For time comparison
W0 <-
  data %>%
  filter(wave == "W0") %>%
  select(-wave) %>%
  select(-IID)

W1 <-
  data %>%
  filter(wave == "W1") %>%
  select(-wave) %>%
  select(-IID)

W2 <-
  data %>%
  filter(wave == "W2") %>%
  select(-wave) %>%
  select(-IID)

## Get Odds Ratio into a table
all_time_OR <- uni_model_OR(all_time)

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
