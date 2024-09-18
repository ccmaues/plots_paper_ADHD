# Controlled variable bias verification
setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

library(car)
library(rstatix)

# PRS data
prs <-
  fread(glue("{Path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
  select(IID, PRSCS_zscore) %>%
  rename(original_PRS = 2) %>%
  inner_join(., select(data, IID, PRS, sex, popID), by = "IID") %>%
  rename(adjusted_PRS = PRS)

# incluenciates the SD, SE and CI if not unique.
non_redudant_data <-
  prs %>%
  unique()

## T test for two samples
# without wave
# NORMALITY:
non_redudant_data %>%
  group_by(sex) %>%
  shapiro_test(adjusted_PRS)

non_redudant_data %>%
  group_by(sex) %>%
  shapiro_test(original_PRS)
  
non_redudant_data %>%
  group_by(popID) %>%
  shapiro_test(adjusted_PRS)

non_redudant_data %>%
  group_by(popID) %>%
  shapiro_test(original_PRS)
