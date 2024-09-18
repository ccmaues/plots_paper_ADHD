# Controlled variable bias verification
setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

# PRS data
prs <-
  fread(glue("{Path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
  select(IID, PRSCS) %>% # question: have I been using a adjusted TWICE PRSCS score?
  rename(original_PRS = 2) %>%
  inner_join(., select(data, IID, PRS, popID), by = "IID") %>%
  rename(adjusted_PRS = PRS)

qqplot(non_redudant_data$original_PRS, rnorm(1363))

library(car)
library(rstatix)

# incluenciates the SD, SE and CI if not unique.
non_redudant_data <-
  prs %>%
  unique()

non_redudant_data  %>%
  group_by(popID) %>%
  get_summary_stats(adjusted_PRS, original_PRS, type = "common")

## T test for two samples

# EQUAL VARIANCES:
levene_test(non_redudant_data, original_PRS ~ popID, center = "mean")
levene_test(non_redudant_data, adjusted_PRS ~ popID, center = "mean")

# T-TEST
t_test(non_redudant_data, original_PRS ~ popID)
t_test(non_redudant_data, adjusted_PRS ~ popID)

# EFFECt SIZE
cohens_d(non_redudant_data, original_PRS ~ popID, var.equal = TRUE)
cohens_d(non_redudant_data, adjusted_PRS ~ popID, var.equal = TRUE)
