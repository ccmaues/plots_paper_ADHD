# Controlled variable bias verification
setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

# PRS data
# incluenciates the SD, SE and CI if not unique.
non_redudant_data <-
  data %>%
  select(IID, adjusted_PRS, zscore_PRS, popID) %>%
  unique()

library(car)
library(rstatix)

non_redudant_data  %>%
  group_by(popID) %>%
  get_summary_stats(adjusted_PRS, zscore_PRS, type = "common")

## T test for two samples
# EQUAL VARIANCES:
levene_test(non_redudant_data, zscore_PRS ~ popID, center = "mean")
levene_test(non_redudant_data, adjusted_PRS ~ popID, center = "mean")

# T-TEST
t_test(non_redudant_data, zscore_PRS ~ popID, var.equal =  TRUE)
t_test(non_redudant_data, adjusted_PRS ~ popID, var.equal =  TRUE)

# EFFECt SIZE
cohens_d(non_redudant_data, zscore_PRS ~ popID, var.equal = TRUE)
cohens_d(non_redudant_data, adjusted_PRS ~ popID, var.equal = TRUE)
