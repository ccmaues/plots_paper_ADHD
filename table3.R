# Controlled variable bias verification
source("functions_to_source.R")

# PRS
vADHD <-
  prs_v3 %>%
  select(IID, ADHD) %>%
  rename(PRS = ADHD)

# Pheno for wave id
pADHD <-
  pheno %>%
  select(IID, wave, dcanyhk) %>%
  rename(diagnosis = 3)

data <-
  plyr::join_all(list(sex, vADHD, state, ages, pADHD, pc), by = "IID", type = "inner") %>%
  select(-starts_with("age_")) %>%
  mutate(
    sex = as.factor(sex),
    popID = as.factor(popID),
    diagnosis = as.factor(diagnosis),
    wave = as.factor(wave))

library(car)
library(rstatix)

# incluenciates the SD, SE and CI if not unique.
non_redudant_data <-
  data %>%
  select(PRS, sex, popID) %>%
  unique()

non_redudant_data %>%
  group_by(sex) %>%
  get_summary_stats(PRS, type = "common")

non_redudant_data  %>%
  group_by(popID) %>%
  get_summary_stats(PRS, type = "common")

## T test for two samples
# without wave
# NORMALITY:
non_redudant_data %>%
  group_by(sex) %>%
  shapiro_test(PRS)

non_redudant_data %>%
  group_by(sex) %>%
  
ks.test(filter(non_redudant_data, sex == "Female")$PRS, rnorm(1363))

non_redudant_data %>%
  group_by(popID) %>%
  shapiro_test(PRS)

# EQUAL VARIANCES:
levene_test(non_redudant_data, PRS ~ sex, center = "mean")
levene_test(non_redudant_data, PRS ~ popID, center = "mean")

# T-TEST
t_test(non_redudant_data, PRS ~ sex)
t_test(non_redudant_data, PRS ~ popID)

# EFFECt SIZE
cohens_d(non_redudant_data, PRS ~ sex, var.equal = TRUE)
cohens_d(non_redudant_data, PRS ~ popID, var.equal = TRUE)
