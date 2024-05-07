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
## T test for two samples

# NORMALITY:
data %>%
  group_by(wave, diagnosis, sex) %>%
  shapiro_test(PRS)

data %>%
  group_by(wave, diagnosis, popID) %>%
  shapiro_test(PRS)

# EQUAL VARIANCES:
levene_test(data, PRS ~ sex, center = "mean")
levene_test(data, PRS ~ popID, center = "mean")

# INDEPEDENCE:
# the samples are independent

# T-TEST
t_test(data, PRS ~ sex, var.equal = TRUE, detailed = TRUE)
t_test(data, PRS ~ popID, var.equal = TRUE, detailed = TRUE)

# COHEN D
cohens_d(data, PRS ~ sex, var.equal = TRUE)
cohens_d(data, PRS ~ popID, var.equal = TRUE)
