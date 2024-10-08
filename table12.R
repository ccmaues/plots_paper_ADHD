setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

###### Survival analysis for ADHD
# probability of those in the interesting
# quintiles of converting in the years
# Install and load the necessary packages
pacman::p_load(survival, survminer, survcomp)

# Example data (replace this with your actual dataset)
# Assuming your data frame is called df and has columns:
# time, status, and PGS
# time: time to event or censoring
# status: 1 if event occurred, 0 if censored
# PGS: polygenic score group (e.g., "High", "Medium", "Low")

# Simplify the data
# separate the controls
t1 <-
	data %>%
  filter(age >= 10 & age <= 20) %>%
  filter(wave == "W2" & diagnosis == 0)
# separate the cases and
# keep just the first occurance of diagnosis
t2 <-
  data %>%
	filter(age >= 10 & age <= 20) %>%
  filter(!IID %in% t1$IID) %>%
  filter(diagnosis == 2) %>%
  group_by(IID) %>%
  filter(age == min(age)) %>%
  ungroup()

wd2 <-
  bind_rows(t1, t2) %>%
  select(IID, age, diagnosis, adjusted_PRS) %>%
  mutate(
    risk = ntile(adjusted_PRS, 3),
    diagnosis = factor(diagnosis, levels = c("0", "2"), labels = c("0", "1")),
    diagnosis = as.numeric(as.character(diagnosis)),
    PRS = case_when(
      risk == 1 ~ "Low",
      risk == 2 ~ "Medium",
      risk == 3 ~ "High"),
    PRS = factor(PRS, levels = c("Low", "Medium", "High"))) %>%
    select(-adjusted_PRS, -risk) %>%
  rename(ID = 1, time = 2, status = 3) %>%
  mutate(time = round(time, digits = 0)) %>%
	rename(IID = 1) %>%
	inner_join(., sex, by = "IID") %>%
  rename(ID = 1)

a <- survdiff(Surv(time, status) ~ PRS, data = wd2)
b <- survdiff(Surv(time, status) ~ PRS, data = filter(wd2, sex == "Female"))
c <- survdiff(Surv(time, status) ~ PRS, data = filter(wd2, sex == "Male"))

opt <- data.frame(
  model = c("All data", "Female", "Male"),
  n = c(nrow(wd2), nrow(filter(wd2, sex == "Female")), nrow(filter(wd2, sex == "Male"))),
  chisq = c(a$chisq, b$chisq, c$chisq),
  pvalue = c(a$pvalue, b$pvalue, c$pvalue)) %>%
  mutate(across(chisq:pvalue, round, 3))

