setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

#### MULTIPLE SURVIVAL/MODEL TESTING
pacman::p_load(survival, survminer)
# Simplify the data
# separate the controls
t1 <-
  data %>%
  filter(wave == "W2" & diagnosis == 0)
# separate the cases and
# keep just the first occurance of diagnosis
t2 <-
  data %>%
  filter(!IID %in% t1$IID) %>%
  filter(diagnosis == 2) %>%
  group_by(IID) %>%
  filter(age == min(age)) %>%
  ungroup()

wd2 <-
  bind_rows(t1, t2) %>%
  select(IID, age, diagnosis, adjusted_PRS) %>%
  mutate(
    quintile = ntile(adjusted_PRS, 5),
    diagnosis = factor(diagnosis, levels = c("0", "2"), labels = c("0", "1")),
    diagnosis = as.numeric(as.character(diagnosis))) %>%
    select(-adjusted_PRS) %>%
  rename(ID = 1, time = 2, status = 3, PRS = 4) %>%
  mutate(time = round(time, digits = 0))

surv_fit <- survfit(Surv(time, status) ~ PRS, data = wd2)

#### Verification of the model
# log.rank.weights
cox_model <- coxph(Surv(time, status) ~ PRS, data = wd2)
ph_assumption <- cox.zph(cox_model)
print(ph_assumption)
plot(ph_assumption)

c_index <- concordance(Surv(wd2$time, wd2$status) ~ fitted(cox_model))
print(c_index)