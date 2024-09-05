setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

#### MULTIPLE SURVIVAL/MODEL TESTING
# 1. Surv(time, status) ~ PRS
# 2. Surv(time, status) ~ PRS + sex
# 3. Surv(time, status) ~ PRS + p_parents
# 4. Surv(time, status) ~ PRS + sex*p_parents
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

wd <-
  bind_rows(t1, t2) %>%
  select(IID, age, diagnosis, adjusted_PRS, sex) %>%
	inner_join(., select(data, IID, p_diagnosis), by = "IID") %>%
  mutate(
    quintile = ntile(adjusted_PRS, 5),
    diagnosis = factor(diagnosis, levels = c("0", "2"), labels = c("0", "1")),
    diagnosis = as.numeric(as.character(diagnosis))) %>%
    select(-adjusted_PRS) %>%
  rename(ID = 1, time = 2, status = 3, Sex = 4, p_diagnosis = 5, PRS = 6) %>%
  mutate(time = round(time, digits = 0))

##### MODEL TESTING ----------------------------------------
# AFT M1
pacman::p_load(flexsurv, SurvRegCensCov)
aft <- survreg(Surv(time, status) ~ PRS, data = wd, dist = 'weibull')
ConvertWeibull(aft, conf.level = 0.95)
flexsurvreg(Surv(time, status) ~ PRS, data = wd, dist = 'weibull')

# Model 1
m1 <- coxph(Surv(time, status) ~ PRS, data = wd)
summary(m1)
# Check proportional harzards assumption Schoenfeld residuals
phaM1 <- cox.zph(m1)
# Must be p > 0.05 = no variance throughout the time
print(phaM1)
# Must present linearity an "Overall" line linear too
plot(phaM1)
# Is the model correct?
cixM1 <- concordance(Surv(time, status) ~ predict(m1), data = wd)
print(cixM1)

# m 2
m2 <- coxph(Surv(time, status) ~ PRS + Sex, data = wd)
summary(m2)
cox.zph(m2)
plot(cox.zph(m2))
concordance(Surv(time, status) ~ predict(m2), data = wd)

# m 3
m3 <- coxph(Surv(time, status) ~ PRS + p_diagnosis, data = wd)
summary(m3)
cox.zph(m3)
plot(cox.zph(m3))
concordance(Surv(time, status) ~ predict(m3), data = wd)

# m 4
m4 <- coxph(Surv(time, status) ~ PRS + Sex + p_diagnosis, data = wd)
summary(m4)
cox.zph(m4)
plot(cox.zph(m4))
concordance(Surv(time, status) ~ predict(m4), data = wd)

# m 5
m5 <- coxph(Surv(time, status) ~ PRS + Sex*p_diagnosis, data = wd)
summary(m5)
cox.zph(m5)
plot(cox.zph(m5))
concordance(Surv(time, status) ~ predict(m5), data = wd)
