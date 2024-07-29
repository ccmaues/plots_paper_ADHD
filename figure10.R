setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

###### Survival analysis for ADHD (sex based)
# probability of those in the interesting
# quintiles of converting in the years
# Install and load the necessary packages
pacman::p_load(survival, survminer)

# Example data (replace this with your actual dataset)
# Assuming your data frame is called df and has columns:
# time, status, and PGS
# time: time to event or censoring
# status: 1 if event occurred, 0 if censored
# PGS: polygenic score group (e.g., "High", "Medium", "Low")
wd <-
  data %>%
  select(IID, age, diagnosis, adjusted_PRS, sex) %>%
  mutate(
    quintile = ntile(adjusted_PRS, 5),
    diagnosis = factor(diagnosis, levels = c("0", "2"), labels = c("0", "1")),
    diagnosis = as.numeric(as.character(diagnosis))) %>%
    select(-adjusted_PRS) %>%
  rename(ID = 1, time = 2, status = 3, sex = 4, PRS = 5) %>%
  filter(PRS %in% c(3, 5)) %>%
  mutate(PRS =  ifelse(PRS == 5, 1, 0))
# mind you that the age is adjusted by the eval date
# theres need to correct it in the formula later

survdiff(
  Surv(time, status) ~ PRS + sex,
  data = wd
)

# Create survival curves
surv_fit <- survfit(Surv(time, status) ~ PRS + sex, data = wd)

# não é ver se é diferente
# e ver só o ponto de conversão
ggsurvplot(
  surv_fit,
  data = wd,
  linetype = "strata",
  fun = "event",
  #conf.int = TRUE,
  surv.scale = "percent",
  break.time.by = 2,
  xlab = "Age (yr)",
  ylab = "Diagnosis Probability",
  legend.labs =
  c("5th F", "5th M",
    "3rd F", "3rd M"),
  legend.title = "PRS quintile\nby sex",
  pval = TRUE,
  xlim = c(5, max(wd$time)),
  risk.table = TRUE,
  risk.table.col = "strata",
  ncensor.plot = TRUE,
  font.legend = 20,
  surv.median.line = "hv",
  ggtheme = theme_publish()
)
