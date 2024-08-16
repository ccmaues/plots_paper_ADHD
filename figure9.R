setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

###### Survival analysis for ADHD
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

survdiff(
  Surv(time, status) ~ PRS,
  data = wd2
)

surv_fit <- survfit(Surv(time, status) ~ PRS, data = wd2)
ggthemr("fresh")

p <-
  ggsurvplot(
    surv_fit,
    data = wd2,
    linetype = "strata",
    fun = "event", # changes to the chance of diagnosis
    censor = TRUE,
    censor.shape = "|",
    censor.size = 3,
    surv.scale = "percent",
    surv.plot.height = 0.5,
    break.time.by = 2,
    break.y.by = 0.05,
    xlab = "Age (yr)",
    ylab = "Diagnosis Probability",
    legend.labs = c("1st", "2nd", "3rd", "4th", "5th"),
    legend.title = "PRS quintile",
    xlim = c(6, max(wd2$time)),
    risk.table = "abs_pct",
    risk.table.col = "strata",
    risk.table.fontsize = 4,
    risk.table.y.text = FALSE,
    tables.height = 0.15,
    cumevents = TRUE,
    cumevents.col = "strata",
    cumevents.y.text = FALSE,
    cumevents.height = 0.15,
    font.legend = 15,
    ggtheme = theme_publish()
  )


p1 <-
  p$plot +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.1, "lines") # issue
  )

ggsave("Figure9.png", p1, device = "png", units = "mm", height = 90, width = 120)

#### Verification of the model
# log.rank.weights
# cox_model <- coxph(Surv(time, status) ~ PRS, data = wd2)
# ph_assumption <- cox.zph(cox_model)
# print(ph_assumption)
# plot(ph_assumption)

# c_index <- concordance(Surv(wd2$time, wd2$status) ~ fitted(cox_model))
# print(c_index)