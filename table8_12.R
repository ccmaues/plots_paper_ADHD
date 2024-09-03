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

surv_summary <- summary(surv_fit)

survival_list <- list()

for (i in unique(surv_summary$strata)) {
  group_data <- surv_summary$time[surv_summary$strata == i]
  n_risk <- surv_summary$n.risk[surv_summary$strata == i]
  n_event <- surv_summary$n.event[surv_summary$strata == i]
  survival <- surv_summary$surv[surv_summary$strata == i]
  std_err <- surv_summary$std.err[surv_summary$strata == i]
  lower_95_CI <- surv_summary$lower[surv_summary$strata == i]
  upper_95_CI <- surv_summary$upper[surv_summary$strata == i]
  data_frame <- data.frame(
    time = group_data,
    n.risk = n_risk,
    n.event = n_event,
    survival = survival,
    std.err = std_err,
    lower_95_CI = lower_95_CI,
    upper_95_CI = upper_95_CI
  )
#  survival_list[[i]] <- data_frame
	data_frame %>%
	mutate(across(1:7, round, digits = 3)) %>%
	rename(
		"Age(yr)" = 1, "N risk" = 2, "N event" = 3,
		Survival = 4, "Std. Error" = 5, "Lower 95% CI" = 6, "Upper 95% CI" = 7) %>%
	fwrite(glue("{i}_adj_ADHD_surv.tsv"), sep = "\t")
}
