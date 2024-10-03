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
  select(IID, age, diagnosis, adjusted_PRS) %>%
  mutate(
    risk = ntile(adjusted_PRS, 5),
    diagnosis = factor(diagnosis, levels = c("0", "2"), labels = c("0", "1")),
    diagnosis = as.numeric(as.character(diagnosis)),
    PRS = case_when(
      risk == 1 ~ "1st",
      risk == 2 ~ "2nd",
      risk == 3 ~ "3rd",
      risk == 4 ~ "4th",
      risk == 5 ~ "5th"),
    PRS = factor(PRS, levels = c("1st", "2nd", "3rd", "4th", "5th"))) %>%
    select(-adjusted_PRS, -risk) %>%
  rename(ID = 1, time = 2, status = 3) %>%
  mutate(time = round(time, digits = 0)) %>%
	rename(IID = 1) %>%
	inner_join(., select(data, IID, sex, popID, p_diagnosis), by = "IID") %>%
  rename(ID = 1) %>%
  filter(sex == "Male") %>%
  unique()

#### Verification of the model
  # Log.rank test: is there any difference between the curves?
  # evaluate the effect of the predictor on survival more formally
  # check propotional hazards assumptions
  # Must be p > 0.05 = no variance throughout the time
  # Must not present any non-linear pattern
  # Concordance between model x data

test_surv_model <- function(data_frame, equation = NULL) {
  if (missing(equation) || is.null(equation)) {
    cox_model <- coxph(Surv(time, status) ~ PRS, data = data_frame)
    diff <- survdiff(Surv(time, status) ~ PRS, data = data_frame)
  } else {
    cox_model <- coxph(as.formula(equation), data = data_frame)
    diff <- survdiff(as.formula(equation), data = data_frame)
  }
  model_summary <- summary(cox_model)
  cox_zph <- cox.zph(cox_model)
  data.frame(
    model_pvalue = model_summary$coefficients[, "Pr(>|z|)"][1],
    cox_assumption_chisq = cox_zph$table[1, "chisq"],
    cox_assumption_df = cox_zph$table[1, "df"],
    cox_assumption_pvalue = cox_zph$table[1, "p"],
    chisq_curves = diff$chisq,
    chisq_pvalue = pchisq(diff$chisq, df = length(diff$n) - 1, lower.tail = FALSE),
    chisq_df = length(diff$n) - 1,
    concordance = model_summary$concordance[1],
    concordance_se = model_summary$concordance[2]) %>%
   mutate(across(where(is.numeric), round, 3)) %>%
  return()
}

# Example usage
m1 <- test_surv_model(wd)
# m 2
m2 <- test_surv_model(wd, "Surv(time, status) ~ PRS + p_diagnosis")

# create final table
cbind(
  model = c("M1", "M2"),
  bind_rows(m1, m2)) %>%
knitr::kable("simple", row.names = FALSE)

cbind(
  model = c("M1", "M2"),
  bind_rows(m1, m2)) %>%
writexl::write_xlsx(glue("table15.xlsx"))
