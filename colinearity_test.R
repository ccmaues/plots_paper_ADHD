#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")


# The statistic we will use to diagnose collinearity is the variance inflation factor (VIF)
# https://www.bookdown.org/rwnahhas/RMPH/mlr-collinearity.html#mlr-collinearity
# we DO NOT want the colinearity or near that. It is an issue
# to the model
# If two predictors are perfectly collinear, then they
# are completely redundant for the purposes of linear regression
# https://www.bookdown.org/rwnahhas/RMPH/mlr-linearity.html
# apparently the categorical ones that can be transformed into
# dummy variables don't need that. They are used in the model in
# a different way

# Variance Inflation Factor (VIF) measures the severity of multicollinearity 
car::vif(
  glm(diagnosis ~ adjusted_PRS + age:sex + p_diagnosis + popID,
  filter(data, wave == "W0"),
  family = "binomial"))
# no multicolinearity :D
car::outlierTest(
  glm(diagnosis ~ adjusted_PRS + age:sex + p_diagnosis + popID,
  filter(data, wave == "W0"), family = "binomial"),
  n.max = Inf)
# one outlier but not significant

car::residualPlots(
  glm(diagnosis ~ adjusted_PRS + age:sex + p_diagnosis + popID,
  filter(data, wave == "W0"), family = "binomial"),
  pch = 20, col = "gray", fitted = T, terms = ~ 1,
  tests = F, quadratic = F, type = "rstudent")
# i can see the two ones
car::influenceIndexPlot(
  glm(diagnosis ~ adjusted_PRS + age:sex + p_diagnosis + popID,
  filter(data, wave == "W0"), family = "binomial"), vars = "Cook",
  id=F, main = "Cook's distance")

# Perform a sensitivity analysis
# ver depois

# https://www.bookdown.org/rwnahhas/RMPH/blr-interaction.html !!!!
# poderia fazer uma nova coluna com N diagnósticos por idade

plot_model(
  glm(diagnosis ~ original_PRS + age:sex + p_diagnosis + popID,
  filter(data, wave == "W0"), family = "binomial"),
  type = "eff", terms = c("original_PRS [all]", "sex"),
  title = "", axis.title = c("Age of diagnosis (years)",
  "PRS"), legend.title = "Sex")

all_time <-
  data %>%
  select(-IID)

## For time comparison
W0 <-
  data %>%
  filter(wave == "W0") %>%
  select(-wave) %>%
  select(-IID)

W1 <-
  data %>%
  filter(wave == "W1") %>%
  select(-wave) %>%
  select(-IID)

W2 <-
  data %>%
  filter(wave == "W2") %>%
  select(-wave) %>%
  select(-IID)

filter(data, wave == "W0") %>%
select(sex, popID, diagnosis, age) %>%
tbl_summary()

filter(data, wave == "W1") %>%
select(sex, popID, diagnosis, age) %>%
tbl_summary()

filter(data, wave == "W2") %>%
select(sex, popID, diagnosis, age) %>%
tbl_summary()

filter(ad, IID %in% data$IID) %>%
select(-IID) %>%
tbl_summary()

data <-
  data %>%
  mutate(ntile = as.factor(ntile(adjusted_PRS, 5)))

data_female <- filter(data, sex == "Female")
data_male <- filter(data, sex == "Male")

# how much is the "jump" of diagnosis for each
# quintile in each wave?
# colocar outros argumentos do tbl_regression dps
first <-
glm(diagnosis ~ wave, filter(data_male, ntile == 1), family = binomial) %>%
# broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
# mutate(across(where(is.numeric), round, digits = 2))
tbl_regression(., exponentiate = TRUE)

second <-
glm(diagnosis ~ wave, filter(data_male, ntile == 2), family = binomial) %>%
# broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
# mutate(across(where(is.numeric), round, digits = 2))
tbl_regression(., exponentiate = TRUE)

third <-
glm(diagnosis ~ wave, filter(data_male, ntile == 3), family = binomial) %>%
# broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
# mutate(across(where(is.numeric), round, digits = 2))
tbl_regression(., exponentiate = TRUE)

fourth <-
glm(diagnosis ~ wave, filter(data_male, ntile == 4), family = binomial) %>%
# broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
# mutate(across(where(is.numeric), round, digits = 2))
tbl_regression(., exponentiate = TRUE)

firth <-
glm(diagnosis ~ wave, filter(data_male, ntile == 5), family = binomial) %>%
# broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
# mutate(across(where(is.numeric), round, digits = 2))
tbl_regression(., exponentiate = TRUE)

tbl_merge(
  tbls = list(first, second, third, fourth, firth),
  tab_spanner = c("**1st**", "**2nd**", "**3rd**", "**4th**", "**5th**")) %>%
as_flex_table() %>%
flextable::save_as_docx(fileext = ".docx", path = "male_quintile_OR.docx")