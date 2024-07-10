#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

###### distribution determination
# Verify the outcome distribution
# (don't know if i have to test one
# per time of all at once)
library(rstatix)

# variable summary
data %>%
  group_by(wave) %>%
  get_summary_stats(age, type = "common")

# normality test
data %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
  select(starts_with("W")) %>%
  shapiro_test(W0, W1, W2)

data %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
  select(starts_with("W")) %>%
  mshapiro_test()

# extreme outiliers verification
data %>%
  filter(wave == "W0") %>%
  select(age) %>%
  identify_outliers(age)
knitr::kable()

data %>%
  filter(wave == "W1") %>%
  select(age) %>%
  identify_outliers(age)

data %>%
  filter(wave == "W2") %>%
  select(age) %>%
  identify_outliers(age)

# QQ Plot
ggthemr("fresh")
p1 <-
  data %>%
  ggplot(aes(sample = age, color = wave)) +
    geom_qq(size = 5, alpha = 0.8) +
    geom_qq_line(linewidth = 2, alpha = 0.6, linetype = "dashed") +
    labs(title = "original age distribution") +
    theme_publish()

# Histogram
p2 <-
  data %>%
  ggplot(aes(age, color = wave)) +
    geom_density(alpha = 0.8, linewidth = 2)

# cumulative
p3 <-
  data %>%
  ggplot(aes(age, color = wave)) +
  stat_ecdf(geom = "step") +
  theme_publish()

# ECDF plot (or Empirical Cumulative Density Function)
# https://en.wikipedia.org/wiki/Normal_distribution
# https://en.wikipedia.org/wiki/Gamma_distribution
# https://en.wikipedia.org/wiki/Inverse_Gaussian_distribution

##### new scale
# transform the data and
# see if the distribution changes
# otherwise use GLMM

# variable summary
new_scale <-
  data %>%
  mutate(age = as.numeric(scale(age)))

new_scale %>%
  group_by(wave) %>%
  get_summary_stats(age, type = "common")

# normality test
new_scale %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
  select(starts_with("W")) %>%
  shapiro_test(W0, W1, W2)

new_scale %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
  select(starts_with("W")) %>%
  mshapiro_test()

# extreme outiliers verification
new_scale %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
    select(starts_with("W")) %>%
  identify_outliers(W0)

new_scale %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
    select(starts_with("W")) %>%
  identify_outliers(W1)

new_scale %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
    select(starts_with("W")) %>%
  identify_outliers(W2)

# QQ Plot
p3 <-
  new_scale %>%
  ggplot(aes(sample = age, color = wave)) +
    geom_qq(size = 5, alpha = 0.8) +
    geom_qq_line(linewidth = 2, alpha = 0.6, linetype = "dashed") +
    labs(title = "transformed age distribution") +
    theme_publish()

# histogram
p4 <-
  new_scale %>%
  ggplot(aes(age, color = wave)) +
    geom_density(alpha = 0.8, linewidth = 2)

library(patchwork)
(p1 + p2) / (p3 + p4)

###### run the model
# Install and load required packages
# install.packages("lme4")
library(lme4)

# Fit the GLMM with Gamma family and log link
model <- glmer(age ~ adjusted_PRS + sex:diagnosis + (1 | IID), family = Gamma(link = "log"), data = data)

# Summary of the model
summary(model)

# Model assumption testing

# Linearity: Residuals vs Fitted plot
plot(model, which = 1)

# Normality of Residuals: Q-Q plot and Shapiro-Wilk test
qqnorm(residuals(model))
qqline(residuals(model))
shapiro.test(residuals(model))

# Homoscedasticity: Plot residuals against fitted values
plot(residuals(model) ~ fitted(model))

# Residuals and Fitted Values
residuals <- resid(model, type = "deviance")
fitted_values <- fitted(model)

# Residuals vs Fitted Values Plot
plot(fitted_values, residuals, 
     xlab = "Fitted Values", 
     ylab = "Deviance Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# Q-Q Plot for Residuals
qqnorm(residuals)
qqline(residuals, col = "red")

# Normality of Random Effects
random_effects <- ranef(model)
qqnorm(unlist(random_effects))
qqline(unlist(random_effects), col = "red")

# Shapiro-Wilk Test for Residuals
shapiro.test(residuals)

# Scale-Location Plot
sqrt_abs_residuals <- sqrt(abs(residuals))
plot(fitted_values, sqrt_abs_residuals, 
     xlab = "Fitted Values", 
     ylab = "Square Root of |Residuals|",
     main = "Scale-Location Plot")
abline(h = 0, col = "red")

# Partial Residual Plots
library(car)
crPlots(model)

# https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
# they say in here that is good to test some
# different models and see thei aplicabillity