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
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
    select(starts_with("W")) %>%
  identify_outliers(W0)

data %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
    select(starts_with("W")) %>%
  identify_outliers(W1)

data %>%
  pivot_wider(
    names_from = wave,
    values_from = age) %>%
    select(starts_with("W")) %>%
  identify_outliers(W2)

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
install.packages("lme4")
library(lme4)

# Fit the GLMM with Gamma family and log link
model <-
  glmer(
    age ~ adjusted_PRS + sex:diagnosis + (1 | IID),
    family = Gamma(link = "log"), data = data)

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

