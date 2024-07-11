setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
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
  identify_outliers(age) %>%
  knitr::kable()

data %>%
  filter(wave == "W1") %>%
  select(age) %>%
  identify_outliers(age) %>%
  knitr::kable()

data %>%
  filter(wave == "W2") %>%
  select(age) %>%
  identify_outliers(age) %>%
  knitr::kable()

# QQ Plot
ggthemr("fresh")
p1 <-
  data %>%
  ggplot(aes(sample = age, color = wave)) +
    geom_qq(size = 1, alpha = 0.8) +
    geom_qq_line(linewidth = 0.5, alpha = 0.6, linetype = "dashed") +
    theme_publish() +
    labs(
      y = "Expected observations",
      x = "Observations") +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 10),
      text = element_text(family = "Arial", color = "black"),
      axis.line.x = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
      axis.text.x = element_text(size = 10),
      axis.line.y = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.y = element_line(linewidth = 0.5, color = "black"),
      axis.text.y = element_text(size = 10))

# histogram
p2 <-
  data %>%
  ggplot(aes(age, color = wave)) +
    geom_histogram(
      linewidth = 1,
      fill = "#ffffff00") +
    theme_publish() +
    facet_grid(~wave) +
    labs(
    y = "Frequency",
    x = "Age (yr)") +
    theme(
      panel.spacing = unit(0.5, "lines"),
      strip.text.x = element_blank(),
      legend.position = "right",
      axis.title = element_text(size = 10),
      text = element_text(family = "Arial", color = "black"),
      axis.line.x = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
      axis.text.x = element_text(size = 10),
      axis.line.y = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.y = element_line(linewidth = 0.5, color = "black"),
      axis.text.y = element_text(size = 10))

# cumulative
p3 <-
  data %>%
  ggplot(aes(age, color = wave)) +
  stat_ecdf(geom = "step", linewidth = 1) +
  theme_publish() +
  labs(
    y = "Cumulative Empirical\nDistribution",
    x = "Age (yr)") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10),
    text = element_text(family = "Arial", color = "black"),
    axis.line.x = element_line(linewidth = 0.5, color = "black"),
    axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
    axis.text.x = element_text(size = 10),
    axis.line.y = element_line(linewidth = 0.5, color = "black"),
    axis.ticks.y = element_line(linewidth = 0.5, color = "black"),
    axis.text.y = element_text(size = 10))

library(patchwork)
teste <- p1 / p2 / p3
print(teste)
ggsave(
  "Figure8.png", width = 85, height = 150, units = "mm",
  bg = "transparent", scale = c(2, 1.5))

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
  filter(wave == "W0") %>%
  select(age) %>%
  identify_outliers(age) %>%
  knitr::kable()

new_scale %>%
  filter(wave == "W1") %>%
  select(age) %>%
  identify_outliers(age) %>%
  knitr::kable()

new_scale %>%
  filter(wave == "W2") %>%
  select(age) %>%
  identify_outliers(age) %>%
  knitr::kable()

# QQ Plot
p4 <-
  new_scale %>%
  ggplot(aes(sample = age, color = wave)) +
    geom_qq(size = 5, alpha = 0.8) +
    geom_qq_line(linewidth = 2, alpha = 0.6, linetype = "dashed") +
    labs(title = "transformed age distribution") +
    theme_publish()

# histogram
p5 <-
  new_scale %>%
  ggplot(aes(age, color = wave)) +
    geom_density(alpha = 0.8, linewidth = 2)

(p1 + p2) / (p3 + p4)