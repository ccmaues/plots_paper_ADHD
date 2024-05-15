#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

## For all observations (no time var)
# I argue that because all samples appear 3 times,
# it won't be an issue ni the GLM, cuz its almost
# controled...? I mean, in ALL samples.
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

## Get Odds Ratios for plotting (already in OR the estimate)
all_time_plot <- uni_model_cal(all_time)
W0_plot <- uni_model_cal(W0)
W1_plot <- uni_model_cal(W1)
W2_plot <- uni_model_cal(W2)

## Get Odds Ratio into a table
all_time_OR <- uni_model_OR(all_time)

W0_OR <- uni_model_OR(W0)
W1_OR <- uni_model_OR(W1)
W2_OR <- uni_model_OR(W2)

# all_time_OR %>%
# as_flex_table() %>%
# flextable::save_as_docx(fileext = ".docx", path = "all_time_model.docx")

# W0_OR %>%
# as_flex_table() %>%
# flextable::save_as_docx(fileext = ".docx", path = "W0_model.docx")

# W1_OR %>%
# as_flex_table() %>%
# flextable::save_as_docx(fileext = ".docx", path = "W1_model.docx")

# W2_OR %>%
# as_flex_table() %>%
# flextable::save_as_docx(fileext = ".docx", path = "W2_model.docx")
# knitr::kable()

## Forest Plot
for_plot <-
  all_time_plot %>%
  mutate(
    term = c(
      "Female", "Male", "(Intercept)", "PRS\nadjusted", "RS", "SP",
      "(Intercept)", "Age", "(Intercept)", "PRS\noriginal",
      "(Intercept)", "Parents'\ndiagnosis"
  )) %>%
  filter(term != "(Intercept)")

for_plot <-
  W0_plot %>%
  mutate(
    term = c(
      "Female", "Male", "(Intercept)", "PRS\nadjusted", "RS", "SP",
      "(Intercept)", "Age", "(Intercept)", "PRS\noriginal",
      "(Intercept)", "Parents'\ndiagnosis"
  )) %>%
  filter(term != "(Intercept)")

for_plot <-
  W1_plot %>%
  mutate(
    term = c(
      "Female", "Male", "(Intercept)", "PRS\nadjusted", "RS", "SP",
      "(Intercept)", "Age", "(Intercept)", "PRS\noriginal",
      "(Intercept)", "Parents'\ndiagnosis"
  )) %>%
  filter(term != "(Intercept)")

for_plot <-
  W2_plot %>%
  mutate(
    term = c(
      "Female", "Male", "(Intercept)", "PRS\nadjusted", "RS", "SP",
      "(Intercept)", "Age", "(Intercept)", "PRS\noriginal",
      "(Intercept)", "Parents'\ndiagnosis"
  )) %>%
  filter(term != "(Intercept)")

p_mid <-
  for_plot %>%
  ggplot(aes(y = fct_rev(term))) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(aes(x = estimate, color = factor(estimate > 1)), shape = 15, size = 3) +
  labs(x = "Odds Ratio", y = "") +
  coord_cartesian(ylim = c(1, 9), xlim = c(0, 3.5)) +
  scale_color_manual(values = c("#7a0000", "#01942d")) +
  theme(
    axis.line.x = element_line(linewidth = 0.5, color = "black"),
    axis.line.y = element_blank(),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black"),
    axis.ticks.y = element_blank(),
    legend.position = "none")

library(stringr)
library(forcats)

for_plot2 <-
  for_plot %>%
  mutate(
  across(c(estimate, conf.low, conf.high),
    ~str_pad(round(.x, 2), width = 4, pad = "0", side = "right")),
    OR_CI = paste(estimate, " (", conf.low, "-", conf.high, ")", sep = "")) %>%
  mutate(p.value = case_when(
    p.value < .001 ~ "<0.001",
    round(p.value, 2) == .05 ~ as.character(round(p.value, 3)),
    p.value < .01 ~ str_pad(
    as.character(round(p.value, 3)), width = 4, pad = "0", side = "right"),
    TRUE ~ str_pad(
    as.character(round(p.value, 2)), width = 4, pad = "0", side = "right"))) %>%
  bind_rows(
    data.frame(
    term = "Variable",
    OR_CI = "Hazard Ratio (95% CI)",
    conf.low = "",
    conf.high = "",
    p.value = "p-value"), .) %>%
    mutate(
      term = fct_rev(fct_relevel(term, "Variable")))

p_left <-
  for_plot2 %>%
  ggplot(aes(y = term)) +
  geom_text(aes(x = 0, label = term), hjust = 0,
  fontface = ifelse(for_plot2$term == "Variable", "bold", "plain")) +
    theme_void() +
    coord_cartesian(xlim = c(0, 4))

p_right <-
  for_plot2 %>%
  ggplot() +
  geom_text(
    aes(x = 0, y = term, label = p.value),
    hjust = 0,
    fontface = ifelse(for_plot2$p.value == "p-value", "bold", "plain")) +
  theme_void()

layout <- c(
  area(t = 0, l = 0, b = 30, r = 3),
  area(t = 1, l = 4, b = 30, r = 10),
  area(t = 0, l = 9, b = 30, r = 13))

p_left + p_mid + p_right + plot_layout(design = layout)

ggsave(
  "forest_plot_uni_W2.png", device = "png",
  units = "mm", height = 200, width = 110
)

# https://www.khstats.com/blog/forest-plots/