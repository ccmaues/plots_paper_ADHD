#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

## For all observations (no time var) FALTA A IDADEEE
no_duplicate <-
  data %>%
  select(-age, -wave) %>%
  unique() %>%
  mutate(
    new_risk = add_risk(., "adjusted_PRS"),
    old_risk = add_risk(., "original_PRS")) %>%
  select(-IID)

for_age <-
  data %>%
  select(diagnosis, age)

age_fem <-
  data %>%
  filter(sex == "Female") %>%
  select(diagnosis, age) %>%
  rename(fem_age = age)

age_man <-
  data %>%
  filter(sex == "Male") %>%
  select(diagnosis, age) %>%
  rename(man_age = age)

## For time comparison
W0 <-
  data %>%
  filter(wave == "W0") %>%
  select(-wave) %>%
  mutate(
    new_risk = add_risk(., "adjusted_PRS"),
    old_risk = add_risk(., "original_PRS")) %>%
  select(-IID)

W1 <-
  data %>%
  filter(wave == "W1") %>%
  select(-wave) %>%
  mutate(
    new_risk = add_risk(., "adjusted_PRS"),
    old_risk = add_risk(., "original_PRS")) %>%
  select(-IID)

W2 <-
  data %>%
  filter(wave == "W2") %>%
  select(-wave) %>%
  mutate(
    new_risk = add_risk(., "adjusted_PRS"),
    old_risk = add_risk(., "original_PRS")) %>%
  select(-IID)

## Get Odds Ratios into object
all_time_model <-
  uni_model_cal(no_duplicate) %>%
  bind_rows(
    uni_model_cal(for_age),
    uni_model_cal(age_fem),
    uni_model_cal(age_man))
W0_model <- uni_model_cal(W0)
W1_model <- uni_model_cal(W1)
W2_model <- uni_model_cal(W2)

## Forest Plot
mod <-
  all_time_model %>%
  edit() %>%
  filter(term != "(Intercept)" & term != "x")
# saveRDS(mod, "cass_BHRC_OR_univariated_model.R")
# mod <- readRDS("cass_BHRC_OR_univariated_model.R")
# seria legal colocar a idade separada por sexo também
# mod$term <-
#   factor(mod$term,
#   levels =
#     c("Female", "Male", "Rio Grande do Sul", "São Paulo",
#     "PRS adjusted", "PRS original", "Parents' diagnosis", "Age"))

# mod <- with(mod, mod[order(term),])

library(stringr)
library(forcats)

mod2 <-
  mod %>%
  mutate(
  across(c(estimate, conf.low, conf.high),
    ~str_pad(
    round(.x, 2),
    width = 4,
    pad = "0",
    side = "right")),
    OR_CI = paste(estimate, " (", conf.low, "-", conf.high, ")", sep = "")) %>%
  mutate(p.value = case_when(
    p.value < .001 ~ "<0.001",
    round(p.value, 2) == .05 ~ as.character(round(p.value, 3)),
    p.value < .01 ~ str_pad(
    as.character(round(p.value, 3)),
    width = 4,
    pad = "0",
    side = "right"),
    TRUE ~ str_pad(
    as.character(round(p.value, 2)),
    width = 4,
    pad = "0",
    side = "right"))) %>%
  bind_rows(
    data.frame(
    term = "Variable",
    OR_CI = "Hazard Ratio (95% CI)",
    conf.low = "",
    conf.high = "",
    p.value = "p-value"))

middle <-
  ggplot(mod, aes(estimate, term)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), linewidth = 1, alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, color = "#3a3a3a") +
  geom_point(shape = 15, size = 3, color = "#013464") +
  labs(x = "Odds Ratio [95% CI]", y = "") +
  coord_cartesian(ylim = c(1, 11), xlim = c(0, 4.5)) +
  theme_publish() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.5, color = "black"),
    axis.title.x = element_text(face = "bold"))

left <-
  ggplot(mod2, aes(x = 0, label = term, y = term)) +
  geom_text(
    aes(x = 0, label = term), hjust = 0,
    fontface = ifelse(mod2$term == "Variable", "bold", "plain")) +
  coord_cartesian(xlim = c(0, 4)) +
  theme_void()

right <-
  ggplot(mod2) +
  geom_text(
    aes(x = 0, y = term, label = p.value), hjust = 0,
    fontface = ifelse(mod2$p.value == "p-value", "bold", "plain")) +
  theme_void()

library(patchwork)

layout <- c(
  area(t = 0, l = 0, b = 30, r = 8), # left plot
  area(t = 1, l = 5, b = 30, r = 11), # middle plot
  area(t = 0, l = 11, b = 30, r = 12) # right plot
)
left + middle + right + plot_layout(design = layout)

ggsave("forest-plot2.png", units = "mm", width = 160, height = 175)

# ruler <-
#   ggplot(mod3, aes(y = term)) +
#   theme_publish() +
#   theme(
#     axis.text.y = element_text(size = 7, color = "black", face = "bold"),
#     axis.line.y = element_blank(),
#     axis.ticks.y = element_line(linewidth = 1, color = "black"),
#     axis.title.y = element_blank()
#   )

## Get Odds Ratio into a table
# all_time_OR <- uni_model_OR(no_duplicate)
# W0_OR <- uni_model_OR(W0)
# W1_OR <- uni_model_OR(W1)
# W2_OR <- uni_model_OR(W2)

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
# # knitr::kable()