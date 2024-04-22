source("functions_to_source.R")

# Phenotype data
pADHD <-
  select(pheno, IID, wave, dcanyhk) %>%
  rename(ADHD = dcanyhk)

# Age data
aADHD <-
tidyr::pivot_longer(
  ages,
  cols = starts_with("age_W"),
  names_to = "wave",
  values_to = "age"
)
aADHD$wave <- gsub("age_W", "W", aADHD$wave)

## Working data
# Overall
data <-
plyr::join_all(
  list(pADHD,  aADHD),
  by = c("wave", "IID"), type = "inner") %>%
mutate(age = round(age, digits = 0)) %>%
rename(diagnosis = ADHD)

## Data preparation for plot (all ages)
calc_prev_by_age <- function(data, wave, new_column_name) {
  data %>%
  filter(wave == {{wave}}) %>%
  group_by(diagnosis, age) %>%
  summarise(freq = n()) %>%
  spread(age, freq, fill = 0) %>%
  t() %>%
  as.data.frame() %>%
  slice(-1) %>%
  tibble::rownames_to_column(var = "age_diagnosis") %>%
  rename(age = 1, control = 2, case = 3) %>%
  mutate(prev = case / (control + case)) %>%
  filter(control >= 20) %>%
  select(-control, -case) %>%
  rename({{new_column_name}} := 2)
}

## Overall data
# W0 data
w0_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W0", "Overall"),
  calc_prev_by_age(data_female, "W0", "Female"),
  calc_prev_by_age(data_male, "W0", "Male")),
  by = "age", type = "inner") %>%
  mutate(age = as.numeric(age))

# W1 data
w1_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W1", "Overall"),
  calc_prev_by_age(data_female, "W1", "Female"),
  calc_prev_by_age(data_male, "W1", "Male")),
  by = "age", type = "inner") %>%
  mutate(age = as.numeric(age))

# W2 data
w2_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W2", "Overall"),
  calc_prev_by_age(data_female, "W2", "Female"),
  calc_prev_by_age(data_male, "W2", "Male")),
  by = "age", type = "inner") %>%
  mutate(age = as.numeric(age))

# Data plotting
## Overall
w0 <- ggplot(w0_for_plot, aes(x = age, y = Overall * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_point(size = 2, color = "#65ADC2") +
    scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\n", x = "") +
  theme_publish()
w1 <- ggplot(w1_for_plot, aes(x = age, y = Overall * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4, color = "#233B43") +
  geom_point(size = 2, color = "#233B43") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\nPrevalence %", x = "") +
  theme_publish()
w2 <- ggplot(w2_for_plot, aes(x = age, y = Overall * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4, color = "#E84646") +
  geom_point(size = 2, color = "#E84646") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\n", x = "Age (yr)\n") +
  theme_publish()

library(ggpubr)

final <- ggarrange(
  w0, w1, w2, nrow = 3, labels = c("A", "B", "C"),
  font.label = list(size = 7, color = "black", face = "bold", family = "Arial"))

ggsave(
  "Figure2.png", final, device = "png",
  width = 85, height = 150, units = c("mm"),
  dpi = 300, bg = "white")

## Sex
# Female plots
w0_sex_female <-
ggplot(w0_for_plot, aes(x = age, y = Female * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_point(size = 2, color = "#65ADC2") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\n", x = "") +
  theme_publish()

w1_sex_female <-
ggplot(w1_for_plot, aes(x = age, y = Female * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4, color = "#233B43") +
  geom_point(size = 2, color = "#233B43") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\nPrevalence %", x = "") +
  theme_publish()

w2_sex_female <-
ggplot(w2_for_plot, aes(x = age, y = Female * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4, color = "#E84646") +
  geom_point(size = 2, color = "#E84646") +
  scale_x_continuous(n.breaks = 8) +
    scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\n", x = "Age (yr)\n") +
  theme_publish()

final <- ggarrange(
  w0_sex_female, w1_sex_female, w2_sex_female, nrow = 3, labels = c("A", "B", "C"),
  font.label = list(size = 7, color = "black", face = "bold", family = "Arial"))

ggsave(
  "Figure2.2.png", final, device = "png",
  width = 85, height = 150, units = c("mm"),
  dpi = 300, bg = "white")

# Male plots
w0_sex_male <-
ggplot(w0_for_plot, aes(x = age, y = Male * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_point(size = 2, color = "#65ADC2") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\n", x = "") +
  theme_publish()
w0_sex_female

w1_sex_male <-
ggplot(w1_for_plot, aes(x = age, y = Male * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4, color = "#233B43") +
  geom_point(size = 2, color = "#233B43") +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\nPrevalence %", x = "") +
  theme_publish()

w2_sex_male <-
ggplot(w2_for_plot, aes(x = age, y = Male * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4, color = "#E84646") +
  geom_point(size = 2, color = "#E84646") +
  scale_x_continuous(n.breaks = 8) +
    scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\n", x = "Age (yr)\n") +
  theme_publish()

final <- ggarrange(
  w0_sex_male, w1_sex_male, w2_sex_male, nrow = 3, labels = c("A", "B", "C"),
  font.label = list(size = 7, color = "black", face = "bold", family = "Arial"))

ggsave(
  "Figure2.3.png", final, device = "png",
  width = 85, height = 150, units = c("mm"),
  dpi = 300, bg = "white")

colors <- c("#65ADC2", "#233B43", "#E84646")
