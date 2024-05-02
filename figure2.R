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
rename(diagnosis = ADHD) %>%
inner_join(., sex, by = "IID")

# Female
data_female <-
filter(data, sex == "Female")

# Male
data_male <-
filter(data, sex == "Male")

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
overall_plot <-
  bind_rows(
  calc_prev_by_age(data, "W0", "Overall"),
  calc_prev_by_age(data, "W1", "Overall"),
  calc_prev_by_age(data, "W2", "Overall")) %>%
mutate(age = as.numeric(age))

## Female data
female_plot <-
  bind_rows(
  calc_prev_by_age(data_female, "W0", "Female"),
  calc_prev_by_age(data_female, "W1", "Female"),
  calc_prev_by_age(data_female, "W2", "Female")) %>%
  mutate(age = as.numeric(age))

# W2 data
male_plot <-
  bind_rows(
  calc_prev_by_age(data_male, "W0", "Male"),
  calc_prev_by_age(data_male, "W1", "Male"),
  calc_prev_by_age(data_male, "W2", "Male")) %>%
  mutate(age = as.numeric(age))

for_plot_sex <-
  do.call(cbind, c(female_plot, male_plot)) %>%
  data.frame() %>%
  select(-3) %>%
  tidyr::pivot_longer(
    cols = c("Female", "Male"),
    names_to = "gp",
    values_to = "prevalence"
  )

# Data plotting
## Overall
p1 <-
ggplot(overall_plot, aes(x = age, y = Overall * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff000060", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_point(size = 2) +
  scale_x_continuous(n.breaks = 24) +
  scale_y_continuous(n.breaks = 20, limits = c(0, 24)) +
  labs(y = "\n", x = "") +
  theme_publish()

## Sex plot
p2 <-
ggplot(for_plot_sex, aes(x = age, y = prevalence * 100, color = gp, group = gp)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    linetype = "dashed", linewidth = 1, aes(color = gp)) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_point(size = 2) +
  scale_x_continuous(n.breaks = 24) +
  scale_y_continuous(n.breaks = 20, limits = c(0, 24)) +
  labs(y = "\n", x = "") +
  theme_publish()

library(ggpubr)

final <-
  ggarrange(
    p1, p2, nrow = 2, labels = c("A", "B"),
    font.label = list(size = 7, color = "black", face = "bold", family = "Arial")
  )

ggsave(
  "Figure2_v2.png", final, device = "png",
  width = 85, height = 150, units = c("mm"),
  dpi = 300, bg = "white")

colors <- c("#65ADC2", "#233B43", "#E84646")
