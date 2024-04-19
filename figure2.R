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

# Female subset
data_female <-
  inner_join(data, sex, by = "IID") %>%
  filter(sex == "Female") %>%
  mutate(age = round(age, digits = 0)) %>%
  select(-sex)

# Male subset
data_male <-
  inner_join(data, sex, by = "IID") %>%
  filter(sex == "Male") %>%
  mutate(age = round(age, digits = 0)) %>%
  select(-sex)

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

# W0 data
w0_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W0", "Overall"),
  calc_prev_by_age(data_female, "W0", "Female"),
  calc_prev_by_age(data_male, "W0", "Male")),
  by = "age", type = "inner") %>%
  mutate(age = as.numeric(age))

# Female data
w1_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W1", "Overall"),
  calc_prev_by_age(data_female, "W1", "Female"),
  calc_prev_by_age(data_male, "W1", "Male")),
  by = "age", type = "inner") %>%
  mutate(age = as.numeric(age))

# Male data
w2_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W2", "Overall"),
  calc_prev_by_age(data_female, "W2", "Female"),
  calc_prev_by_age(data_male, "W2", "Male")),
  by = "age", type = "inner") %>%
  mutate(age = as.numeric(age))

# a minha pergunta é: o nrow que eu uso é o
# antigo ou o novo de tirar coisas sem 20 hits?
stat_overall_w0 <- c(
  mean = mean(w0_for_plot$Overall), sd = sd(w0_for_plot$Overall),
  se = sd(w0_for_plot$Overall) / sqrt(nrow(w0_for_plot)),
  lower = t.test(w0_for_plot$Overall)$conf.int[1],
  upper = t.test(w0_for_plot$Overall)$conf.int[2]
  )

stat_overall_w1 <- c(
  mean = mean(w1_for_plot$Overall), sd = sd(w1_for_plot$Overall),
  se = sd(w1_for_plot$Overall) / sqrt(nrow(w1_for_plot)),
  lower = t.test(w1_for_plot$Overall)$conf.int[1],
  upper = t.test(w1_for_plot$Overall)$conf.int[2]
  )

stat_overall_w2 <- c(
  mean = mean(w2_for_plot$Overall), sd = sd(w2_for_plot$Overall),
  se = sd(w2_for_plot$Overall) / sqrt(nrow(w2_for_plot)),
  lower = t.test(w2_for_plot$Overall)$conf.int[1],
  upper = t.test(w2_for_plot$Overall)$conf.int[2]
  )

# Data plotting
w0 <- ggplot(w0_for_plot, aes(x = age, y = Overall * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff0000ef", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 5, color = "#65ADC2") +
  geom_errorbar(aes(
    x = age, ymin = (Overall * 100) - stat_overall_w0[4],
    ymax = (Overall * 100) + stat_overall_w0[5]),
    position = position_dodge(0.05),
    width = 0.3, linewidth = 2,
    alpha = 0.6, color = "#65ADC2") +
    scale_x_continuous(n.breaks = 8) +
  #scale_y_continuous(n.breaks = 10, limits = c(10, 20)) +
  theme_publish()
w1 <- ggplot(w1_for_plot, aes(x = age, y = Overall * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff0000ef", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1.5, alpha = 0.6, color = "#233B43") +
  geom_point(size = 5, color = "#233B43") +
  geom_errorbar(aes(
    x = age, ymin = (Overall * 100) - stat_overall_w1[4],
    ymax = (Overall * 100) + stat_overall_w1[5]),
    position = position_dodge(0.05),
    width = 0.3, linewidth = 2,
    alpha = 0.6, color = "#233B43") +
    scale_x_continuous(n.breaks = 8) +
  #scale_y_continuous(n.breaks = 10, limits = c(10, 20)) +
  theme_publish()
w2 <- ggplot(w2_for_plot, aes(x = age, y = Overall * 100)) +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    color = "#ff0000ef", linetype = "dashed", linewidth = 1) +
  geom_line(linewidth = 1.5, alpha = 0.6, color = "#E84646") +
  geom_point(size = 5, color = "#E84646") +
  geom_errorbar(aes(
    x = age, ymin = (Overall * 100) - stat_overall_w2[4],
    ymax = (Overall * 100) + stat_overall_w2[5]),
    position = position_dodge(0.05),
    width = 0.3, linewidth = 2,
    alpha = 0.6, color = "#E84646") +
    scale_x_continuous(n.breaks = 8) +
  #scale_y_continuous(n.breaks = 10, limits = c(10, 20)) +
  theme_publish()
w2
colors <- c("#65ADC2", "#233B43", "#E84646")

library(ggpubr)

final <- ggarrange(
  w0, w1, w2, nrow = 3, labels = c("A", "B", "C"),
  font.label = list(size = 7, color = "black", face = "bold", family = "Arial")
)
final
