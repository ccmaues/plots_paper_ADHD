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
  select(-control) %>% # keep only case N
  rename({{new_column_name}} := 3, N = case)
}

# Overall data
overall_for_plot <-
  rbind(
    cbind(calc_prev_by_age(data, "W2", "prevalence"), wave = "W2"),
    cbind(calc_prev_by_age(data, "W1", "prevalence"), wave = "W1"),
    cbind(calc_prev_by_age(data, "W0", "prevalence"), wave = "W0")) %>%
  mutate(age = as.numeric(age))

# female data
female_for_plot <-
  rbind(
    cbind(calc_prev_by_age(data_female, "W2", "prevalence"), wave = "W2"),
    cbind(calc_prev_by_age(data_female, "W1", "prevalence"), wave = "W1"),
    cbind(calc_prev_by_age(data_female, "W0", "prevalence"), wave = "W0")) %>%
  mutate(age = as.numeric(age))
  
# male data
male_for_plot <-
  rbind(
    cbind(calc_prev_by_age(data_male, "W2", "prevalence"), wave = "W2"),
    cbind(calc_prev_by_age(data_male, "W1", "prevalence"), wave = "W1"),
    cbind(calc_prev_by_age(data_male, "W0", "prevalence"), wave = "W0")) %>%
  mutate(age = as.numeric(age))

# não dá pra fazer assim
# pq tem idade de se repete em cada wave
# então seria bom colocar a wave aqui
# como color e group sex

ggthemr("fresh")
library(ggnewscale)

# Data plotting
## Overall (gotta put size = N)
p1 <-
ggplot(overall_for_plot, aes(x = age, y = prevalence * 100, color = wave, group = wave)) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_point(aes(size = N)) +
  new_scale_color() +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    linetype = "dashed", linewidth = 1, aes(color = wave)) +
  scale_color_manual(values = c("#65acc27b", "#233b436f", "#e8464668")) +
  scale_x_continuous(n.breaks = 24) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\n", x = "") +
  theme_publish() +
  theme(
    text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 0.5)
  )

## Female
p2 <-
ggplot(female_for_plot, aes(x = age, y = prevalence * 100, color = wave, group = wave)) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_point(aes(size = N)) +
  new_scale_color() +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    linetype = "dashed", linewidth = 1, aes(color = wave)) +
  scale_color_manual(values = c("#65acc27b", "#233b436f", "#e8464668")) +
  scale_x_continuous(n.breaks = 24) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\nPrevalence", x = "") +
  theme_publish() +
  theme(
    text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 0.5)
  )

## Male
p3 <-
ggplot(male_for_plot, aes(x = age, y = prevalence * 100, color = wave, group = wave)) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_point(aes(size = N)) +
  new_scale_color() +
  stat_smooth(
    method = "lm", formula = y ~ x, geom = "smooth", se = FALSE,
    linetype = "dashed", linewidth = 1, aes(color = wave)) +
  scale_color_manual(values = c("#65acc27b", "#233b436f", "#e8464668")) +
  scale_x_continuous(n.breaks = 24) +
  scale_y_continuous(n.breaks = 8, limits = c(0, 24)) +
  labs(y = "\n", x = "Age (yr)") +
  theme_publish() +
  theme(
    text = element_text(size = 15, family = "Arial"),
    axis.text = element_text(size = 15),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5)
    )

library(ggpubr)

final <-
  ggarrange(
    p1, p2, p3, nrow = 3, labels = c("A", "B", "C"),
    font.label = list(size = 15, color = "black", face = "bold", family = "Arial"),
    common.legend = TRUE, widths = 1.5, heights = 1
  )

# ggsave(
#   "Figure2.png", final, device = "png",
#   width = 200, height = 300, units = c("mm"),
#   dpi = 300, bg = "white")

colors <- c("#65ADC2", "#233B43", "#E84646")

opt1 <- p2 +
  labs(x = "Age (yr)") +
  theme(
    legend.text = element_text(color = "white", family = "Arial", size = 10),
    axis.line.x = element_line(color = "white"),
    axis.ticks.x = element_line(color = "white"),
    axis.text.x = element_text(color = "white", family = "Arial", size = 10),
    axis.title.x = element_text(color = "white", family = "Arial", size = 10),
    axis.line.y = element_line(color = "white"),
    axis.ticks.y = element_line(color = "white"),
    axis.text.y = element_text(color = "white", family = "Arial", size = 10),
    axis.title.y = element_text(color = "white", family = "Arial", size = 10)
  )

ggsave(
  "Figure2_v2_female.png", opt1, device = "png",
  width = 200, height = 100, units = c("mm"),
  dpi = 300
)
