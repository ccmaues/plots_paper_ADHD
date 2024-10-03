setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

# Working data
data <- data %>%
  select(IID, adjusted_PRS, sex, wave, diagnosis) %>%
  rename(PRS = 2) %>%
  tidyr::pivot_wider(
    names_from = "wave",
    values_from = "diagnosis")

data_female <- data %>%
  filter(sex == "Female") %>%
  select(-sex)

data_male <- data %>%
  filter(sex == "Male") %>%
  select(-sex)

# Prevalence calculation (Overall)
p1 <-
  calc_prev(data, 5, "PRS", "W0") %>%
  rename(W0 = 2)

p2 <-
  calc_prev(data, 5, "PRS", "W1") %>%
  rename(W1 = 2)

p3 <-
  calc_prev(data, 5, "PRS", "W2") %>%
  rename(W2 = 2)

# Prevalence calculation (Female)
p1_fem <-
  calc_prev(data_female, 5, "PRS", "W0") %>%
  rename(W0 = 2)

p2_fem <-
  calc_prev(data_female, 5, "PRS", "W1") %>%
  rename(W1 = 2)

p3_fem <-
  calc_prev(data_female, 5, "PRS", "W2") %>%
  rename(W2 = 2)

# Prevalence calculation (Male)
p1_male <-
calc_prev(data_male, 5, "PRS", "W0") %>%
rename(W0 = 2)

p2_male <-
calc_prev(data_male, 5, "PRS", "W1") %>%
rename(W1 = 2)

p3_male <-
calc_prev(data_male, 5, "PRS", "W2") %>%
rename(W2 = 2)

# Plot data preparation
for_plot_overall <-
plyr::join_all(
  list(p1, p2, p3),
  by = "ntile",
  type = "inner") %>%
tidyr::pivot_longer(
  cols = starts_with("W"),
  values_to = "prevalence",
  names_to = "wave"
  ) %>%
mutate(gp = "Overall")

for_plot_female <-
plyr::join_all(
  list(p1_fem, p2_fem, p3_fem),
  by = "ntile",
  type = "inner") %>%
tidyr::pivot_longer(
  cols = starts_with("W"),
  values_to = "prevalence",
  names_to = "wave"
  ) %>%
mutate(gp = "Female")

for_plot_male <-
plyr::join_all(
  list(p1_male, p2_male, p3_male),
  by = "ntile",
  type = "inner") %>%
tidyr::pivot_longer(
  cols = starts_with("W"),
  values_to = "prevalence",
  names_to = "wave"
  ) %>%
mutate(gp = "Male")

for_plot_sex <- bind_rows(for_plot_female, for_plot_male)

new_x_axis <- c("1st", "2nd", "3rd", "4th", "5th")
ggthemr("fresh")

all_plots <- bind_rows(for_plot_female, for_plot_male, for_plot_overall) %>%
mutate(tags = case_when(
  gp == "Female" ~ "C",
  gp == "Male" ~ "B",
  gp == "Overall" ~ "A"
))
all_plots$tags <- factor(all_plots$tags, levels = c("A", "B", "C"))
finalv2 <-
ggplot(for_plot_male, aes(ntile, prevalence * 100, color = wave, group = wave)) +
  geom_line(linetype = "dashed", alpha = 0.6) +
  geom_point() +
  scale_x_discrete(labels = new_x_axis) +
  scale_y_continuous(n.breaks = 10, limits = c(5, 24)) +
  theme_publish(base_size = 7) +
  labs(y = "\nPrevalence\n", x = "PRS quintile") +
  theme(
    text = element_text(family = "Arial"),
    axis.line = element_line(linewidth = 1),
    legend.position = "top",
    legend.title = element_blank(),
    strip.text.x = element_text(color = "black", face = "bold", vjust = 2))
  #facet_wrap( ~ tags, scales = "free_y", nrow = 3)
  
finalv2

# ggsave(
#   "Figure1_only_males.png", finalv2, device = "png",
#   width = 100, height = 60, units = c("mm"),
#   dpi = 300, bg = "white"
# )

opt <- finalv2 +
theme(
  legend.text = element_text(color = "white"),
  axis.line.x = element_line(color = "white"),
  axis.ticks.x = element_line(color = "white"),
  axis.text.x = element_text(color = "white"),
  axis.title.x = element_text(color = "white"),
  axis.line.y = element_line(color = "white"),
  axis.ticks.y = element_line(color = "white"),
  axis.text.y = element_text(color = "white"),
  axis.title.y = element_text(color = "white")
  )

ggsave(
  "Figure1_v2_male.png", opt, device = "png",
  width = 120, height = 60, units = c("mm"),
  dpi = 300
)
