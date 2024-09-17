setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

data <- inner_join(data, pc, by = "IID") %>%
  filter(sex == "Male")

data_W0 <- filter(data, wave == "W0")

data_W1 <- filter(data, wave == "W1")

data_W2 <- filter(data, wave == "W2")

### Get individual R2 for diagnosis
## P_ALL
calculate_explained_var <- function(data_frame) {
  if (any(table(data_frame$wave) == 0)) {
    m1 <- DescTools::PseudoR2(glm(diagnosis ~ adjusted_PRS, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m2 <- DescTools::PseudoR2(glm(diagnosis ~ popID, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m3 <- DescTools::PseudoR2(glm(diagnosis ~ age, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m4 <- DescTools::PseudoR2(glm(diagnosis ~ p_diagnosis, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m5 <- DescTools::PseudoR2(glm(diagnosis ~ PC1, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m6 <- DescTools::PseudoR2(glm(diagnosis ~ PC2, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m7 <- DescTools::PseudoR2(glm(diagnosis ~ PC3, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m8 <- DescTools::PseudoR2(glm(diagnosis ~ PC4, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m9 <- DescTools::PseudoR2(glm(diagnosis ~ PC5, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m10 <- DescTools::PseudoR2(glm(diagnosis ~ PC6, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m11 <- DescTools::PseudoR2(glm(diagnosis ~ PC7, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m12 <- DescTools::PseudoR2(glm(diagnosis ~ PC8, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m13 <- DescTools::PseudoR2(glm(diagnosis ~ PC9, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m14 <- DescTools::PseudoR2(glm(diagnosis ~ PC10, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m15 <- 0  # Set m16 to 0 or some value, or remove it if unnecessary

  } else {
    m1 <- DescTools::PseudoR2(glm(diagnosis ~ adjusted_PRS, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m2 <- DescTools::PseudoR2(glm(diagnosis ~ popID, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m3 <- DescTools::PseudoR2(glm(diagnosis ~ wave, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m4 <- DescTools::PseudoR2(glm(diagnosis ~ age, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m5 <- DescTools::PseudoR2(glm(diagnosis ~ p_diagnosis, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m6 <- DescTools::PseudoR2(glm(diagnosis ~ PC1, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m7 <- DescTools::PseudoR2(glm(diagnosis ~ PC2, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m8 <- DescTools::PseudoR2(glm(diagnosis ~ PC3, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m9 <- DescTools::PseudoR2(glm(diagnosis ~ PC4, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m10 <- DescTools::PseudoR2(glm(diagnosis ~ PC5, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m11 <- DescTools::PseudoR2(glm(diagnosis ~ PC6, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m12 <- DescTools::PseudoR2(glm(diagnosis ~ PC7, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m13 <- DescTools::PseudoR2(glm(diagnosis ~ PC8, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m14 <- DescTools::PseudoR2(glm(diagnosis ~ PC9, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
    m15 <- DescTools::PseudoR2(glm(diagnosis ~ PC10, data = data_frame, family = "binomial"), which = "Nagelkerke") * 100
  }

  for_plot <- data.frame(
    Variables = c(
      "PRS", "Site", "Wave", "Age", "Parent\nDiagnosis", 
      "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"
    ),
    Explained_variance = c(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)
  ) %>%
  mutate(Variables = factor(Variables, levels = Variables[order(Explained_variance, decreasing = TRUE)]))

  return(for_plot)
}

fp_all <- calculate_explained_var(data)
fp_w0 <- calculate_explained_var(data_W0)
fp_w1 <- calculate_explained_var(data_W1)
fp_w2 <- calculate_explained_var(data_W2)

colors_and_tags <- c(
  Site = "#1f77b4", Wave = "#2ca02c",
	PRS = "#d62728", Age = "#9467bd", 'Parent\nDiagnosis' = "#8c564b",
	PC1 = "#e377c2", PC2 = "#7f7f7f", PC3 = "#bcbd22",
	PC4 = "#17becf", PC5 = "#ffbb78", PC6 = "#98df8a",
	PC7 = "#ff9896", PC8 = "#c5b0d5", PC9 = "#c49c94",
  PC10 = "#f7b6d2")

p_all <- ggplot(fp_all, aes(x = Variables, y = Explained_variance, fill = Variables)) +
  geom_col() +
  scale_y_continuous(n.breaks = 12) +
  labs(
    y = "\n% of diagnosis explained\n",
    x = "") +
  scale_fill_manual(values = colors_and_tags) +
	scale_y_continuous(limits = c(0, 2), n.breaks = 10) +
  theme_publish() +
  theme(
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

p_w0 <- ggplot(fp_w0, aes(x = Variables, y = Explained_variance, fill = Variables)) +
  geom_col() +
  scale_y_continuous(n.breaks = 12) +
  labs(
    y = "\n% of diagnosis explained\n",
    x = "") +
  scale_fill_manual(values = colors_and_tags) +
	scale_y_continuous(limits = c(0, 2), n.breaks = 10) +
  theme_publish() +
  theme(
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

p_w1 <- ggplot(fp_w1, aes(x = Variables, y = Explained_variance, fill = Variables)) +
  geom_col() +
  scale_y_continuous(n.breaks = 12) +
  labs(
    y = "\n% of diagnosis explained\n",
    x = "") +
  scale_fill_manual(values = colors_and_tags) +
	scale_y_continuous(limits = c(0, 2), n.breaks = 10) +
  theme_publish() +
  theme(
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

p_w2 <- ggplot(fp_w2, aes(x = Variables, y = Explained_variance, fill = Variables)) +
  geom_col() +
  scale_y_continuous(n.breaks = 12) +
  labs(
    y = "\n% of diagnosis explained\n",
    x = "") +
  scale_fill_manual(values = colors_and_tags) +
	scale_y_continuous(limits = c(0, 2), n.breaks = 10) +
  theme_publish() +
  theme(
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

library(patchwork)
(p_all / p_w0 / p_w1 / p_w2) +
plot_annotation(tag_levels = 'A')

# fazer por onda e colocar os pcs no cálculo
ggsave(
  "figure14.png",
  height = 200,
  width = 180,
  units = "mm",
  device = "png",
  bg = "white"
  )
