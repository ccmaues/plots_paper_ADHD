source("data_to_source.R")
source("functions_to_source.R")


ggtherm("fresh")

p1 <-
  data %>%
  filter(wave == "W0") %>%
  ggplot(aes(sample = adjusted_PRS)) +
    geom_qq_line(linetype = "dashed", linewidth = 0.5) +
    geom_qq(color = "blue") +
    theme_publish() +
    theme(
      text = element_text(family = "Arial", color = "black"),
      axis.line.x = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.line.y = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.y = element_line(linewidth = 0.5, color = "black"),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_blank()
    )

p2 <-
  data %>%
  filter(wave == "W0") %>%
  ggplot(aes(sample = original_PRS)) +
    geom_qq_line(linetype = "dashed", linewidth = 0.5) +
    geom_qq(color = "blue") +
    theme_publish() +
    theme(
      text = element_text(family = "Arial", color = "black"),
      axis.line.x = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_blank(),
      axis.line.y = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.y = element_line(linewidth = 0.5, color = "black"),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_blank()
    )

library(patchwork)
library(ggpubr)

ggarrange(
  p1, p2, nrow = 2, labels = c("A", "B"),
  font.label = list(size = 10, color = "black", face = "bold", family = "Arial"),
  common.legend = FALSE)
  
ggsave(
  "figure7_version1.png", device = "png", dpi = 300,
  width = 85, height = 170, units = "mm", bg = "white")
