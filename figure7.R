source("data_to_source.R")
source("functions_to_source.R")

prs_og <-
  fread(glue("{Path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
  select(IID, PRSCS) %>%
  filter(IID %in% data$IID) %>%
  rename(original_PRS = 2)

data <- rename(data, zscored_PRS = original_PRS)

ggtherm("fresh")

p1 <-
  prs_og %>%
  ggplot(aes(sample = original_PRS)) +
    geom_qq_line(linetype = "dashed", linewidth = 0.5) +
    geom_qq(color = "blue", size = 0.6, alpha = 0.8) +
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
  ggplot(aes(sample = zscored_PRS)) +
    geom_qq_line(linetype = "dashed", linewidth = 0.5) +
    geom_qq(color = "blue", size = 0.6, alpha = 0.8) +
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
p3 <-
  data %>%
  filter(wave == "W0") %>%
  ggplot(aes(sample = adjusted_PRS)) +
    geom_qq_line(linetype = "dashed", linewidth = 0.5) +
    geom_qq(color = "blue", size = 0.6, alpha = 0.8) +
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

library(patchwork)
(p1 / p2 / p3) +
plot_annotation(tag_levels = 'A')

ggsave(
  "figure7.png", device = "png", dpi = 300,
  width = 85, height = 170, units = "mm", bg = "white")
