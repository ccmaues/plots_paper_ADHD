source("functions_to_source.R")
source("data_to_source.R")

pacman::p_load(RColorBrewer, raincloudplots)

ggthemr("fresh")

final <-
ggplot(data, aes(x = wave, y = age, fill = wave)) +
ggdist::stat_halfeye(
  adjust = 0.5, width = 0.6, .width = 0,
  justification = -0.2, point_colour = NA, alpha = 0.9) +
geom_boxplot(
  width = 0.15, outlier.shape = 19, outlier.size = 1,
  color = "white", position = position_dodge(width = 0.75),
  coef = 1, notch = FALSE, alpha = 0.9, linewidth = 0.3) +
gghalves::geom_half_point(
  size = 1, side = "l", range_scale = 0.4,
  alpha = 0.1, shape = 21) +
scale_y_continuous(n.breaks = 10) +
scale_x_discrete(expand = expansion(mult = c(0, 0))) +
coord_flip() +
labs(y = "Age (yr)\n") +
theme_minimal() +
theme(
  text = element_text(family = "Arial", size = 7),
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  axis.text = element_text(color = "white"),
  legend.position = "none",
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.line.x = element_line(color = "white", linewidth = 0.5),
  axis.ticks.x = element_line(color = "white",  linewidth = 0.5),
  axis.title.x = element_text(color = "white"),
  panel.background = element_rect(fill = "transparent", color = NA),
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.grid = element_blank()) 
# ggsave(
#   "Figure3.png", final, device = "png",
#   width = 85, height = 100, units = c("mm"),
#   dpi = 300, bg = "white")

ggsave(
  "Figure3_v2.png", final, device = "png",
  width = 150, height = 150, units = "mm",
  dpi = 300, bg = NULL
)
