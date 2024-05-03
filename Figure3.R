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
  list(pADHD, aADHD),
  by = c("wave", "IID"), type = "inner") %>%
rename(diagnosis = ADHD) %>%
inner_join(., select(prs_v2, IID, ADHD), by = "IID") %>%
select(-diagnosis, -ADHD, -IID)

pacman::p_load(RColorBrewer, raincloudplots)

ggthemr("fresh")

final <-
ggplot(data, aes(x = wave, y = age, fill = wave)) +
ggdist::stat_halfeye(
  adjust = 0.5, width = 0.6, .width = 0,
  justification = -0.2, point_colour = NA, alpha = 0.9) +
geom_boxplot(
  width = 0.15, outlier.shape = 19, outlier.size = 1,
  color = "black", position = position_dodge(width = 0.75),
  coef = 1, notch = FALSE, alpha = 0.9, linewidth = 0.3) +
gghalves::geom_half_point(
  size = 1, side = "l", range_scale = 0.4,
  alpha = 0.1, shape = 21) +
scale_y_continuous(n.breaks = 10) +
scale_x_discrete(expand = expansion(mult = c(0, 0))) +
coord_flip() +
labs(y = "Age (yr)\n") +
theme(
  text = element_text(family = "Arial", size = 7),
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  axis.text = element_text(color = "black"),
  legend.position = "none",
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.line.x = element_line(color = "black", linewidth = 0.2),
  axis.ticks.x = element_line(color = "black",  linewidth = 0.2),
  axis.title.x = element_text(color = "black"))

final

ggsave(
  "Figure3.png", final, device = "png",
  width = 85, height = 100, units = c("mm"),
  dpi = 300, bg = "white")
