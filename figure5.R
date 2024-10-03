source("functions_to_source.R")

## PC plots

vADHD <-
prs_v3 %>%
select(IID, ADHD) %>%
rename(PRS = ADHD) %>%
mutate(
  quintile = as.factor(ntile(PRS, 5)),
  quintile = case_when(
    quintile == 1 ~ "1st",
    quintile == 2 ~ "2nd",
    quintile == 3 ~ "3rd",
    quintile == 4 ~ "4th",
    quintile == 5 ~ "5th",
  )) %>%
select(-PRS)

## working data
data <-
plyr::join_all(
  list(vADHD, pc, sex),
  by = "IID", type = "inner"
) 
ggthemr("earth")
ggplot(data, aes(PC1, PC2, color = quintile, shape = sex)) +
geom_point(size = 2, alpha = 0.5) +
theme_publish() +
labs(
  color = "PRS Quintile",
  shape = "Sex") +
  theme(
      axis.title.y = element_text(size = 7, color = "white"),
      axis.title.x = element_text(size = 7, color = "white"),
      axis.text = element_text(size = 7, color = "white"),
      axis.line.y = element_line(color = "white", linewidth = 0.5),
      axis.line.x = element_line(color = "white", linewidth = 0.5),
      axis.ticks.y = element_line(color = "white", linewidth = 0.5),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
  )
# theme(
#   text = element_text(size = 7),
#   axis.text = element_text(size = 7),
#   axis.line.x = element_line(color = "black", linewidth = 0.5),
#   axis.line.y = element_line(color = "black", linewidth = 0.5),
#   legend.position = "right",
#   legend.box = "vertical")

ggsave(
  "Figure5_version1.png", device = "png",
  width = 100, height = 100, units = c("mm"),
  dpi = 300, bg = "transparent")
