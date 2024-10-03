data <- readRDS("E:/objects_R/cass_BHRC_26092024.RDS")

pacman::p_load(
  dplyr, ggplot2, remotes,
  tidyr, envalysis, ggthemr,
  patchwork
)

t <- select(data, !IID)

# Distribution of variables
ggthemr("fresh")
p_prs <-
  ggplot(t, aes(PRS)) +
  geom_histogram(color = "black") +
  scale_y_continuous(n.breaks = 25) +
  coord_cartesian(ylim = c(0, 650)) +
  theme_publish() +
  theme(
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  )

ggplot(t, aes(PRS)) +
  geom_boxplot() +
  theme_publish()
# depois
ntile(PRS, 100)

p_prs2 <-
  ggplot(t, aes(sample = PRS)) +
  geom_qq(color = "black") +
  geom_qq_line(alpha = 0.5, color = "red") +
  theme_publish() +
  theme(
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  )

p_h <-
  ggplot(t, aes(height)) +
  geom_histogram(color = "black") +
  scale_y_continuous(n.breaks = 25) +
  theme_publish() +
  theme(
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  )

p_h2 <-
  ggplot(t, aes(sample = height)) +
  geom_qq(color = "black") +
  geom_qq_line(alpha = 0.5, color = "red") +
  theme_publish() +
  theme(
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  )

opt <- (p_prs + p_prs2) / (p_h + p_h2)
# ggsave("prs_height_dist_BHRC.png", opt, device = "png", height = 30, width = 20, units = "cm")

# data de avaliação por wave
p_dw0 <-
  filter(t, wave == "W0", !is.na(site)) %>%
  ggplot(aes(x = d_date)) +
  geom_histogram(color = "black", binwidth = 30) +
  scale_x_date(
    date_breaks = "1 months",
    date_labels = "%b",
    limits = c(as.Date("2010-01-01"), as.Date("2011-12-31"))
  ) +
  scale_y_continuous(n.breaks = 25) +
  theme_publish() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  ) +
  facet_wrap(~year)

p_dw1 <-
  filter(t, !is.na(wave) & wave == "W1", !is.na(site)) %>%
  ggplot(aes(x = d_date)) +
  geom_histogram(color = "black", binwidth = 30) +
  scale_x_date(
    date_breaks = "1 months",
    date_labels = "%b",
    limits = c(as.Date("2013-01-01"), as.Date("2014-12-31"))
  ) +
  scale_y_continuous(n.breaks = 20) +
  theme_publish() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  ) +
  facet_wrap(~year)

p_dw2 <-
  filter(t, !is.na(wave) & wave == "W2", !is.na(site)) %>%
  ggplot(aes(x = d_date)) +
  geom_histogram(color = "black", binwidth = 30) +
  scale_x_date(
    date_breaks = "1 months",
    date_labels = "%b",
    limits = c(as.Date("2018-01-01"), as.Date("2019-12-31"))
  ) +
  scale_y_continuous(n.breaks = 15) +
  theme_publish() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  ) +
  facet_wrap(~year)

opt <- p_dw0 / p_dw1 / p_dw2
# ggsave("eval_date_dist_BHRC.png", opt, device = "png", height = 30, width = 30, units = "cm")

# preciso que todos comecem em jan
# colocar as porcentagens, quem sabe, na parte de cima
p_p <-
  filter(t, !is.na(wave) & wave %in% c("W0", "W1", "W2"), !is.na(site)) %>%
  ggplot(aes(x = ADHD, fill = factor(ADHD))) +
  geom_histogram(stat = "count", color = "black") +
  scale_y_continuous(n.breaks = 25) +
  scale_fill_manual(values = c("0" = "#2488a4", "2" = "#FF9513"), na.value = "grey") +
  theme_publish() +
  theme(
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  ) +
  facet_grid(wave ~ site)

# ggsave("pheno_ADHD_comp_BHRC.png", p_p, device = "png", height = 30, width = 30, units = "cm", bg = "white")

p_s <-
  ggplot(filter(t, wave == "W0"), aes(x = site, fill = factor(site))) +
  geom_histogram(stat = "count", color = "black") +
  scale_y_continuous(n.breaks = 25) +
  scale_fill_manual(values = c("SP" = "#2488a4", "RS" = "#FF9513"), na.value = "grey") +
  theme_publish() +
  theme(
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  ) +
  facet_wrap(gender ~ .)

# ggsave("sex_site_comp_BHRC.png", p_s, device = "png", height = 15, width = 15, units = "cm", bg = "white")
p_s1 <-
  filter(t, gender == "Female") %>%
  ggplot(aes(x = site, fill = factor(ADHD))) +
  geom_histogram(stat = "count", color = "black") +
  scale_y_continuous(n.breaks = 25) +
  scale_fill_manual(values = c("0" = "#2488a4", "2" = "#FF9513"), na.value = "grey") +
  labs(title = "Female") +
  theme_publish() +
  theme(
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  ) +
  facet_wrap(wave ~ .)

p_s2 <-
  filter(t, gender == "Male") %>%
  ggplot(aes(x = site, fill = factor(ADHD))) +
  geom_histogram(stat = "count", color = "black") +
  scale_y_continuous(n.breaks = 25) +
  labs(title = "Males") +
  scale_fill_manual(values = c("0" = "#2488a4", "2" = "#FF9513"), na.value = "grey") +
  theme_publish() +
  theme(
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      color = "#aaaaaa",
      linetype = "dashed"
    )
  ) +
  facet_wrap(wave ~ .)

opt <- p_s1 / p_s2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# ggsave("sex_site_diagnosis_comp_BHRC.png", opt, device = "png", height = 30, width = 15, units = "cm", bg = "white")
