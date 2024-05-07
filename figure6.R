source("functions_to_source.R")

## quintile composition plots

# PRS values
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

# Ancestry data
anc <-
  readRDS(glue("{Path}/objects_R/cass_BHRC_ADMIXTURE.RDS")) %>%
  select(-popID)

## working data
data <-
  plyr::join_all(
  list(vADHD, sex, state, anc),
  by = "IID", type = "inner") %>%
  filter(IID %in% ages$IID) %>%
  select(-IID) %>%
  mutate(
    popID = as.factor(case_when(
    popID == "BRA_SP" ~ "SP",
    popID == "BRA_RS" ~ "RS")))

data$quintile <-
  factor(
    data$quintile,
    levels = c("1st", "2nd", "3rd", "4th", "5th"))

# aqui posso testar se há diferença significativa
# entre essa variáves em cada quintil para
# atestar que elas não estão influenciando
# hipótese: não há diferença relevante entre os
# quintis em relação a sua composição.

stateCount <-
  data %>%
  count(quintile, popID) %>%
  rename(N = n) %>%
  group_by(quintile) %>%
  mutate(percentage = N / sum(N) * 100)

sexCount <-
  data %>%
  count(quintile, sex) %>%
  rename(N = n) %>%
  group_by(quintile) %>%
  mutate(percentage = N / sum(N) * 100)

ancesPercentage <-
  data %>%
  select(AMR, AFR, EUR, quintile) %>%
  group_by(quintile) %>%
  summarise(
    AMR = mean(AMR),
    AFR = mean(AFR),
    EUR = mean(EUR)) %>%
  tidyr::pivot_longer(
    cols = c("AMR", "EUR", "AFR"),
    names_to = "ancestry",
    values_to = "percentage"
  )

# swatch()

p1 <-
  ggplot(stateCount, aes(quintile, percentage, fill = popID)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#073642bd", "#db735cd5")) +
  theme_publish() +
  labs(fill = "State", y = "") +
  theme(
    text = element_text(size = 7),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    legend.position = "right",
    legend.box = "vertical",
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank())

p2 <-
  ggplot(sexCount, aes(quintile, percentage, fill = sex)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#3d7a5bce", "#32a79bc5")) +
  theme_publish() +
  labs(fill = "Sex", y = "Percentage") +
  theme(
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    legend.position = "right",
    legend.box = "vertical",
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank())

p3 <-
  ggplot(ancesPercentage, aes(quintile, y = percentage * 100, fill = ancestry)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#af2e50b9", "#483248ad", "#470424d2")) +
  labs(fill = "Ancestry", x = "PRS Quintile", y = "") +
  theme_publish() +
  theme(
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    legend.position = "right",
    legend.box = "vertical")

library(ggpubr)

ggarrange(
  p1, p2, p3, nrow = 3, labels = c("A", "B", "C"),
  font.label = list(size = 10, color = "black",
  face = "bold", family = "Arial"),
  common.legend = FALSE, widths = 1.5, heights = 1,
  align = "hv")

ggsave(
  "Figure6_version1.png", device = "png",
  width = 85, height = 150, units = c("mm"),
  dpi = 300, bg = "white")
