setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

pacman::p_load(survival, survminer, survcomp)

data <- mutate(data, age = round(age, 0)) %>%
  filter(age >= 10 & age <= 20)

# Simplify the data
# separate the controls
t1 <-
  data %>%
  filter(age >= 10 & age <= 20) %>%
  filter(wave == "W2" & diagnosis == 0)
# separate the cases and
# keep just the first occurance of diagnosis
t2 <-
  data %>%
  filter(age >= 10 & age <= 20) %>%
  filter(!IID %in% t1$IID) %>%
  filter(diagnosis == 2) %>%
  group_by(IID) %>%
  filter(age == min(age)) %>%
  ungroup()

wd2 <-
  bind_rows(t1, t2) %>%
  select(IID, age, diagnosis, adjusted_PRS) %>%
  mutate(
    risk = ntile(adjusted_PRS, 3),
    diagnosis = factor(diagnosis, levels = c("0", "2"), labels = c("0", "1")),
    diagnosis = as.numeric(as.character(diagnosis)),
    PRS = case_when(
      risk == 1 ~ "Low",
      risk == 2 ~ "Medium",
      risk == 3 ~ "High"),
    PRS = factor(PRS, levels = c("Low", "Medium", "High"))) %>%
    select(-adjusted_PRS, -risk) %>%
  rename(ID = 1, time = 2, status = 3) %>%
  mutate(time = round(time, digits = 0)) %>%
	rename(IID = 1) %>%
	inner_join(., sex, by = "IID") %>%
  rename(ID = 1)

surv_fit <- survfit(Surv(time, status) ~ PRS, data = wd2)

ggthemr("fresh")

p <-
  ggsurvplot(
    surv_fit,
    data = wd2,
    linetype = "strata",
    fun = "event", # changes to the chance of diagnosis
    censor = TRUE,
    censor.shape = "|",
    censor.size = 3,
    surv.scale = "percent",
    surv.plot.height = 0.5,
    break.time.by = 2,
    break.y.by = 0.05,
    xlab = "Age (yr)",
    ylab = "Diagnosis Probability",
    legend.labs = c("Low", "Medium", "High"),
    legend.title = "Risk level",
    xlim = c(min(wd2$time), max(wd2$time)),
    ylim = c(0, 0.25),
    risk.table = "abs_pct",
    risk.table.col = "strata",
    risk.table.fontsize = 4,
    risk.table.y.text = FALSE,
    tables.height = 0.15,
    cumevents = TRUE,
    cumevents.col = "strata",
    cumevents.y.text = FALSE,
    cumevents.height = 0.15,
    font.legend = 15,
    ggtheme = theme_publish()
  )

p1 <-
  p$plot +
  scale_x_continuous(n.breaks = 10) +
  scale_color_manual(values = c("Low" = "#6a9716", "Medium" = "#FF5733", "High" = "#C70039")) +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.1, "lines"))
  
p1
ggsave(
  "Figure9.png",
  p1,
  device = "png",
  units = "mm",
  height = 80,
  width = 150,
  bg = "white")
