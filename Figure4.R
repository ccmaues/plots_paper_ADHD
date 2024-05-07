# PRS distribuition plot (case x control)
source("functions_to_source.R")

# Phenotype data
pADHD <-
select(pheno, IID, wave, dcanyhk) %>%
rename(diagnosis = dcanyhk)

# Age data
vADHD <-
select(prs_v3, IID, ADHD) %>%
rename(PRS = ADHD)

## Working data
# Overall
data <-
plyr::join_all(
  list(pADHD, vADHD),
  by = "IID",
  type = "inner") %>%
  mutate(
    diagnosis = case_when(
      diagnosis == 2 ~ "Case",
      diagnosis == 0 ~ "Control")) %>%
select(-IID)

library(ggpubr)
ggthemr("dust")
w0 <-
data %>%
filter(wave == "W0") %>%
ggdensity(
  x = "PRS", add = "mean", fill = "diagnosis", rug = TRUE,
  color = "diagnosis", palette = c("#ad5478b4", "#1c4f5ec7")) +
  labs(x = "", y = "") +
  theme_publish() +
  theme(
    axis.line.y = element_line(linewidth = 0.5, color = "black"),
    axis.line.x = element_line(linewidth = 0.5, color = "black")
  )

w1 <-
data %>%
filter(wave == "W1") %>%
ggdensity(
  x = "PRS", add = "mean", fill = "diagnosis", rug = TRUE,
  color = "diagnosis", palette = c("#ad5478b4", "#1c4f5ec7")) +
  labs(x = "", y = "Density") +
  theme_publish() +
  theme(
    axis.line.y = element_line(linewidth = 0.5, color = "black"),
    axis.line.x = element_line(linewidth = 0.5, color = "black")
  )

w2 <-
data %>%
filter(wave == "W1") %>%
ggdensity(
  x = "PRS", add = "mean", fill = "diagnosis", rug = TRUE,
  color = "diagnosis", palette = c("#ad5478b4", "#1c4f5ec7")) +
  labs(x = "PRS z-score", y = "") +
  theme_publish() +
  theme(
    axis.line.y = element_line(linewidth = 0.5, color = "black"),
    axis.line.x = element_line(linewidth = 0.5, color = "black")
  )
library(ggpubr)

final <-
ggarrange(
  w0, w1, w2, nrow = 3, labels = c("A", "B", "C"),
  font.label = list(size = 10, color = "black", face = "bold", family = "Arial"),
  common.legend = TRUE)

ggsave(
  "Figure4_version3.png", final, device = "png",
  width = 85, height = 180, units = c("mm"),
  dpi = 300, bg = "white")
