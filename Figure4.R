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

w0 <-
data %>%
filter(wave == "W0") %>%
ggplot(aes(PRS, fill = diagnosis, color = diagnosis)) +
geom_density(alpha = 0.2) +
scale_fill_manual(values = c("red", "blue")) +
theme_publish() +
theme(
  text = element_text(size = 7),
  axis.text = element_text(size = 7)) +
  labs(y = "\n", x = "")

w1 <-
data %>%
filter(wave == "W1") %>%
ggplot(aes(PRS, fill = diagnosis, color = diagnosis)) +
geom_density(alpha = 0.2) +
scale_fill_manual(values = c("red", "blue")) +
theme_publish() +
theme(
  text = element_text(size = 7),
  axis.text = element_text(size = 7)) +
  labs(y = "\nDensity", x = "")

w2 <-
data %>%
filter(wave == "W1") %>%
ggplot(aes(PRS, fill = diagnosis, color = diagnosis)) +
geom_density(alpha = 0.2) +
scale_fill_manual(values = c("red", "blue")) +
theme_publish() +
theme(
  text = element_text(size = 7),
  axis.text = element_text(size = 7)) +
  labs(y = "\n", x = "PRS")

library(ggpubr)

final <-
ggarrange(
  w0, w1, w2, nrow = 3, labels = c("A", "B", "C"),
  font.label = list(size = 7, color = "black", face = "bold", family = "Arial"),
  common.legend = TRUE)

ggsave(
  "Figure4_version2.png", final, device = "png",
  width = 85, height = 100, units = c("mm"),
  dpi = 300, bg = "white")
