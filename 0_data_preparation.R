# main object preparation
setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")

pacman::p_load(dplyr, tidyr, glue, data.table)

# check system
if (Sys.info()["sysname"] == "Linux") {
  Path <- "/media/santorolab/C207-3566"
  font <- "Ubuntu Condensed"
} else {
  Path <- "E:"
  font <- "Arial Narrow"
}

# Decore the identification in numbers to IID
mi <-
  readRDS(glue("{Path}/0_external_files/Lucas_MINI_BHRCS.rds")) %>%
  select(1, 4) %>%
  mutate(ident = as.numeric(ident))

# Get necessary variables
bhrc <- readRDS(
  glue("{Path}/objects_R/Santoro_192BHRC_2024_08_17.rds")) %>%
  select(ident, gender, birth_date, d_date, age, site) %>%
  inner_join(., mi, by = "ident") %>%
  select(-ident) %>%
  mutate(
    site = case_when(site == 1 ~ "RS", TRUE ~ "SP"),
    gender = case_when(gender == 2 ~ "Female", TRUE ~ "Male"),
    year = year(d_date),
    wave = case_when(
      year < 2012 ~ "W0",
      year >= 2012 & year < 2015 ~ "W1",
      year >= 2015 ~ "W2",
      is.na(year) ~ NA_character_
    )
  ) %>%
  filter(!is.na(wave)) %>% # remove covid waves
  data.frame()

# Heigh phenotype from BHRC in each wave
h <-
  readRDS(glue("{Path}/objects_R/Antrop.rds")) %>%
  select(ident, redcap_event_name, p_height) %>%
  inner_join(., mi, by = "ident") %>%
  select(-ident) %>%
  mutate(
    redcap_event_name = case_when(
      redcap_event_name == "wave0_arm_1" ~ "W0",
      redcap_event_name == "wave1_arm_1" ~ "W1",
      redcap_event_name == "wave2_arm_1" ~ "W2"
    )
  ) %>%
  rename(wave = 1, height = 2) %>%
  data.frame()

# Diagnosis code obtained with dawba for each wave
pheno <-
  readRDS(glue("{Path}/0_external_files/dawba_20200526.rds")) %>%
  select(subjectid, redcap_event_name, dcanyhk) %>%
  rename(IID = 1, wave = 2, ADHD = 3) %>%
  mutate(wave = case_when(
    wave == "wave0_arm_1" ~ "W0",
    wave == "wave1_arm_1" ~ "W1",
    wave == "wave2_arm_1" ~ "W2",
    TRUE ~ NA_character_
  )) %>%
  data.frame()
pheno$IID <- gsub("^", "C", pheno$IID)

# Polygenic Risk Score values for ADHD:
prs <-
  fread(glue("{Path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
  select(IID, PRSCS) %>%
  rename(PRS = 2)

# tem dado pra deduzir o quanto a pessoa teve de sono

data <- plyr::join_all(
  list(pheno, h, bhrc),
  by = c("IID", "wave"),
  type = "full"
) %>%
  full_join(., prs, by = "IID") %>%
  mutate(
    wave = factor(wave, levels = c("W0", "W1", "W2")),
    site = factor(site, levels = c("SP", "RS")),
    gender = factor(gender, levels = c("Female", "Male")),
    ADHD = factor(ADHD, levels = c(0, 2))
  )

# Save unaltered data
#saveRDS(data, glue("{Path}/objects_R/cass_BHRC_26092024.RDS"))