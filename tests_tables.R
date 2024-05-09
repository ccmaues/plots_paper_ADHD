#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("functions_to_source.R")

#### DATA LOAD AND PREP
vADHD <-
prs_v3 %>%
select(IID, ADHD) %>%
rename(adjusted_PRS = ADHD)

ogvADHD <-
fread(glue("{Path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
select(IID, PRSCS_zscore) %>%
rename(original_PRS = 2)

aADHD <-
ages %>%
tidyr::pivot_longer(
  cols = c(2, 3, 4),
  names_to = "wave",
  values_to = "age")
aADHD$wave <- gsub("age_", "", aADHD$wave)

pADHD <-
pheno %>%
select(IID, wave, dcanyhk) %>%
rename(diagnosis = 3)

data <-
plyr::join_all(list(sex, vADHD, state, aADHD, ogvADHD), by = "IID", type = "inner") %>%
inner_join(., pADHD, by = c("IID", "wave")) %>%
filter(IID %in% ages$IID) %>%
mutate(
  diagnosis = as.factor(diagnosis),
  wave = as.factor(wave))

for_glm <-
  data %>%
  mutate(diagnosis = case_when(
    diagnosis == 2 ~ 1,
    .default = 0
  ),
  risk_gp_new = ntile(adjusted_PRS, 3),
  risk_gp_new = as.factor(case_when(
    risk_gp_new == 1 ~ "low",
    risk_gp_new == 2 ~ "medium",
    risk_gp_new == 3 ~ "high")),
  risk_gp_old = ntile(original_PRS, 3),
  risk_gp_old = as.factor(case_when(
    risk_gp_old == 1 ~ "low",
    risk_gp_old == 2 ~ "medium",
    risk_gp_old == 3 ~ "high"))) %>%
  inner_join(., data_pt, by = "IID")