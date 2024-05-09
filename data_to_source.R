# R objects for source in other codes
# standard in the analysis
# less "polution" in main codes
# created for use in our data

pacman::p_load(
  glue, dplyr, ggplot2, remotes, tidyr,
  envalysis, extrafont, glue, ggthemr, data.table
)

# check system
if (Sys.info()["sysname"] == "Linux") {
  Path <- "/media/santorolab/C207-3566"
  font <- "Ubuntu Condensed"
} else {
  Path <- "E:"
  font <- "Arial Narrow"
}
pheno <- readRDS(glue("{Path}/objects_R/cass_BHRC_Mod_All_Phenotypes_29-02-2024.RDS"))
ages <-	readRDS(glue("{Path}/objects_R/cass_BHRC_Age_Imputed_26-02-2024.RDS")) %>%
  rename(age_W0 = W0, age_W1 = W1, age_W2 = W2)
state <- readRDS(glue("{Path}/objects_R/cass_BHRC_STATE.RDS"))
sex <- readRDS(glue("{Path}/objects_R/cass_BHRC_sex.RDS"))
pc <- readRDS(glue("{Path}/objects_R/cass_BHRC_PC20.RDS"))
prs_v1 <- readRDS(glue("{Path}/objects_R/cass_BHRC_PRS.RDS"))
prs_v2 <- readRDS(glue("{Path}/objects_R/cass_BHRC_PRS_minus_age.RDS"))
prs_v3 <- readRDS(glue("{Path}/objects_R/cass_BHRC_PRS_minus_age_10PCs.RDS"))
parents <- readRDS(glue("{Path}/objects_R/cass_BHRC_parents_phenotype.RDS"))

## MAIN DATA USED IN THE ANALYSIS
# made with the objects above

data_pt <-
parents %>%
select(IID, ADHD) %>%
rename(p_diagnosis = ADHD)

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

rm(vADHD, ogvADHD, aADHD, pADHD)