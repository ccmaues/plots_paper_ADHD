setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("functions_to_source.R")
source("data_to_source.R")

pacman::p_load(VIM, mice, naniar)

# pqp eu tenho que ver cada uma variável desse db
# supresa! Eu não pedi a MINHA VARIÁVEL DE INTERESSE :))))))))))
cass_BHRC <- readRDS(
	glue("{Path}/objects_R/Santoro_192BHRC_2024_08_17.rds")) %>%
	select(ident, gender, birth_date, d_date, age, site)

espelho <-
  readRDS(glue("{Path}/0_external_files/Lucas_MINI_BHRCS.rds")) %>%
  select(1, 4) %>%
  mutate(ident = as.numeric(ident))

og_prs <-
	fread(glue("{Path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
	select(IID, PRSCS_zscore) %>%
	rename(original_PRS = 2)

# only 2344 unique entries (possibly)
pheno <- readRDS(glue("{Path}/0_external_files/dawba_20200526.rds")) %>%
	select(subjectid, redcap_event_name, dcanyhk) %>%
	rename(IID = 1, wave = 2, ADHD = 3) %>%
	mutate(wave = case_when(
		wave == "wave0_arm_1" ~ "W0",
		wave == "wave1_arm_1" ~ "W1",
		wave == "wave2_arm_1" ~ "W2"
	))
pheno$IID <- gsub("^", "C", pheno$IID)

data <- cass_BHRC %>%
  select(ident, site, gender, d_date) %>%
	inner_join(., espelho, by = "ident") %>%
	select(-ident) %>%
  mutate(
    site = case_when(site == 1 ~ "RS", TRUE ~ "SP"),
    gender = case_when(gender == 2 ~ "Female", TRUE ~ "Male"),
		year = year(d_date),
    wave = case_when(
      year < 2012 ~ "W0",
      year >= 2012 & year < 2015 ~ "W1",
      year >= 2015 ~ "W2",
      is.na(year) ~ NA_character_)) %>%
	filter(!is.na(wave)) %>% # remove covid waves
	full_join(., pheno, by = c("IID", "wave")) %>%
	inner_join(., og_prs, by = "IID") %>%
	rename(evaluation_date = d_date, sex = gender) %>%
	mutate_if(is.character, as.factor)

ggthemr("fresh")

summary(data)
anyNA(data)
prop_miss(data)
miss_var_summary(data)
aggr(data, numbers = TRUE, prop = TRUE)
# pick data with missing variables only
cor(data) %>% round(digits = 2)
# pick all variables
cor(data %>% mutate_if(is.factor, as.numeric),
    dummyNA, use = "pairwise.complete.obs") %>%
  round(digits = 2)
# test what type of missingness is in our data
mcar_test()