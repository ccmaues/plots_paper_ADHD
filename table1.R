#setwd("/media/santorolab/C207-3566")
source("functions_to_source.R")

# PRS data
vADHD <-
select(prs_v3, IID, ADHD) %>%
rename(PRS = ADHD)

# Phenotype data
pADHD <-
select(pheno, IID, wave, dcanyhk) %>%
rename(ADHD = dcanyhk) %>%
tidyr::pivot_wider(
  names_from = "wave",
  values_from = "ADHD"
)

# Ancestry data
anc <- readRDS(glue("{Path}/objects_R/cass_BHRC_ADMIXTURE.RDS")) %>%
mutate(across(c("EUR", "AFR", "AMR"), ~ . * 100))

# Working data
data <-
plyr::join_all(list(vADHD, pADHD, sex, ages, state, anc), by = "IID", type = "inner")

library(psych)
describe(data$EUR)
describe(data$AMR)
describe(data$AFR)

describe(data$age_W0)
describe(data$age_W1)
describe(data$age_W2)

table(data$sex)
table(data$W0)
table(data$W1)
table(data$W2)

table(data$popID)
