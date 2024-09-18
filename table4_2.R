# Chi-square for quintile
# composition difference testing
source("functions_to_source.R")

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

# Phenotype for N adjustment
pADHD <-
  pheno %>%
  select(IID, dcanyhk, wave) %>%
  rename(ADHD = dcanyhk) %>%
  tidyr::pivot_wider(
    names_from = "wave",
    values_from = "ADHD"
  )

## working data
data <-
  plyr::join_all(
  list(vADHD, sex, state, anc, ages, pADHD),
  by = "IID", type = "inner") %>%
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

## CHI-SQUARE TEST:
library(rstatix)
# perfoms all assumptions tests:
chisq.test(table(data$quintile, data$sex))
chisq.test(table(data$quintile, data$popID))
