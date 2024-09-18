# Chi-square for quintile
# composition difference testing
setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

# working data
wd <-
  data %>%
  select(IID, adjusted_PRS, popID, sex) %>%
  unique() %>%
  rename(PRS = 2) %>%
  mutate(
    quintile = as.factor(ntile(PRS, 5)),
    quintile = case_when(
      quintile == 1 ~ "1st",
      quintile == 2 ~ "2nd",
      quintile == 3 ~ "3rd",
      quintile == 4 ~ "4th",
      quintile == 5 ~ "5th")) %>%
  select(-PRS, -IID)

wd$quintile <-
  factor(
    wd$quintile,
    levels = c("1st", "2nd", "3rd", "4th", "5th"))

# aqui posso testar se há diferença significativa
# entre essa variáves em cada quintil para
# atestar que elas não estão influenciando
# hipótese: não há diferença relevante entre os
# quintis em relação a sua composição.

table(wd$quintile, wd$sex) %>% cbind(table(wd$quintile, wd$popID))