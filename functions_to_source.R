# Functions for source in other codes
# and datas that are standard in the analysis
# less "polution" in main codes
# created for use in our data

pacman::p_load(
  glue, dplyr, ggplot2, remotes,
  tidyr, envalysis, extrafont, glue,
  ggthemr, data.table, purrr, gtsummary, broom
)

# PRS value adjustment with all variables
adjust_model <- function(data_list) {
  # data must contain the dfs with
  # variables adjustment
  var_for_ajustment <-
    plyr::join_all(
      data_list,
      by = "IID",
      type = "inner"
    )
  # The + indicates multiple predictors
  new_PRS <-
    glm(PRS ~ sex + age_W0 + age_W1 + age_W2 +
    popID + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
    PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
    PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20,
    data = var_for_ajustment)
  return(new_PRS)
}


# PRS value adjustment withou age
adjust_model_v2 <- function(data_list) {
  # data must contain the dfs with
  # variables adjustment
  var_for_ajustment <-
    plyr::join_all(
      data_list,
      by = "IID",
      type = "inner"
    )
  # The + indicates multiple predictors
  new_PRS <-
    glm(PRS ~ sex +
    popID + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
    PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 +
    PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20,
    data = var_for_ajustment)
  return(new_PRS)
}


# PRS value adjustment withou age and only 10 PCs
adjust_model_v3 <- function(data_list) {
  # data must contain the dfs with
  # variables adjustment
  var_for_ajustment <-
    plyr::join_all(
      data_list,
      by = "IID",
      type = "inner"
    )
  # The + indicates multiple predictors
  new_PRS <-
    glm(PRS ~ sex +
    popID + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
    PC7 + PC8 + PC9 + PC10,
    data = var_for_ajustment)
  return(new_PRS)
}

# Calculate the prevalence
calc_prev <- function(data, n, column_name, wave) {
  # column_name and wave must be give as char
  df <-
    select(data, all_of(column_name), wave) %>%
    mutate(quantile_n = ntile(!!sym(column_name), n)) %>%
    select(quantile_n, wave)
  # !! bang-bang == unquote for dplyr
  # sym() converts a char to symbol
  p <-
    as.data.frame(cbind(1:n, table(df$quantile_n, df[[wave]]))) %>%
    rename("ntile" = 1, "Control" = 2, "Case" = 3) %>%
    mutate(
      prev = Case / (Control + Case),
      ntile = factor(ntile)) %>%
    select(-Control, -Case)
  return(p)
}

# add risk groups to data.frame based on given column
add_risk <- function(data, column_name) {
  opt <-
    data %>%
    mutate(diagnosis = case_when(
      diagnosis == 2 ~ 1,
      .default = 0),
    gp_risk = ntile(!!sym(column_name), 3),
    gp_risk = as.factor(case_when(
      gp_risk == 1 ~ "low",
      gp_risk == 2 ~ "medium",
      gp_risk == 3 ~ "high")))
  return(opt$gp_risk)
}

# calculate univariated GLM
uni_model_cal <- function(data) {
  colnames(data)[!colnames(data) == "diagnosis"] %>%
  paste("diagnosis ~", .) %>%
  map(.f = ~glm(formula = as.formula(.x),
  family = "binomial", data = data)) %>%
  map(.f = ~tidy(.x, exponentiate = TRUE, conf.int = TRUE)) %>%
  bind_rows() %>%
  mutate(
  across(where(is.numeric), round, digits = 3))
#  p.value = format(p.value, scientific = TRUE))
}

# get ORs from data
uni_model_OR <- function(data) {
  data %>%
  tbl_uvregression(
    method = glm,
    y = diagnosis,
    method.args = list(family = binomial),
    exponentiate = TRUE)
}
