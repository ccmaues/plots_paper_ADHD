source("functions_to_source.R")

# Phenotype data
pADHD <-
  select(pheno, IID, wave, dcanyhk) %>%
  rename(ADHD = dcanyhk)

# Age data
aADHD <-
tidyr::pivot_longer(
  ages,
  cols = starts_with("age_W"),
  names_to = "wave",
  values_to = "age"
)
aADHD$wave <- gsub("age_W", "W", aADHD$wave)

## Working data
# Overall
data <-
plyr::join_all(
  list(pADHD,  aADHD),
  by = c("wave", "IID"), type = "inner") %>%
mutate(age = round(age, digits = 0)) %>%
rename(diagnosis = ADHD)

# Female subset
data_female <-
  inner_join(data, sex, by = "IID") %>%
  filter(sex == "Female") %>%
  mutate(age = round(age, digits = 0)) %>%
  select(-sex)

# Male subset
data_male <-
  inner_join(data, sex, by = "IID") %>%
  filter(sex == "Male") %>%
  mutate(age = round(age, digits = 0)) %>%
  select(-sex)

## Data preparation for plot (all ages)
calc_prev_by_age <- function(data, wave) {
  data %>%
  filter(wave == {{wave}}) %>%
  group_by(diagnosis, age) %>%
  summarise(freq = n()) %>%
  spread(age, freq, fill = 0) %>%
  t() %>%
  as.data.frame() %>%
  slice(-1) %>%
  tibble::rownames_to_column(var = "age_diagnosis") %>%
  rename(age = 1, control = 2, case = 3) %>%
  mutate(prev = case / (control + case)) %>%
  filter(control >= 100)
}

# Overall data
overall_for_plot <- list(
  W0 = calc_prev_by_age(data, "W0"),
  W1 = calc_prev_by_age(data, "W1"),
  W2 = calc_prev_by_age(data, "W2")
  )

# Female data
female_for_plot <- list(
  W0 = calc_prev_by_age(data_female, "W0"),
  W1 = calc_prev_by_age(data_female, "W1"),
  W2 = calc_prev_by_age(data_female, "W2")
  )

# Male data
female_for_plot <- list(
  W0 = calc_prev_by_age(data_male, "W0"),
  W1 = calc_prev_by_age(data_male, "W1"),
  W2 = calc_prev_by_age(data_male, "W2")
  )
