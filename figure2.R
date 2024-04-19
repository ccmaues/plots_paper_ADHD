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
calc_prev_by_age <- function(data, wave, new_column_name) {
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
  filter(control >= 20) %>%
  select(-control, -case) %>%
  rename({{new_column_name}} := 2)
}

# W0 data
w0_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W0", "All"),
  calc_prev_by_age(data_female, "W0", "Female"),
  calc_prev_by_age(data_male, "W0", "Male")),
  by = "age", type = "inner"
  )

# Female data
w2_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W1", "All"),
  calc_prev_by_age(data_female, "W1", "Female"),
  calc_prev_by_age(data_male, "W1", "Male")),
  by = "age", type = "inner"
  )

# Male data
w2_for_plot <- plyr::join_all(list(
  calc_prev_by_age(data, "W2", "All"),
  calc_prev_by_age(data_female, "W2", "Female"),
  calc_prev_by_age(data_male, "W2", "Male")),
  by = "age", type = "inner"
  )

# Data plotting

ggplot(overall_for_plot$W0, aes(age, prev))
