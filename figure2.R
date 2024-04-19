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
calc_prev_by_age <- function(data, wave, threshold) {
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
  filter(control >= as.numeric({{threshold}})) %>%
  select(-control, -case)
}

# W0 data
w0_for_plot <- list(
  Overall = calc_prev_by_age(data, "W0", "100"),
  Female = calc_prev_by_age(data_female, "W0", "20"),
  Male = calc_prev_by_age(data_male, "W0", "20")
  )

# Female data
w1_for_plot <- list(
  Overall = calc_prev_by_age(data, "W1", "overall"),
  Female = calc_prev_by_age(data_female, "W1", "female"),
  Male = calc_prev_by_age(data_male, "W1", "male")
  )

# Male data
w2_for_plot <- list(
  Overall = calc_prev_by_age(data, "W2", "overall"),
  Female = calc_prev_by_age(data_female, "W2", "female"),
  Male = calc_prev_by_age(data_male, "W2", "male")
  )

# Data plotting

ggplot(overall_for_plot$W0, aes(age, prev))
