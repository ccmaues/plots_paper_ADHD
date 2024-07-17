setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

### Calculate the weights for my data
# get all IIDs, make new var to mark which one
# stays in my analysis
dawba <-
  readRDS(glue("{Path}/0_external_files/dawba_20200526.rds")) %>%
  mutate(subjectid = gsub("^", "C", subjectid)) %>%
  select(subjectid) %>%
  rename(IID = 1)

all_IDs <- unique(dawba$IID)

# Load IIDs 'translator'
espelho <-
  readRDS(glue("{Path}/0_external_files/Lucas_MINI_BHRCS.rds")) %>%
  select(1, 4) %>%
  mutate(ident = as.numeric(ident))

# Ages
ages_og <-
  readRDS(glue("{Path}/0_external_files/Ota_149BHRC_2023_01_15.rds")) %>%
  select(1, 2, 3) %>%
  filter(redcap_event_name == "wave0_arm_1") %>%
  select(-2) %>%
  inner_join(., espelho, by = "ident")

# Combine data for weight estimation
check <-
  c(all_IDs, unique(data$IID)) %>%
  table() %>%
  as.data.frame() %>%
  filter(Freq == 2) %>%
  rename(IID = 1, new_weight = 2) %>%
  left_join(data.frame(IID = unique(dawba$IID)), ., by = "IID") %>%
  mutate(
    new_weight = case_when(
      new_weight == 2 ~ 1,
      TRUE ~ 0)) %>%
  inner_join(., ages_og, by = "IID") %>%
  select(-ident) %>%
  mutate(age =
    as.numeric(
      difftime(as.Date("2010-01-01"),
      birth_date_original,
      units = "days")) / 365.25)

# Logit regression to generate probability of attrition
w_fit <-
  glm(
    new_weight ~ age,
    family = binomial(link = 'logit'),
    data = check
    )

summary(w_fit)

# Predict
p_weights <-
  predict(
    w_fit,
    type = "response"
  )

# Invert prediction
inverted_p_weights <- 1 / p_weights

# Add weights to IIDs
new_weights <-
  cbind(check$IID, inverted_p_weights) %>%
  as.data.frame() %>%
  rename(IID = 1, weights = 2) %>%
  filter(IID %in% data$IID) %>%
  mutate(weights = as.numeric(weights))


### testing
# ~~~~~~ issue
sum(inverted_p_weights) # sum must be around 2511

mydata_w1_attr <-
  new_weights %>%
  filter(weights == 1)

sum(mydata_w1_attr) # maximum value should not be higher than 4. If so, suggest to trimm for the 99 or 95th percentile.
summary(mydata_w1_attr)

###### Make pair plots
# The variables have correlation between them?
pacman::p_load(GGally)
ggthemr("fresh")
data %>%
  select(age, adjusted_PRS, sex, popID) %>%
  ggpairs() +
  theme_publish()

###### run the model
# Install and load required packages
# GLMM calculates the population effect and individual effect
pacman::p_load("lme4")

# Inicialmente farei SEM separação por quintil
# fazer só com casos
final_data <- inner_join(data, new_weights, by = "IID")

# Fit the GLMM with Gamma family and log link
pacman::p_load("doParallel")

# Set up parallel backend
cl <- makeCluster(detectCores() - 1) # leave one core free
registerDoParallel(cl)

# Fit the model
m1 <- glmer(
  age ~ adjusted_PRS + sex * diagnosis + (1 | IID), # formula
  data = final_data,  # data
  family = poisson(link = "log")
)

# Stop the cluster
stopCluster(cl)

# m4 <-
#   glmer(
#     age ~ adjusted_PRS + sex * diagnosis + (1 | IID), # formula
#     weights = final_data$weights, # weights for calculating
#     data = final_data,  # data
#     family = inverse.gaussian(link = "1/mu^2"), # link function
#     control = glmerControl(optimizer = "bobyqa"), # control for overfitting
#     nAGQ = 10 # number of quadrature points
#   )

# Summary of the model
summary(m1)

# Model assumption testing
# Linearity: Residuals vs Fitted plot
plot(model, which = 1)

# Normality of Residuals: Q-Q plot and Shapiro-Wilk test
qqnorm(residuals(m3))
qqline(residuals(m3))
shapiro.test(residuals(m2))

# Homoscedasticity: Plot residuals against fitted values
plot(residuals(model) ~ fitted(model))

# Residuals and Fitted Values
residuals <- resid(model, type = "deviance")
fitted_values <- fitted(model)

# Residuals vs Fitted Values Plot
plot(fitted_values, residuals, 
     xlab = "Fitted Values", 
     ylab = "Deviance Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# Q-Q Plot for Residuals
qqnorm(residuals)
qqline(residuals, col = "red")

# Normality of Random Effects
random_effects <- ranef(model)
qqnorm(unlist(random_effects))
qqline(unlist(random_effects), col = "red")

# Shapiro-Wilk Test for Residuals
shapiro.test(residuals)

# Scale-Location Plot
sqrt_abs_residuals <- sqrt(abs(residuals))
plot(fitted_values, sqrt_abs_residuals, 
     xlab = "Fitted Values", 
     ylab = "Square Root of |Residuals|",
     main = "Scale-Location Plot")
abline(h = 0, col = "red")

# Partial Residual Plots
library(car)
crPlots(model)

plot_model(modelo_PRScs_2, type = "pred", terms = c("PGS","deprivation", "gender1"))
plot_summs(modelo_PRScs_2, scale = TRUE, exp = TRUE)
summary(modelo_PRScs_1)
plot_model(modelo_PRScs_1, type = "pred", terms = c("PGS","Deprivation", "gender1"))
(0 + factor(site))