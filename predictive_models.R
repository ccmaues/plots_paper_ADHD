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
    family = binomial(linNk = 'logit'),
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

###### run the m1
# Install and load required packages
# GLMM calculates the population effect and individual effect
pacman::p_load("lme4")

# Inicialmente farei SEM separação por quintil
# fazer só com casos
final_data <-
  select(data, IID, age, adjusted_PRS, sex, diagnosis, wave) %>%
  inner_join(., new_weights, by = "IID")

# Fit the GLMM with Gamma family and log link
# Set up parallel backend
pacman::p_load("doParallel")
cl <- makeCluster(detectCores() - 1) # leave one core free
registerDoParallel(cl)

m3 <- glmer(
  age ~ adjusted_PRS + sex * diagnosis + wave + (1 | IID), # formula
  weights = final_data$weights,
  data = final_data,  # data
  family = gaussian(link = "log")
)

# Stop the cluster
stopCluster(cl)

# Summary of the m1
summary(m3)

### Residuals vs Fitted plot
# Look for a random scatter without any pattern
# Patterns could indicate non-linearity or other
# m1 specification issues.
plot(fitted(m3), residuals(m3),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red")  # Add a horizontal line at zero

### Normal Q-Q plot
# Residuals should fall
# along the 45-degree line.
# Deviations suggest non-normality
qqnorm(residuals(m3))
qqline(residuals(m3), col = 2)

### Scale-Location plot
# Look for a horizontal line with
# equally spread points. A funnel
# shape indicates non-constant variance.
std_residuals <-
  residuals(m3) / sqrt(1 - hatvalues(m3)) # Calculate square root of standardized residuals
plot(fitted(m3), std_residuals,
     xlab = "Fitted values", ylab = "Square root of standardized residuals",
     main = "Scale-Location Plot")
abline(h = 0, col = "red")  # Add a horizontal line at zero

### Residuals vs Leverage plot
# Identify influential
# points that have a high
# leverage and large residuals.
plot(cooks.distance(m3), pch = 19,
     main = "Cook's distance plot",
     xlab = "Observation", ylab = "Cook's distance")
abline(h = 4/length(resid(m3)), col = "red")  # Add a cutoff line for influential observations

### Predicted vs Actual plot
# Points should ideally lie close
# to the 45-degree line, indicating
# good predictions.
pred <- predict(m3, type = "response")
plot(final_data$age, pred, xlab = "Actual Age", ylab = "Predicted Age", main = "Predicted vs Actual")
abline(0, 1, col = "red")

### Residuals histogram
# Should resemble a normal
# distribution (bell curve).
residuals <- resid(m3)
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

### Random effects
qqnorm(ranef(m3))

### Autocorrelation plot of residuals
# Significant spikes suggest autocorrelation
acf(residuals, main = "Autocorrelation of Residuals")
car::vif(m3)

### Partial residual plots
# Help to check the relationship between
# the predictors and response, accounting
# for other predictors.
pacman::p_load(effects)
plot(allEffects(m3), residuals = TRUE)
