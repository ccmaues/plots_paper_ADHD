###### run the model
# Install and load required packages
# install.packages("lme4")
# why not use GLMM instead of GEE
# GEE is for population estimation
# GLMM is for the individual estimation
# https://stats.stackexchange.com/questions/16390/when-to-use-generalized-estimating-equations-vs-mixed-effects-models
library(lme4)

# Fit the GLMM with Gamma family and log link
model <- glmer(age ~ adjusted_PRS + sex:diagnosis + (1 | IID), family = Gamma(link = "log"), data = data)

# Summary of the model
summary(model)

# Model assumption testing

# Linearity: Residuals vs Fitted plot
plot(model, which = 1)

# Normality of Residuals: Q-Q plot and Shapiro-Wilk test
qqnorm(residuals(model))
qqline(residuals(model))
shapiro.test(residuals(model))

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

# https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
# they say in here that is good to test some
# different models and see thei aplicabillity