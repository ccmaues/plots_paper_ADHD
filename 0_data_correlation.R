pacman::p_load(dplyr)
data <- readRDS("E:/objects_R/cass_BHRC_26092024.RDS")

glm(PRS ~ wave, family = "gaussian", data = data)
glm(PRS ~ ADHD, family = "gaussian", data = data)
glm(PRS ~ height, family = "gaussian", data = data)
glm(PRS ~ site, family = "gaussian", data = data)
glm(PRS ~ gender, family = "gaussian", data = data)
glm(PRS ~ birth_date, family = "gaussian", data = data)
glm(PRS ~ d_date, family = "gaussian", data = data)
model <- glm(PRS ~ ., family = gaussian, data = t) # issue / multicolinearity?
car::vif(model)
model2 <- glm(PRS ~ ADHD + height + site + gender + birth_date + d_date, family = "gaussian", data = data)
car::vif(model2)
