#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

# assumptions
# glm residuals must be normally distributed
model <-
  glm(
    age ~ adjusted_PRS + diagnosis,
    data,
    family = "poisson"
  )
# res <-
#   as.data.frame(residuals(model)) %>%
#   rename(residuals = 1)

ggplot(res, aes(sample = residuals)) +
  stat_qq_line(linetype = "dashed") +
  stat_qq(size = 5, alpha = 0.5)

rstatix::shapiro_test(res$residuals)

t <-
  data.frame(model$fitted.values, residuals(model)) %>%
  rename(fit = 1, res = 2)

ggplot(t, aes(fit, res)) +
geom_point()
