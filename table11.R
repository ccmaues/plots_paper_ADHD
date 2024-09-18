setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

### Make generalized linear model first
uni <- glm(
	diagnosis ~ adjusted_PRS + age + popID + wave + p_diagnosis + sex,
  family = "binomial",
  data
)

### Get explained variance of diagnosis
### the adjusted one takes into account the
### number of predictors (apparently and wheather
### they are significant)
opt <- data.frame(
	Beta = coef(uni),
	CI = confint(uni),
	P_value = summary(uni)$coefficients[, "Pr(>|z|)"],
	#R2_Nagelkerke = numeric(),
	stringsAsFactors = FALSE
)

opt %>% knitr::kable()
summary(uni)
