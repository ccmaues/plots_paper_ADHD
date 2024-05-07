# Association tests
setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("functions_to_source.R")

vADHD <-
prs_v3 %>%
select(IID, ADHD) %>%
rename(PRS = ADHD)

aADHD <-
ages %>%
tidyr::pivot_longer(
  cols = c(2, 3, 4),
  names_to = "wave",
  values_to = "age")
aADHD$wave <- gsub("age_", "", aADHD$wave)

pADHD <-
pheno %>%
select(IID, wave, dcanyhk) %>%
rename(diagnosis = 3)

data <-
plyr::join_all(list(sex, vADHD, state, aADHD), by = "IID", type = "inner") %>%
inner_join(., pADHD, by = c("IID", "wave")) %>%
filter(IID %in% ages$IID) %>%
mutate(
  diagnosis = as.factor(diagnosis),
  wave = as.factor(wave),
  )

non_redudant_data <-
data %>%
select(IID, sex, popID, PRS) %>%
unique()

### Normality test
theorical_dist <- rnorm(1363)
ks.test(unique(data$PRS), theorical_dist)

ggthemr("fresh")
library(ggrepel)

data.frame(IID = non_redudant_data$IID, PRS = non_redudant_data$PRS, theorical_dist) %>%
ggplot(aes(sample = PRS)) +
geom_qq(size = 4) +
geom_qq_line(alpha = 0.5, linewidth = 1, color = "red", linetype = "dashed") +
geom_label_repel(
  data = . %>% filter(PRS >= 3 | PRS <= -3),
  aes(x = PRS, y = theorical_dist, label = IID), size = 5) +
theme_publish() +
labs(x = "Sample Z-score", y = "Theorical distribution")

# > filter(ad, IID == "xxxxxx")
#        AMR      AFR      EUR  popID    IID
# 1 0.226309 0.438284 0.335407 BRA_RS xxxxx
# Control in all waves
# > filter(ad, IID == "yyyyyy")
#        AMR      AFR      EUR  popID    IID
# 1 0.151129 0.320829 0.528042 BRA_SP yyyyyyy
# Control in all waves

library(car)
library(rstatix)
### Teste de efeito das variáveis no PRS
## sexo
shapiro.test(filter(non_redudant_data, sex == "Female")$PRS)
shapiro.test(filter(non_redudant_data, sex == "Male")$PRS)
# Os dois são normais
leveneTest(PRS ~ sex, non_redudant_data)
# Variancia igual
t.test(PRS ~ sex, non_redudant_data, var.equal = TRUE)
# Diferença não é relevante estatisticamente falando
cohens_d(data = non_redudant_data, PRS ~ sex, paired = FALSE)
# Magnitude irrelevante

## estado
shapiro.test(filter(non_redudant_data, popID == "BRA_SP")$PRS)
shapiro.test(filter(non_redudant_data, popID == "BRA_RS")$PRS)
# Os dois são normais
leveneTest(PRS ~ popID, non_redudant_data)
# Variancia igual
t.test(PRS ~ popID, non_redudant_data, var.equal = TRUE)
# não há diferença estatística
cohens_d(data = non_redudant_data, PRS ~ popID, paired = FALSE)
# Magnitude irrelevante

# even though our data is normal, our outcome
# is binomial so we use glm in this case
# but we need to change our coding to 0 and 1

### diagnosis, PRS, and wave association
group_by(data, wave, diagnosis) %>% shapiro_test(PRS)
for_glm <-
  data %>%
  mutate(diagnosis = case_when(
    diagnosis == 2 ~ 1,
    .default = 0
  ),
  risk_gp = as.factor(case_when(
    risk_gp == 1 ~ "low",
    risk_gp == 2 ~ "medium",
    risk_gp == 3 ~ "high"
  )))

model1 <- glm(diagnosis ~ PRS:wave, for_glm, family = binomial)
confint(model1)

library(lmtest)
summary(model1)

### diagnosis, PRS, sex, wave association
group_by(data, wave, diagnosis, sex) %>% shapiro_test(PRS)
model2 <- glm(diagnosis ~ PRS:wave:sex, for_glm, family = binomial)
confint(model2)
summary(model2)

## Parent diagnosis association
# houston, we have a problem...
data_pt <-
filter(parents, IID %in% data$IID) %>%
inner_join(., vADHD, by = "IID")

model_p <- glm(ADHD ~ PRS, for_glm_p, family = binomial)
confint(model_p)

## 3 risk groups
data$risk_gp <- ntile(data$PRS, 3)
bind_cols(PRS = data$PRS, risk_gp = ntile(data$PRS, 3)) %>%
group_by(risk_gp) %>%
summarise(mean(PRS), median(PRS))
# tá certo o.o
head(data)

model3 <- glm(diagnosis ~ PRS:risk_gp:wave, for_glm, family = binomial)
confint(model3)
summary(model3)

## age

model4 <- glm(diagnosis ~ PRS:age:wave, for_glm, family = binomial)
confint(model4)
summary(model4)
