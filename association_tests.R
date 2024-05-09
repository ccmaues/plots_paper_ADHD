# Association tests
#setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("functions_to_source.R")

vADHD <-
prs_v3 %>%
select(IID, ADHD) %>%
rename(adjusted_PRS = ADHD)

ogvADHD <-
fread(glue("{Path}/PRS_database/Final_Scores_PRSCS/PRSCS_ADHD_Score.profile")) %>%
select(IID, PRSCS_zscore) %>%
rename(original_PRS = 2)

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
plyr::join_all(list(sex, vADHD, state, aADHD, ogvADHD), by = "IID", type = "inner") %>%
inner_join(., pADHD, by = c("IID", "wave")) %>%
filter(IID %in% ages$IID) %>%
mutate(
  diagnosis = as.factor(diagnosis),
  wave = as.factor(wave)
  )

non_redudant_data <-
data %>%
select(IID, sex, popID, adjusted_PRS) %>%
unique()

### Normality test
theorical_dist <- rnorm(1363)
ks.test(unique(data$adjusted_PRS), theorical_dist)

ggthemr("fresh")
library(ggrepel)

data.frame(IID = non_redudant_data$IID, adjusted_PRS = non_redudant_data$adjusted_PRS, theorical_dist) %>%
ggplot(aes(sample = adjusted_PRS)) +
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
shapiro.test(filter(non_redudant_data, sex == "Female")$adjusted_PRS)
shapiro.test(filter(non_redudant_data, sex == "Male")$adjusted_PRS)
# Os dois são normais
leveneTest(adjusted_PRS ~ sex, non_redudant_data)
# Variancia igual
t.test(adjusted_PRS ~ sex, non_redudant_data, var.equal = TRUE)
# Diferença não é relevante estatisticamente falando
cohens_d(data = non_redudant_data, adjusted_PRS ~ sex, paired = FALSE)
# Magnitude irrelevante

## estado
shapiro.test(filter(non_redudant_data, popID == "BRA_SP")$adjusted_PRS)
shapiro.test(filter(non_redudant_data, popID == "BRA_RS")$adjusted_PRS)
# Os dois são normais
leveneTest(adjusted_PRS ~ popID, non_redudant_data)
# Variancia igual
t.test(adjusted_PRS ~ popID, non_redudant_data, var.equal = TRUE)
# não há diferença estatística
cohens_d(data = non_redudant_data, adjusted_PRS ~ popID, paired = FALSE)
# Magnitude irrelevante

# even though our data is normal, our outcome
# is binomial so we use glm in this case
# but we need to change our coding to 0 and 1

### diagnosis, PRS, and wave association
group_by(data, wave, diagnosis) %>% shapiro_test(adjusted_PRS)
group_by(data, wave, diagnosis) %>% shapiro_test(original_PRS)
data_pt <-
filter(parents, IID %in% data$IID) %>%
select(IID, ADHD) %>%
rename(p_diagnosis = ADHD)

for_glm <-
  data %>%
  mutate(diagnosis = case_when(
    diagnosis == 2 ~ 1,
    .default = 0
  ),
  risk_gp_new = ntile(adjusted_PRS, 3),
  risk_gp_new = as.factor(case_when(
    risk_gp_new == 1 ~ "low",
    risk_gp_new == 2 ~ "medium",
    risk_gp_new == 3 ~ "high")),
  risk_gp_old = ntile(original_PRS, 3),
  risk_gp_old = as.factor(case_when(
    risk_gp_old == 1 ~ "low",
    risk_gp_old == 2 ~ "medium",
    risk_gp_old == 3 ~ "high"))) %>%
  inner_join(., data_pt, by = "IID")

# terrível dúvida aqui sobre como fazer isso já que o PRS não interage
# com as outras waves, mas sim o diagnóstico. Por isso usar + ao invés de :

group_by(data, wave, diagnosis, sex) %>% shapiro_test(adjusted_PRS)
group_by(data, wave, diagnosis, sex) %>% shapiro_test(original_PRS)

## multivar generalized linear model
# outcome = intecept + weight1 * VD1 +  weight2 * VD2 + ...

### PRS is corrected for age, sex, state, and first 10 PCs
### in theory, sex and state are not a bias here (corrected)

### HIPOTHESIS: Diagnosis is associated with PRS, sex, wave
### might be good to add follow-up date

####### BY WAVE
##### ALL SCORES
## MODEL 1: the diagnosis is predicted[~] by new PRS in W0
model1 <- glm(diagnosis ~ adjusted_PRS, filter(for_glm, wave == "W0"), family = binomial)
confint(model1)
summary(model1)

## MODEL 2: the diagnosis is predicted[~] by old PRS in W0
model2 <- glm(diagnosis ~ original_PRS, filter(for_glm, wave == "W0"), family = binomial)
confint(model2)
summary(model2)

## MODEL 3: the diagnosis is predicted[~] by new PRS in W1
model3 <- glm(diagnosis ~ adjusted_PRS, filter(for_glm, wave == "W1"), family = binomial)
confint(model3)
summary(model3)

## MODEL 4: the diagnosis is predicted[~] by old PRS in W1
model4 <- glm(diagnosis ~ original_PRS, filter(for_glm, wave == "W1"), family = binomial)
confint(model4)
summary(model4)

## MODEL 5: the diagnosis is predicted[~] by new PRS in W2
model1 <- glm(diagnosis ~ adjusted_PRS, filter(for_glm, wave == "W2"), family = binomial)
confint(model1)
summary(model1)

## MODEL 6: the diagnosis is predicted[~] by old PRS in W2
model2 <- glm(diagnosis ~ original_PRS, filter(for_glm, wave == "W2"), family = binomial)
confint(model2)
summary(model2)

####### BY WAVE
##### ONLY HIGH RISK
## MODEL 1: the diagnosis is predicted[~] by new high risk PRS in W0
model1 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "high" &
  wave == "W0"),
family = binomial)
summary(model1)
confint(model1)

## MODEL 2: the diagnosis is predicted[~] by old high risk PRS in W0
model2 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "high" &
  wave == "W0"),
family = binomial)
summary(model2)
confint(model2)

## MODEL 3: the diagnosis is predicted[~] by new high risk PRS in W1
model3 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "high" &
  wave == "W1"),
family = binomial)
summary(model3)
confint(model3)

## MODEL 4: the diagnosis is predicted[~] by old high risk PRS in W1
model4 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "high" &
  wave == "W1"),
family = binomial)
summary(model4)
confint(model4)

## MODEL 5: the diagnosis is predicted[~] by new high risk PRS in W2
model5 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "high" &
  wave == "W2"),
family = binomial)
summary(model5)
confint(model5)

## MODEL 6: the diagnosis is predicted[~] by old high risk PRS in W2
model6 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "high" &
  wave == "W2"),
family = binomial)
summary(model6)
confint(model6)

##### ONLY MEDIUM RISK
## MODEL 1: the diagnosis is predicted[~] by new medium risk PRS in W0
model1 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "medium" &
  wave == "W0"),
family = binomial)
summary(model1)
confint(model1)

## MODEL 2: the diagnosis is predicted[~] by old medium risk PRS in W0
model2 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "medium" &
  wave == "W0"),
family = binomial)
summary(model2)
confint(model2)

## MODEL 3: the diagnosis is predicted[~] by new medium risk PRS in W1
model3 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "medium" &
  wave == "W1"),
family = binomial)
summary(model3)
confint(model3)

## MODEL 4: the diagnosis is predicted[~] by old medium risk PRS in W1
model4 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "medium" &
  wave == "W1"),
family = binomial)
summary(model4)
confint(model4)

## MODEL 5: the diagnosis is predicted[~] by new medium risk PRS in W2
model5 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "medium" &
  wave == "W2"),
family = binomial)
summary(model5)
confint(model5)

## MODEL 6: the diagnosis is predicted[~] by old medium risk PRS in W2
model6 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "medium" &
  wave == "W2"),
family = binomial)
summary(model6)
confint(model6)

##### ONLY LOW RISK
## MODEL 1: the diagnosis is predicted[~] by new low risk PRS in W0
model1 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "low" &
  wave == "W0"),
family = binomial)
summary(model1)
confint(model1)

## MODEL 2: the diagnosis is predicted[~] by old low risk PRS in W0
model2 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "low" &
  wave == "W0"),
family = binomial)
summary(model2)
confint(model2)

## MODEL 3: the diagnosis is predicted[~] by new low risk PRS in W1
model3 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "low" &
  wave == "W1"),
family = binomial)
summary(model3)
confint(model3)

## MODEL 4: the diagnosis is predicted[~] by old low risk PRS in W1
model4 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "low" &
  wave == "W1"),
family = binomial)
summary(model4)
confint(model4)

## MODEL 5: the diagnosis is predicted[~] by new low risk PRS in W2
model5 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  risk_gp_new == "low" &
  wave == "W2"),
family = binomial)
summary(model5)
confint(model5)

## MODEL 6: the diagnosis is predicted[~] by old low risk PRS in W2
model6 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  risk_gp_old == "low" &
  wave == "W2"),
family = binomial)
summary(model6)
confint(model6)

##### AGE IN YEARS
## MODEL 1: the diagnosis is predicted[~] by age in W0
model1 <- glm(diagnosis ~ age, filter(for_glm, wave == "W0"), family = binomial)
summary(model1)
confint(model1)

## MODEL 2: the diagnosis is predicted[~] by age in W1
model2 <- glm(diagnosis ~ age, filter(for_glm, wave == "W1"), family = binomial)
summary(model2)
confint(model2)

## MODEL 3: the diagnosis is predicted[~] by age in W2
model3 <- glm(diagnosis ~ age, filter(for_glm, wave == "W2"), family = binomial)
summary(model3)
confint(model3)

####### BY WAVE
##### SEX
## MODEL 1: the diagnosis is predicted[~] by Female adjusted PRS sex in W0
model1 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  wave == "W0" &
  sex == "Female"),
family = binomial)
summary(model1)
confint(model1)

## MODEL 2: the diagnosis is predicted[~] by Female original PRS sex in W0
model2 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  wave == "W0" &
  sex == "Female"),
family = binomial)
summary(model2)
confint(model2)

## MODEL 3: the diagnosis is predicted[~] by Male adjusted PRS sex in W0
model3 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  wave == "W0" &
  sex == "Male"),
family = binomial)
summary(model3)
confint(model3)

## MODEL 4: the diagnosis is predicted[~] by Male original PRS sex in W0
model4 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  wave == "W0" &
  sex == "Male"),
family = binomial)
summary(model4)
confint(model4)

## MODEL 5: the diagnosis is predicted[~] by Female adjusted PRS sex in W1
model5 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  wave == "W1" &
  sex == "Female"),
family = binomial)
summary(model5)
confint(model5)

## MODEL 6: the diagnosis is predicted[~] by Female original PRS sex in W1
model6 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  wave == "W1" &
  sex == "Female"),
family = binomial)
summary(model6)
confint(model6)

## MODEL 7: the diagnosis is predicted[~] by Male adjusted PRS sex in W1
model7 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  wave == "W1" &
  sex == "Male"),
family = binomial)
summary(model7)
confint(model7)

## MODEL 8: the diagnosis is predicted[~] by Male original PRS sex in W1
model8 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  wave == "W1" &
  sex == "Male"),
family = binomial)
summary(model8)
confint(model8)

## MODEL 9: the diagnosis is predicted[~] by Female adjusted PRS sex in W1
model9 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  wave == "W2" &
  sex == "Female"),
family = binomial)
summary(model9)
confint(model9)

## MODEL 10: the diagnosis is predicted[~] by Female original PRS sex in W1
model10 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  wave == "W2" &
  sex == "Female"),
family = binomial)
summary(model10)
confint(model10)

## MODEL 11: the diagnosis is predicted[~] by Male adjusted PRS sex in W1
model11 <-
glm(
diagnosis ~ adjusted_PRS,
filter(for_glm,
  wave == "W2" &
  sex == "Male"),
family = binomial)
summary(model11)
confint(model11)
0
## MODEL 12: the diagnosis is predicted[~] by Male original PRS sex in W1
model12 <-
glm(
diagnosis ~ original_PRS,
filter(for_glm,
  wave == "W2" &
  sex == "Male"),
family = binomial)
summary(model12)
confint(model12)

####### ALL TIME
##### SEX
## MODEL 1: diagnosis is predicted[~] by PRS values with adjustement
model1 <- glm(diagnosis ~ adjusted_PRS, for_glm, family = binomial)
summary(model1)
confint(model1)

## MODEL 2: diagnosis is predicted[~] by PRS values without adjustement
model2 <- glm(diagnosis ~ original_PRS, for_glm, family = binomial)
summary(model2)
confint(model2)

## MODEL 3: diagnosis is predicted[~] by low PRS values with adjustement
model3 <- glm(diagnosis ~ adjusted_PRS, filter(for_glm, risk_gp_new == "low"), family = binomial)
summary(model3)
confint(model3)

## MODEL 4: diagnosis is predicted[~] by low PRS values without adjustement
model4 <- glm(diagnosis ~ original_PRS, filter(for_glm, risk_gp_old == "low"), family = binomial)
summary(model4)
confint(model4)

## MODEL 6: diagnosis is predicted[~] by medium PRS values with adjustement
model6 <- glm(diagnosis ~ adjusted_PRS, filter(for_glm, risk_gp_new == "medium"), family = binomial)
summary(model6)
confint(model6)

## MODEL 7: diagnosis is predicted[~] by medium PRS values without adjustement
model7 <- glm(diagnosis ~ original_PRS, filter(for_glm, risk_gp_old == "medium"), family = binomial)
summary(model7)
confint(model7)

## MODEL 7: diagnosis is predicted[~] by high PRS values with adjustement
model7 <- glm(diagnosis ~ adjusted_PRS, filter(for_glm, risk_gp_new == "high"), family = binomial)
summary(model7)
confint(model7)

## MODEL 8: diagnosis is predicted[~] by high PRS values without adjustement
model8 <- glm(diagnosis ~ original_PRS, filter(for_glm, risk_gp_old == "high"), family = binomial)
summary(model8)
confint(model8)

## MODEL 9: diagnosis is predicted[~] by age
model9 <- glm(diagnosis ~ age, for_glm, family = binomial)
summary(model9)
confint(model9)

## MODEL 10: diagnosis is predicted[~] by PRS of females with adjustment
model10 <- glm(diagnosis ~ adjusted_PRS, filter(for_glm, sex == "Female"), family = binomial)
summary(model10)
confint(model10)

## MODEL 11: diagnosis is predicted[~] by PRS of females without adjustment
model11 <- glm(diagnosis ~ original_PRS, filter(for_glm, sex == "Female"), family = binomial)
summary(model11)
confint(model11)

## MODEL 12: diagnosis is predicted[~] by PRS of females with adjustment
model12 <- glm(diagnosis ~ adjusted_PRS, filter(for_glm, sex == "Male"), family = binomial)
summary(model12)
confint(model12)

## MODEL 13: diagnosis is predicted[~] by PRS of females without adjustment
model13 <- glm(diagnosis ~ original_PRS, filter(for_glm, sex == "Male"), family = binomial)
summary(model13)
confint(model13)

# na minha opinião, seria wave:sex ou sex:age