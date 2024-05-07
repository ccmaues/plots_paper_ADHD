# PRS MODEL EVALUATION
source("functions_to_source.R")

# PRS values
vADHD <-
  prs_v3 %>%
  select(IID, ADHD) %>%
  rename(PRS = ADHD) %>%
  mutate(
    quintile = as.factor(ntile(PRS, 5)),
    quintile = case_when(
    quintile == 1 ~ "1st",
    quintile == 2 ~ "2nd",
    quintile == 3 ~ "3rd",
    quintile == 4 ~ "4th",
    quintile == 5 ~ "5th",)) %>%
  select(-PRS)

# Phenotype for N adjustment
pADHD <-
  pheno %>%
  select(IID, dcanyhk, wave) %>%
  rename(ADHD = dcanyhk) %>%
  tidyr::pivot_wider(
    names_from = "wave",
    values_from = "ADHD"
  )

## working data
data <-
  plyr::join_all(
  list(vADHD, sex, state, anc, ages, pADHD),
  by = "IID", type = "inner") %>%
  select(-IID) %>%
  mutate(
    popID = as.factor(case_when(
    popID == "BRA_SP" ~ "SP",
    popID == "BRA_RS" ~ "RS")))

data$quintile <-
  factor(
    data$quintile,
    levels = c("1st", "2nd", "3rd", "4th", "5th"))

## Necessary libraries
# nsROC
library(nsROC)
# R2
library(DescTools)
# PR Curve
library(PRROC)
evaluate_PRS <-
  function(prs_column, pheno_code) {
  ## Main data
  data <-
    prs_v3 %>%
    select(IID, {{prs_column}}) %>%
    inner_join(., select(pheno, IID, wave, {{pheno_code}}), by = "IID") %>%
    rename(PRS = 2, diagnosis = 4) %>%
    select(-IID)
  processing <- group_split(data, wave)

  ## Calculate ROC
  roc_results <- lapply(processing, function(data) {
    gROC(data$PRS, data$diagnosis, pvac.auc = TRUE, side = "auto")})
  names(roc_results) <- c("W0", "W1", "W2")
  ggthemr("fresh")

  p1 <-
  rbind(data.frame(
    FPR = roc_results$W0$points.coordinates[, "FPR"],
    TPR = roc_results$W0$points.coordinates[, "TPR"],
    wave = "W0"),
    data.frame(
    FPR = roc_results$W1$points.coordinates[, "FPR"],
    TPR = roc_results$W1$points.coordinates[, "TPR"],
    wave = "W1"),
    data.frame(
    FPR = roc_results$W2$points.coordinates[, "FPR"],
    TPR = roc_results$W2$points.coordinates[, "TPR"],
    wave = "W2")) %>%
    ggplot(aes(x = FPR, y = TPR, color = wave)) +
    geom_path(linewidth = 1) +
    geom_abline(
      linewidth = 1,
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "#000000",
      alpha = 0.5) +
    coord_equal() +
    labs(
      x = "False Positive Rate",
      y = "True Positive Rate") +
    labs(
      title = glue("ADHD - N = {nrow(data)/3}"),
      subtitle = glue("
      W0 = {format(roc_results$W0$auc*100, digits = 4)}% W1 = {format(roc_results$W1$auc*100, digits = 4)}% W2 = {format(roc_results$W2$auc*100, digits = 4)}%
      "),
      caption = "20-03-2024_cass_BHRC_PRS_eval.R") +
    theme_publish() +
    theme(
      text = element_text(family = font),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 20),
      plot.title = element_text(size = 30),
      plot.subtitle = element_text(size = 25),
      plot.caption = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_blank())

    ## Calculate Area Under Precision-Recall Curve
    baselines <- c()
    pr_curve_df <- lapply(processing, function(data) {
      score <- data$PRS
      labels <- ifelse(data$diagnosis == 0, 0, 1)
      t <- pr.curve(
        scores.class0 = score,
        weights.class0 = labels,
        curve = TRUE,
        sorted = FALSE,
        max.compute = TRUE,
        min.compute = TRUE,
        rand.compute = TRUE
        )
      baseline <- sum(labels) / length(labels)
      baselines <<- c(baselines, baseline)
      data.frame(Precision = t$curve[, 2], Recall = t$curve[, 1], AUPRC = t$auc.integral)
    })
    names(pr_curve_df) <- c("W0", "W1", "W2")
    p2 <- rbind(
      data.frame(
        Recall = pr_curve_df$W0$Recall,
        Precision = pr_curve_df$W0$Precision,
        wave = "W0",
        Baseline = baselines[1],
        cor = "#65ADC2"
      ),
      data.frame(
        Recall = pr_curve_df$W1$Recall,
        Precision = pr_curve_df$W1$Precision,
        wave = "W1",
        Baseline = baselines[2],
        cor = "#233B43"
      ),
      data.frame(
        Recall = pr_curve_df$W2$Recall,
        Precision = pr_curve_df$W2$Precision,
        wave = "W2",
        Baseline = baselines[3],
        cor = "#E84646"
      )
    ) %>%
      ggplot(aes(x = Recall, y = Precision, color = cor)) +
        geom_path(linewidth = 1) +
        geom_hline(aes(yintercept = Baseline, color = cor), linetype = "dashed") +
        scale_color_discrete(name = "Wave", labels = c("W0", "W1", "W2")) +
        labs(
          x = "Recall", y = "Precision",
          title = glue("Precision-Recall Curve ADHD - N = {nrow(data)/3}"),
          subtitle = glue("
      W0 = {format(unique(pr_curve_df$W0$AUPRC)*100, digits = 4)}% W1 = {format(unique(pr_curve_df$W1$AUPRC)*100, digits = 4)}% W2 = {format(unique(pr_curve_df$W1$AUPRC)*100, digits = 4)}%
      "),
      caption = "20-03-2024_cass_BHRC_PRS_eval.R"
    ) +
      theme(
        text = element_text(family = font),
        axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_blank()
    )

    ## Calculate R2
    R2 <- do.call(bind_rows, lapply(processing, function(df){
      PseudoR2(glm(PRS ~ diagnosis, data = df), which = "Nagelkerke") * 100
    })) %>%
    as.data.frame() %>%
    bind_cols(
      data.frame(
        c(
        roc_results$W0$auc * 100,
        roc_results$W1$auc * 100,
        roc_results$W2$auc * 100
        ),
        c(
        unique(pr_curve_df$W0$AUPRC * 100),
        unique(pr_curve_df$W1$AUPRC * 100),
        unique(pr_curve_df$W2$AUPRC * 100)
        ))
      )
    colnames(R2) <- c("Nagelkerke", "AUROC", "AUPRC")
    rownames(R2) <- c("W0", "W1", "W2")
  list(p1, p2, R2)
}

## by wave
evaluate_PRS("ADHD", "dcanyhk")
opt <- evaluate_PRS("ADHD", "dcanyhk")
## by sex


## all time