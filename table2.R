#setwd("/media/santorolab/C207-3566")
source("data_to_source.R")
source("functions_to_source.R")

prs_data <-
	data %>%
	select(IID, orignal_PRS, zscore_PRS, adjusted_PRS) %>%
	unique()

library(psych)
opt <- rbind(
	describe(prs_values$original_PRS),
	describe(prs_values$zscored_PRS),
	describe(prs_values$adjusted_PRS))
rownames(opt) <- c("Original", "Z-score", "Adjusted")

writexl::write_xlsx(opt, "table2.xlsx")
