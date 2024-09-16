setwd("C:/Users/cassi/OneDrive/Área de Trabalho/github_files/plots_paper/")
source("data_to_source.R")
source("functions_to_source.R")

### Get individual R2 for diagnosis
m1 <- DescTools::PseudoR2(
	glm(diagnosis ~ sex, data = data, family = "binomial"),
	which = "Nagelkerke"
) * 100
m2 <- DescTools::PseudoR2(
	glm(diagnosis ~ adjusted_PRS, data = data, family = "binomial"),
	which = "Nagelkerke"
) * 100
m3 <- DescTools::PseudoR2(
	glm(diagnosis ~ popID, data = data, family = "binomial"),
	which = "Nagelkerke"
) * 100
m4 <- DescTools::PseudoR2(
	glm(diagnosis ~ wave, data = data, family = "binomial"),
	which = "Nagelkerke"
) * 100
m5 <- DescTools::PseudoR2(
	glm(diagnosis ~ age, data = data, family = "binomial"),
	which = "Nagelkerke"
) * 100
m6 <- DescTools::PseudoR2(
	glm(diagnosis ~ p_diagnosis, data = data, family = "binomial"),
	which = "Nagelkerke"
) * 100

for_plot <- data.frame(
	Variables = c(
	"Sex", "PRS", "State", "Wave",
	"Age", "Parent\nDiagnosis"),
	Explained_variance = c(m1, m2, m3, m4, m5, m6)) %>%
mutate(Variables = 
	factor(Variables, levels = Variables[order(Explained_variance, decreasing = TRUE)])
)

ggthemr("grape")
ggplot(for_plot, aes(x = Variables, y = Explained_variance, fill = Variables)) +
	geom_col() +
  scale_y_continuous(n.breaks = 12) +
  labs(
    y = "\n% of diagnosis explained\n",
    x = ""
  ) +
	theme_publish() +
  theme(
    text = element_text(size = 7),
    axis.text = element_text(size = 7),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

ggsave(
  "figure10.png",
  height = 100,
  width = 85,
  units = "mm",
  device = "png"
  )
