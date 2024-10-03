data <- readRDS("E:/objects_R/cass_BHRC_26092024.RDS")

pacman::p_load(VIM, mice, naniar, dplyr, tidyr)

# only analyze the ones longitudinal and of inteterest
# site, gender, and birthday are all at w0
# d_date maybe redudant over diagnosis (but they differ, apparently)
m <- select(data, ADHD, d_date, PRS)  %>%
  mutate_if(is.character, as.factor)

anyNA(m)
# over all data
paste0(round(prop_miss(m) * 100, digits = 3), "%")
# per variable
miss_var_summary(m) %>% knitr::kable("simple")
summary(m) %>% knitr::kable("simple")
aggr(m, numbers = TRUE, prop = FALSE)
md.pattern(m, plot = FALSE)
# pick t with missing variables only
dummyNA <- as.data.frame(abs(is.na(m)))
cor(dummyNA) %>% round(digits = 2) * 100
# pick all variables
cor(m %>% mutate_if(is.factor, as.numeric),
  dummyNA,
  use = "pairwise.complete.obs") %>%
  round(digits = 2)

# test what type of missingness is in our t
mcar_test()
# falta a idade