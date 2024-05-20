source("functions_to_source.R")
source("data_to_source.R")

# used tutorials:
## https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
## https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
## http://www.sthda.com/english/wiki/survival-analysis-basics

# need data of evaluation
# phenotype by wave
# want age (x-axis)
# and sex (grouping)

# for the time part, I need to let it be in one
# scale. I am thinking of putting in days, just
# like the tutorials.

# phenotype codes must be 0 and 1s
# censored = control

# https://epirhandbook.com/en/working-with-dates.html
# Working with the dates:
# fazer com as datas de nascimento ou só com o tempo da coorte?
library(lubridate)

mirrow <-
  readRDS(glue("{Path}/0_external_files/Lucas_Keep2190_BHRCS.rds")) %>%
  select(ident, IID) %>%
  mutate(ident = as.numeric(ident))


# data is so broken that looks like my head
# need to remove NAs
dates <-
  readRDS(glue("{Path}/0_external_files/dates.rds")) %>%
  select(ident, redcap_event_name, d_date) %>%
  mutate(
    redcap_event_name = case_when(
      redcap_event_name == "wave0_arm_1" ~ "W0",
      redcap_event_name == "wave1_arm_1" ~ "W1",
      redcap_event_name == "wave2_arm_1" ~ "W2")) %>%
  rename(wave = redcap_event_name) %>%
  inner_join(mirrow, ., by = "ident") %>%
  inner_join(., pheno[, c("IID", "dcanyhk", "wave")], by = c("IID", "wave")) %>%
  inner_join(., sex, by = "IID") %>%
  select(-ident) %>%
  drop_na()

dates %>%
  group_by(IID) %>%
  slice(which.min(d_date)) %>%
  ungroup()

dates$d_date <- as.Date(dates$d_date)
start_date <- min(dates$d_date)
dates$time <- as.numeric(dates$d_date - start_date)


# follow up time
# pivor_wider by wave


library(survival)
fit <- survfit(Surv(time = dates$time, event = dates$dcanyhk == 2) ~ sex, data = dates)

survminer::ggsurvplot(
   fit, 
   data = dates,          # again specify the data used to fit linelistsurv_fit_sex 
   conf.int = TRUE,              # do not show confidence interval of KM estimates
   surv.scale = "percent",        # present probabilities in the y axis in %
   break.time.by = 360,            # present the time axis with an increment of 30 days
   xlab = "Follow-up days",
   ylab = "Survival Probability",
   pval = TRUE,                   # print p-value of Log-rank test 
   pval.coord = c(40, .91),       # print p-value at these plot coordinates
   risk.table = TRUE,             # print the risk table at bottom 
   legend.title = "Gender",       # legend characteristics
   legend.labs = c("Female", "Male"),
   font.legend = 10,
   palette = "Dark2",             # specify color palette 
   surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
   ggtheme = theme_publish()        # simplify plot background
)

# method 2
dates$d_date <- as.Date(dates$d_date)
dates$IID <- as.character(dates$IID)

# Aggregating data
dates_agg <- dates %>%
  group_by(IID) %>%
  summarise(
    d_date = ifelse(any(dcanyhk == 2), min(d_date[dcanyhk == 2], na.rm = TRUE), max(d_date, na.rm = TRUE)),
    dcanyhk = ifelse(any(dcanyhk == 2), 2, 0),
    sex = first(sex)
  ) %>%
  ungroup()

# Calculate time in days from the earliest date
start_date <- min(dates_agg$d_date)
dates_agg$time <- as.numeric(dates_agg$d_date - start_date)

# Create the Surv object and fit the survival curve
surv_object <- Surv(time = dates_agg$time, event = dates_agg$dcanyhk == 2)
fit <- survfit(surv_object ~ sex, data = dates_agg)

# Plot the survival curve
survminer::ggsurvplot(
   fit, 
   data = dates_agg,           
   conf.int = TRUE,            
   surv.scale = "percent",     
   break.time.by = 360,         
   xlab = "Follow-up days",
   ylab = "Cumulative Probability of Diagnosis",
   pval = TRUE,                
   pval.coord = c(300, .91),   
   risk.table = TRUE,          
   legend.title = "Gender",    
   legend.labs = c("Female", "Male"),
   font.legend = 10, 
   palette = "Dark2",          
   surv.median.line = "hv",    
   ggtheme = theme_light(),
   fun = "event"
)
