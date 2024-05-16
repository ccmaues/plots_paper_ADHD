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