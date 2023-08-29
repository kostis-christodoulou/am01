library(tidyverse)
library(mosaic)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/

# load data set
cvr_comparison <- read_csv(here::here('data', 'cvr_comparison.csv'))

# summary statistics
mosaic::favstats(convert ~ layout, data = cvr_comparison)

# run a t.test
t.test(convert ~ layout, data = cvr_comparison)

# t = 2.0788, df = 158.57, p-value = 0.03924
# t > 2, p-value < 0.05, so we cvan reject the hypothesis that the two CVRs are the same


# alternative hypothesis: true difference in means between group new and group old is not equal to 0
# 95 percent confidence interval:
#   0.004742797 0.185257203

# 95 CI for difference does *NOT* include 0, so again we are almost certain that the two CVRs are not the same.