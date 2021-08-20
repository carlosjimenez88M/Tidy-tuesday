#================================#
#         Tidytuesday #31        #
#          The Olympics          #
#           2021-08-19           #
#================================#


## libraries ---------------
library(tidyverse)


## Data ---------

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

## EDA -----------

olympics %>%
    head()
