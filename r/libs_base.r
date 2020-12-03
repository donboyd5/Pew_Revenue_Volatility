
library(magrittr)
library(plyr) # needed for ldply; must be loaded BEFORE dplyr
library(tidyverse)
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library(scales)
library(hms) # hms, for times
library(lubridate) # lubridate, for date/times
library(vctrs)

library(broom) # for automating the cleanup of complex output
library(modelr)
library(janitor)

library(grDevices)
library(knitr)
library(kableExtra)

library(usmap)
library(btools)
library(bdata)
library(BEAData)