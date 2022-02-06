
# As of tidyverse 1.3.0, the following packages are included in the core tidyverse:
# ggplot2
# dplyr
# tidyr
# readr
# purrr
# tibble
# stringr
# forcats

# base libraries ----
# library(magrittr)
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
# library(purrr)
library(slider) # part of tidyverse
library(fs)
library(vctrs)


# input and output ----
# library(vroom) # do not load vroom, it causes too many problems, but make sure it is installed
library(readxl)
library(openxlsx)
library(data.table)
library(archive)


# graph tools ----
library(scales)
library(grDevices)
library(gridExtra)
# remotes::install_github("YuLab-SMU/ggbreak")  # good
library(ggbreak) 
library(ggrepel)
library(patchwork)
library(RColorBrewer)
library(viridis)
# library(hms) # hms, for times automatically loaded
# library(lubridate) # lubridate, for date/times, automatically loaded

# tables ----
library(gt)
library(gtsummary)
library(janitor)
library(knitr)
library(kableExtra)


# maps ----
library(maps)
library(usmap)


# modeling ----
library(broom) # for automating the cleanup of complex output
library(modelr)

# miscellaneous ----
# devtools::install_github("correlatesstatepolicy/cspp")
# library(cspp)
library(csppData)


# FRED ----
library(fredr)
fred_apikey <- "a5e1199baac333154cbffcba3b263c28"
fredr_set_key(fred_apikey)


# Boyd libraries ----
library(bdata)
library(btools)
library(BEAData)
library(BLSdata)
library(qtax)
