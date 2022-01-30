
# As of tidyverse 1.3.0, the following packages are included in the core tidyverse:
# ggplot2. ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics. ...
# dplyr
# tidyr
# readr
# purrr
# tibble
# stringr
# forcats


# library(magrittr)
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats
# library(purrr)
library(fs)
# library(vroom) # do not load vroom, it causes too many problems, but make sure it is installed
library(readxl)
library(openxlsx)
library(data.table)
library(archive)

library(scales)
# library(hms) # hms, for times automatically loaded
# library(lubridate) # lubridate, for date/times, automatically loaded
library(vctrs)

library(maps)
library(gt)
library(gtsummary)

# remotes::install_github("GuangchuangYu/ggbreak")  # not found
# remotes::install_github("YuLab-SMU/ggbreak")  # good
library(ggbreak) 
library(patchwork)

library(broom) # for automating the cleanup of complex output
library(modelr)
library(janitor)

library(grDevices)
library(gridExtra)
library(knitr)
library(kableExtra)

library(usmap)

# devtools::install_github("correlatesstatepolicy/cspp")
# library(cspp)
library(csppData)


# Boyd libraries ----
library(bdata)
library(btools)
library(BEAData)
library(BLSdata)
library(qtax)
