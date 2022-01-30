

library(zoo) # for rollapply
library(tseries) # for as.ts()
library(timetk)
library(tsbox)
library(broom) # for automating the cleanup of complex output
library(tidyquant)
library(sweep)

library(changepoint) # shift analysis
library(tsoutliers) # for automating detection of outliers and shifts
library(RcppRoll)
library(locfit)
library(npreg)

# library(forecast) # for auto.arima
library(fable)
library(lmtest) # for coeftest
library(sandwich) # for Newey-West Standard Errors

library(mFilter)  # Hodrick-Prescott filter

library(ggpmisc)

# library(plm) # panel linear models -- possibly to be used later