

corr_tidy <- function(vars){
  # return a tidy data frame with correlations
  mat <- as.matrix((vars))
  Hmisc::rcorr(mat) %>%
    tidy
}

dl <- function(vec) {
  # difference in log values
  # ASSUME data are sorted by year and there are no gaps and no zeros
  lvec = log(vec)
  laglvec = c(NA, head(lvec, -1))
  lvec - laglvec
}

episodes <- function(x, f="mean"){
  # create measures of how long and severe episodes are
  # x: a vector of deviations from trend, in date order
  # returns: list(len, asum) where
  #   len is the mean length of episodes that are above or below trend
  #   asum is the mean of the sums, across episodes, of absolute deviations from trend
  
  if(f=="mean") {
    f <- mean
  } else if(f=="median"){
    f <- median
  } else print("ERROR!!!")
  
  
  df <- tibble(x) %>%
    mutate(group = data.table::rleid(sign(x))) %>%
    group_by(group) %>%
    summarise(len=n(), asum=sum(abs(x), na.rm=TRUE),
              .groups="drop") %>%
    summarise(len=f(len), asum=f(asum), .groups="drop")
  
  list(elength=df$len, esum=df$asum)
}


# hpdf <- function(data, colname="value") {
#   # when type="lambda" freq is the lambda parameter for the HP filter
#   # 6.25 is commonly recommended for annual data; see:
#   # Ravn, Morten O., and Harald Uhlig. “On Adjusting the Hodrick-Prescott Filter for the Frequency of Observations.” Review of Economics and Statistics 84, no. 2 (May 2002): 371–76. https://doi.org/10.1162/003465302317411604.
#   mod <- hpfilter(data[colname], freq=6.25, type=c("lambda"), drift=FALSE)
#   tibble(trend=as.numeric(mod$trend),
#          cycle=as.numeric(mod$cycle))
# }


hptrend <- function(x, type="lambda", smooth=6.25, minlen=10){
  # Hodrick-Prescott (HP) filter
  # x: vector of time series data, in order, no missing values
  # returns: trend vector
  
  # this is intended to be robust in the sense that:
  #   it trims any leading or trailing NA values - but keeps track of them
  #   it approximates any interior NA values so that hpfilter will work
  #   it adds back any leading or trailing NA values to the trend
  
  # when type="lambda" freq is the lambda parameter for the HP filter
  # 6.25 is commonly recommended for annual data; see:
  
  # Ravn, Morten O., and Harald Uhlig. “On Adjusting the Hodrick-Prescott Filter
  # for the Frequency of Observations.” Review of Economics and Statistics 84,
  # no. 2 (May 2002): 371–76. https://doi.org/10.1162/003465302317411604.
  
  if(sum(x > 0, na.rm=TRUE) < minlen) return(NA_real_)
  
  # if statement allows HP for a percent change where first element is NA
  if(is.na(x[1])){
    vts <- ts(x[-1])
  } else vts <- ts(x)
  
  # fill any missing values in the interior via interpolation
  # do NOT fill values on the ends
  vts_filled <- zoo::na.approx(vts, na.rm=FALSE)
  
  # remove any leading or trailing NA values and keep track
  vts_ltrim <- zoo::na.trim(vts_filled, sides="left")
  na_left <- length(vts_filled) - length(vts_ltrim)
  
  vts_fulltrim <- zoo::na.trim(vts_ltrim, sides="right")
  na_right <- length(vts_ltrim) - length(vts_fulltrim)

  # this should always work as we have a time series with no NA values
  vts_hp <- hpfilter(vts_fulltrim, freq=smooth, type=type, drift=FALSE)
  
  trend_trimmed <- as.numeric(vts_hp$trend)
  # addback the trimmed NA values
  trend <- c(rep(NA_real_, na_left),
             trend_trimmed,
             rep(NA_real_, na_right))
  
  if(is.na(x[1])){
    trend <- c(NA, trend)
  } 
  trend
}


sre <- function(data) {
  # short-run elasticity relative to national gdp
  mod = lm(dlvalue ~ dlgdp, data=data)
  list(mod=mod, coeff=unname(mod$coefficients["dlgdp"]))
}


sre_gsp <- function(data) {
  # short-run elasticity with both national and state gdp log differences on rhs
  mod = lm(dlvalue ~ dlgdp + dlgsp, data=data)
  list(mod=mod, coeff=unname(mod$coefficients["dlgdp"]))
}


rollsre <- function(y, x, nobs) {
  # rolling short run elasticity
  # print("in rollsre")
  sremod <- function(df){
    # print("in sremod")
    mod <- lm(y ~ x, na.action=na.exclude, data=df)
    sre <- coef(summary(mod))[2, "Estimate"]
    sre
  }
  safe_sremod <- safely(sremod)
  # print("before df")
  df <- data.frame(y=y, x=x)
  # print("after df")
  zoo::rollapply(df, nobs, function(df) sremod(as.data.frame(df)),
                 by.column = FALSE,
                 fill=NA, align="right")
}


get_measures <- function(df){
  # df data frame with:
  #     stabbr
  #     name
  #     realnom
  #     year
  #     value
  #     trend
  #     pch
  #     pdtrend
  #   assumed to already be winnowed down to desired states, names, years
  
  vol_basic <- df %>%
    group_by(stabbr, realnom, name) %>%
    summarise(n=n(),
              pchsd = sd(pch, na.rm=TRUE),
              hpsd = sd(pdtrend, na.rm=TRUE),
              .groups = "drop")
  # vol_basic
  
  # now construct the more complicated measures
  vol_other <- df %>%
    # bring in real gdp or nominal gdp from vbase, which must exist
    left_join(vbase %>% 
                filter(name=="gdp") %>% 
                select(stabbr, realnom, year, gdp=value),
              by=c("stabbr", "realnom", "year")) %>%
    group_by(stabbr, name, realnom) %>%
    mutate(dlvalue=dl(value), dlgdp=dl(gdp)) %>%
    nest() %>%
    # note that this does not filter out the case where GDP is on the lhs and
    # the rhs so we will get warnings for that! they will arise in the next step
    # when we unpack
    mutate(sremod = purrr::map(data, 
                               safely(function(df) lm(dlvalue ~ dlgdp, data=df))),
           epi = purrr::map(data, 
                            safely(function(df) episodes(df$pdtrend)))) %>%
    ungroup
  # vol_other
  # vol_basic
  
  # sre -- short-run elasticity -- unpack into sre and sresum unpack the summary
  # measures from sremod to get coefficient (short-run elasticity, sre) and its
  # standard error
  sredf <- vol_other %>%
    select(stabbr, name, realnom, data, sremod) %>% 
    # we want to keep data from one set
    unnest_wider(col=sremod) %>%  # we need the column "result"
    mutate(tidied=purrr::map(result, tidy)) %>%
    unnest(cols=tidied) %>%
    filter(term=="dlgdp") %>%
    select(stabbr, name, realnom, data, sre=estimate, srese=std.error)
  # the warnings are for gdp on lhs and rhs
  
  # epi -- episodes -- unpack into elength and esum
  epidf <- vol_other %>%
    select(stabbr, name, realnom, epi) %>%
    unnest_wider(col=epi) %>%
    unnest_wider(col=result)
  
  # unpack the summary measures from sregspmod to get coefficient (short-run
  # elasticity, sre) and its standard error
  measures <- vol_basic %>%
    left_join(sredf, by = c("stabbr", "name", "realnom")) %>%
    left_join(epidf, by = c("stabbr", "name", "realnom")) %>%
    select(stabbr, name, realnom, data, everything())
  measures
}
