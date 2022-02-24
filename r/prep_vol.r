

# libraries ---------------------------------------------------------------
source(here::here("r", "libs_base.r"))
source(here::here("r", "libs_ts.r"))
devtools::session_info()

# constants ---------------------------------------------------------------
source(here::here("r", "constants_system.r"))
source(here::here("r", "constants_analysis.r"))

#.. define volatility vars ----
vtax <- c("tottax", "iit", "gst", "selsalestax", "cit", "sevtax", "othertax")
vnontax <- c("gdp", "capgains")
volvars <- c(vtax, vnontax)

# functions -----------------------------------------------------------
source(here::here("r", "functions", "functions_runs.r"))
source(here::here("r", "functions", "functions_measures.r"))

#.. function create summary vol measures for time period and set of vars ----

f_vol <- function(startend, vars, vbase){
  # get vars that represent the years used
  year1 <- startend[1]
  year2 <- startend[2]
  period <- paste0(year1, "-", year2)
  print(period)
  
  # vol1 is our starting point for creating measures
  # it has first difference of log values
  vol1 <- vbase %>%
    filter(name %in% vars,
           # initially include year before start to get lagged value
           year %in% (year1 - 1):year2) %>% 
    # bring in gdp for each state
    left_join(vbase %>% 
                filter(name=="gdp") %>%
                select(stabbr, year, realnom, gdp=value),
              by=c("stabbr", "year", "realnom")) %>%
    group_by(stabbr, name, realnom) %>%
    arrange(year) %>%
    # get first difference of log values
    mutate(dlvalue=dl(value),
           dlgdp=dl(gdp)) %>%
    # get rid of unnecessary year before period start
    filter(year >= year1) %>%
    nest() %>%
    ungroup
  
  # create model-based measures
  volmods <- vol1 %>%
    # use "safely" because some state-tax combinations might be bad -- for example,
    # having a zero value for tax revenue which, when we take the log, became NA
    mutate(sremod = purrr::map(data, 
                               safely(function(df) lm(dlvalue ~ dlgdp, data=df))))
  
  # sre -- short-run elasticity (coefficient on dlgdp)
  #   it is, essentially, the percent change in tax wrt % change in gdp
  # srese -- standard error of sre
  # unpack the summary measures from sremod to get:
  #   coefficient (short-run elasticity, sre), and
  #   its standard error
  # this will generate warnings where we have models that are not proper
  # we will weed them out later
  # this can be time consuming
  sredf <- volmods %>%
    select(stabbr, name, realnom, sremod) %>%
    mutate(result=purrr::map(sremod, "result"),
           tidied=purrr::map(result, tidy),
           stats=purrr::map(result, glance)) %>%
    # pull specific statistics out of stats
    hoist(stats, nobs="nobs", dfr="df.residual", r2adj="adj.r.squared") %>%
    select(-c(sremod, result)) %>%
    unnest(cols=tidied) %>%
    filter(term=="dlgdp") %>%
    select(stabbr, name, realnom, nobs, dfr, r2adj, sre=estimate, srese=std.error)
  # note - we lost MO sevtax; it has some junky values like 0, 1, etc.
  # count(sredf, dfr)
  
  # get the other volatility measures and combine them with sredf
  volmeas <- vol1 %>%
    # calc the other volatility measures
    select(stabbr, name, realnom, data) %>%
    unnest(cols=data) %>% # data won't be in the output
    group_by(stabbr, name, realnom) %>% 
    summarise(
      # use median and IQR to calculate robust measures of growth and volatility
      growthmdn=median(pch, na.rm=TRUE),
      pdtrendiqr=IQR(pdtrend, na.rm=TRUE),
      pdtrend25=p25(pdtrend, na.rm=TRUE),
      pdtrend75=p75(pdtrend, na.rm=TRUE),
      pchiqr=IQR(pch, na.rm=TRUE),
      pch25=p25(pch, na.rm=TRUE),
      pch75=p75(pch, na.rm=TRUE),
      dpchtrendiqr=IQR(dpchtrend, na.rm=TRUE),
      # standard deviation of growth - more traditional, lest robust
      pchsd=sd(pch, na.rm=TRUE),
      .groups="drop") %>%
    ungroup 
  
  # put the pieces back together:
  #   vol1 -- nested data used to calculate volatility
  #   sredf -- model-based short-run elasticity estimates
  #   volmeas -- other summary measures
  
  vol <- vol1 %>%
    # bring in the model-based volatility measures
    left_join(sredf, by = c("stabbr", "name", "realnom")) %>%
    left_join(volmeas, by = c("stabbr", "name", "realnom")) %>%
    mutate(namef=factor(name,
                        levels=taxnames$name,
                        labels=taxnames$namef),
           year1=!!year1,
           year2=!!year2,
           period=!!period) %>%
    select(stabbr, name, namef, year1, year2, period, data, everything()) %>%
    arrange(namef)
  
  vol
}


# load vbase --------------------------------------------------------------
vbase <- readRDS(here::here("data", "vbase.rds"))


# calc volatility measures for decades and for 2000-2020 ----

# vbase %>% filter(stabbr == "AZ", name == "sevtax", realnom == "nominal")

# get several volatility time periods with different start and end years
# create a list of start-end pairs for which we want volatility estimates
ylist <- list(
  c(1970, 2020),
  c(2000, 2020),
  c(1960, 1969),
  c(1970, 1979),
  c(1980, 1989),
  c(1990, 1999),
  c(2000, 2009),
  c(2010, 2019)
)
volall <- map_dfr(ylist, f_vol, vars=volvars, vbase=vbase)
saveRDS(volall, here::here("data", "volall.rds"))

glimpse(volall)
count(volall, period)
count(volall, stabbr)
count(volall, name, namef)
count(volall, realnom)

volall %>% filter(stabbr=="US", name=="tottax", realnom=="nominal")



# calc volatility measures for recessions ---------------------------------

# create a list of start-ends for recession periods
recessions

# create a list of start-end pairs for which we want volatility estimates
recyears <- c(1969, 1973, 1980, 1990, 2001, 2007)
plusminus <- 5

recylist <- purrr::map(recyears, function(y) c(y - plusminus, y + plusminus))
recylist
# create vector of periods that we can map back to recyears
recperiods <- purrr::map_chr(recylist, function(years) paste0(years[1], "-", years[2]))
cbind(recyears, recperiods)

volrecs <- map_dfr(recylist, f_vol, vars=volvars, vbase=vbase)
volrecs <- volrecs %>%
  mutate(recyear=factor(period, levels=recperiods, labels=recyears),
         recyear=as.integer(as.character(recyear)))
glimpse(volrecs)
count(volrecs, recyear, period)  
saveRDS(volrecs, here::here("data", "volrecs.rds"))


