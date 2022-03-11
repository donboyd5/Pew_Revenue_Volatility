
# sources for top income tax rates ----
# https://users.nber.org/~taxsim/state-rates/  1977-2018
# https://users.nber.org/~taxsim/state-rates/maxrate.dat  # 2nd column of data is top state rate
# https://users.nber.org/~taxsim/state-rates/maxrate.html  # looks like html version of same
# tpc https://www.taxpolicycenter.org/statistics/state-individual-income-tax-rates 2000-2021
#   https://www.taxpolicycenter.org/file/186568/download?token=mYtsqZlA


# libraries ----

source(here::here("r", "libs_base.r"))
library(cspp)
# source(here::here("r", "functions.r"))

# Part A: NBER 1977-2018 ----
# https://users.nber.org/~taxsim/state-rates/
#
# Available here is a list of tax rates by year and state for the maximum state
# tax. It is calculated from a run of the TAXSIM model. Note that the top
# federal rate varies across states because of the deductibility of state income
# taxes on the federal tax.

# This calculation was originally requested by Josh Lerner of the Harvard
# Business School, and reports the maximum tax rate for an additional $1000 of
# income on an initial $1,500,000 of wage income (split evenly between husband
# and wife). The taxpayer is assumed to be married and filing jointly and with
# $10,000 of long term capital gains. A mortgage interest deduction of $150,000
# and the calculated state income tax are present as personal deductions. To
# avoid a simultaneous determination, the federal and state calculation is
# iterated to a fixed point. (Note, in real life each taxing authority allows a
# deduction only for taxes paid to the other, not tax liability, which avoids
# any simultaneity). This deductibility would be the main difference between
# these rates and the maximum bracket rate which might be published in a summary
# of state tax laws.

# The rates shown include the interaction of the regular bracket rates with the
# phaseout of exemptions and itemized deductions, and many other features of the
# tax law, but not the minimum tax which by 2003 was affecting our synthetic
# taxpayer if and only if he lived in Oregon or Washington, DC. Suggestions for
# treating the AMT are welcome. It will always reduce the marginal rate, but
# generally not apply to the highest income taxpayers, so it doesn't seem
# exactly right to include it here. These are rates for very high income
# taxpayers rather than the maximum rates (which may be very high at moderate
# income levels in phaseout ranges). These phaseouts explain why there are so
# few round numbers in the table.



# Here are the column labels for the ASCII version:
# Year
# State ID
# Federal Rate, Wages
# State Rate, Wages
# Total Rate, Wages
# Federal Rate, Long Gains
# State Rate, Long Gains
# Total Rate, Long Gains
# Federal Rate, Mortgage Deduction
# State Rate, Mortgage Deduction
# Total Rate, Mortgage Deduction
# State Name
# https://users.nber.org/~taxsim/state-rates/maxrate.dat  # 2nd column is top state rate
# 1977     0    70.00     0.00    70.00    25.00     0.00    25.00   -70.00     0.00   -70.00 federal         
# 1977     1    68.91     1.55    70.47    24.09     1.30    25.39   -72.54     3.63   -68.91 Alabama         
# 1977     2    59.85    14.50    74.35    19.93     7.25    27.18   -70.00     0.00   -70.00 Alaska    

# get NBER maxrate data previously downloaded from:
#   https://users.nber.org/~taxsim/state-rates/maxrate.dat

cols <- fwf_cols(year = c(2, 5), 
                 stcen = c(10, 11), 
                 frate = c(15, 20), 
                 srate = c(24, 30), 
                 srate_gains = c(51, 56),
                 stname = c(94, 110))
ctypes <- "iiddc"
nber1 <- read_fwf(here::here("data", "raw_data", "nber", "maxrate.dat"), 
               col_positions=cols,
               col_types = ctypes)

nber1
glimpse(nber1)
nber1 %>% filter(stname=="New York")  # sanity check

nber2 <- nber1 %>%
  left_join(stcodes %>% select(stabbr, stcen) %>% mutate(stcen=as.integer(stcen)),
            by=c("stcen"))
nber2
count(nber2, stabbr, stcen, stname)  # all state abbreviations look good

count(nber2, year)

# traditional non pit states
nonpit <- c("AK", "FL", "NV", "NH", "TN", "SD", "TX", "WA", "WY")
iit_rates_nber <- nber2 %>%
  filter(stabbr %in% state.abb, !stabbr %in% nonpit) %>%
  select(stabbr, year, iit_toprate=srate, srate_gains)

iit_rates_nber %>% filter(stabbr=="HI")
summary(iit_rates_nber)

saveRDS(iit_rates_nber, here::here("data", "details", "iit_rates_nber.rds"))


# Part B: TPC 2019 and 2020, but check all years ----
# get TPC income tax rate data previously downloaded manually from
#    https://www.taxpolicycenter.org/statistics/state-individual-income-tax-rates
# (manual because the url they give does not seem to work)
fpath <- here::here("data", "raw_data", "tpc", "state_income_tax_rates_0.xlsx")

f <- function(year){
  print(year)
  if(year != 2013){
    cols <- cell_cols("A:D")
    cnames <- c("stname", "lowrate", "junk", "highrate")
  } else if(year == 2013) {
    cols <- cell_cols("A:E")
    cnames <- c("stname", "lowrate", "junk1", "junk2", "highrate")
  }
  df <- read_excel(fpath, sheet=as.character(year),
                   range=cols, col_names=cnames, col_types="text")
  df %>%
    select(stname, lowrate, highrate) %>% 
    mutate(year=year)
}
# f(2013)

years <- 2000:2021
df <- purrr::map_dfr(years, f)

# clean the state name and keep states and DC
df2 <- df %>%
  mutate(stname2=stname,
         stname2=gsub("\\(.*)", "", stname2),  # get rid of parentheses and text inside
         stname2=str_remove_all(stname2, "[0-9]"),  # get rid of numbers
         stname2=str_to_upper(stname2) %>% str_trim(),
         stname2=ifelse(stname2=="DIST. OF COLUMBIA", "DISTRICT OF COLUMBIA", stname2),  # fix an apparent issue
         stabbr=stabbr(stname2))

df2 %>% filter(str_detect(stname2, "ARKANS")) %>% select(stname, stname2)


check <- count(df2, stabbr, stname2, stname)  # inspect check
count(df2, stabbr) %>%
  filter(n!=22)
check2 <- check %>% filter(is.na(stabbr))  # inspection shows that none of these is a problem

df3 <- df2 %>%
  filter(!is.na(stabbr)) %>%
  select(stabbr, stname=stname2, year, lowrate, highrate)
# compare state abbreviations and state names as an additional check
count(df3, stabbr, stname)  # all good, 50 states plus DC, we can drop stname

#.. now clean up the rates ----
df4 <- df3 %>%
  filter(!year %in% 2005:2006) %>%  # drop 2005-2006 because TPC gave full rate schedules of married and singles
  select(stabbr, year, lowrate, highrate) %>%
  mutate( # get rid of parentheses and text inside
    lowrate=gsub("\\(.*)", "", lowrate),
    highrate=gsub("\\(.*)", "", highrate),
    lowraten=as.numeric(lowrate),
    highraten=as.numeric(highrate))

#.... identify problems and issues -- start with specific states with known issues ----

# traditional non-PIT states over this period:
nonpit <- c("AK", "FL", "NV", "NH", "TN", "SD", "TX", "WA", "WY")
df4 %>% filter(stabbr==nonpit[7])
# AK, FL, NV, SD, TX, WA, WY missing for all - ok
# NH 5 for lowrate in 2000 - ???
# TN 6 for lowrate and highrate in 2000 - ???

# look for additional states with suspect numbers
df4 %>%
  filter(!stabbr %in% nonpit, is.na(highraten)) %>%
  arrange(stabbr, year)
# states that only have a lowrate but not a high rate

# make adjustments for these issues and check for more
df5 <- df4 %>%
  mutate(lowraten=ifelse(stabbr %in% nonpit, NA_real_, lowraten),
         highraten=ifelse(stabbr %in% nonpit, NA_real_, highraten),
         highraten=ifelse(is.na(highraten) & !is.na(lowraten), lowraten, highraten))

df5 %>%
  filter(!stabbr %in% nonpit, is.na(highraten)) %>%
  arrange(stabbr, year)

# the remaining issues are all states that use % of federal taxable liability, except MA in 2000
# MA in 2000
df5 %>% filter(stabbr=="MA", year >= 2000)  # NA, 5.6 in 2000, 2001
iit_rates_nber %>% filter(stabbr=="MA", year >= 2000)  # 5.85, 5.6 in 2000, 2001

# states that are % of federal tax liability
df5 %>% filter(stabbr=="CO")  # CO was listed as 5% of FTI in 2000 and flat rate in later years
iit_rates_nber %>% filter(stabbr=="CO", year >= 2000)

df5 %>% filter(stabbr=="RI")  # RI was listed as 26.5% to 25% of FTI in 2000-2008
iit_rates_nber %>% filter(stabbr=="RI", year >= 2000) # calc'd top rate in 2000-2008

# save the tpc rates
iit_rates_tpc <- df5 %>%
  select(stabbr, year, iit_toprate=highraten)
summary(iit_rates_tpc)

saveRDS(iit_rates_tpc, here::here("data", "details", "iit_rates_tpc.rds"))

# PART C: combine nber and tpc, compare, then save ----
iit_rates_nber <- readRDS(here::here("data", "details", "iit_rates_nber.rds"))

df1 <- readRDS(here::here("data", "details", "iit_rates_nber.rds")) %>%
  mutate(type="nber")

df2 <- readRDS(here::here("data", "details", "iit_rates_tpc.rds")) %>%
  mutate(type="tpc")

df3 <- bind_rows(df1, df2) %>%
  select(stabbr, year, iit_toprate, type) %>%
  arrange(stabbr, type, year)

# compare rates
stnums <- 1:12
stnums <- 13:24
stnums <- 25:36
stnums <- 37:48
stnums <- 49:length(state.abb)

sts <- state.abb[stnums]
df3 %>% 
  filter(stabbr %in% sts) %>%
  ggplot(aes(year, iit_toprate, colour=type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~stabbr, ncol=3, scales="free")


st <- "NY"
st <- "HI"
df3 %>% 
  filter(stabbr==st) %>%
  ggplot(aes(year, iit_toprate, colour=type)) +
  geom_line() +
  geom_point()

df3 %>% 
  filter(stabbr==st) %>%
  pivot_wider(names_from = type, values_from = iit_toprate)
  


# state-specific notes:
# HI: nber is zero in 2017 why?
# NY: nber drops in 2011+, tpc does not??

iit_rates_nber %>% filter(iit_toprate==0, !stabbr %in% nonpit)

iit_rates <- iit_rates_nber %>%
  mutate(iit_toprate=ifelse(stabbr=="HI" & year==2017, 8.5, iit_toprate)) %>%
  filter(iit_toprate != 0)

saveRDS(iit_rates, here::here("data", "iit_rates.rds"))

# https://taxfoundation.org/publications/state-individual-income-tax-rates-and-brackets/  Tax foundation rates


# Possible Plan B if needed: maybe look at cspp, avail 1977-2012 ----
all_variables <- get_var_info()
all_variables %>%
  filter(str_detect(long_desc, pattern=coll("tax", ignore_case=TRUE))) %>%
  filter(str_detect(long_desc, pattern=coll("rate", ignore_case=TRUE))) %>%
  select(variable, years, short_desc)

tmp <- all_variables %>% filter(variable=="x_tax_rate_rich")
tmp$sources
glimpse(tmp)
# What is the state individual income tax rate for an individual who makes more than 1.5 million real dollars?"
# 1977-2012



