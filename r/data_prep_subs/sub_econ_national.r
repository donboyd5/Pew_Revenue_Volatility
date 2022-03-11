
# econ_national.RData ---------------------------------------

# This file gets and prepares national economic data which is handled
# in a separate program.

# As of now, this only gets:
#   nominal gdp
#   gdp price index

# My package BEAData includes virtually all major data from the Bureau of
# Economic Analysis (BEA) in relation to the National Income and Products
# Accounts (NIPA) and in relation to regional data.

# For details on data creation see
#   https://github.com/donboyd5/BEAData 

# The package is stored locally in the renv/local folder
# data(package="BEAData")` shows the files available
# NIPA national data are in the dataframe nipa
# comment(nipa) shows creation date

# create and save 3 data frames:
#    national GDP (nominal), state fiscal year basis
#    GDP price index, state fiscal year basis


#.. determine desired variables ----
# use quarterly data as they are easy to convert to a July 1 typical state fiscal year
glimpse(BEAData::nipa)

# which variable name (i.e., vname) is current dollar GDP?
BEAData::nipa %>%
  filter(freq=="Q", str_detect(vdesc, "Gross domestic product, Current")) %>%
  group_by(vname, vdesc) %>%
  filter(date==max(date))

# which vname is the gdp price index?
BEAData::nipa %>%
  # filter(vname=="A191RG")
  filter(freq=="Q", 
         str_detect(vdesc, "Gross domestic product"),
         str_detect(vdesc, "Price Index")) %>%
  group_by(vname, vdesc) %>%
  filter(date==max(date))

# which vname is personal income? I want this on cy basis
BEAData::nipa %>%
  filter(freq=="A", str_detect(vdesc, coll("personal income", ignore_case = TRUE))) %>%
  group_by(vname, vdesc) %>%
  filter(date==max(date))

# which vname is disposable personal income? I want this on cy basis
BEAData::nipa %>%
  filter(freq=="A", str_detect(vdesc, coll("disposable personal income", ignore_case = TRUE))) %>%
  group_by(vname, vdesc) %>%
  filter(date==max(date))

# We want:
#   A191RC US nominal gdp
#   A191RG US GDP price index
#   A065RC US nominal personal income
#   A067RC US nominal disposable personal income

#.. nominal gdp on typical July 1 state fiscal year basis ----
gdpfy <- BEAData::nipa %>%
  filter(vname=="A191RC", freq=="Q") %>%
  # CAUTION: I do NOT bother to adjust for the fact that 4 states have
  # other-than-July-1 fiscal years:
  #   NY Apr 1
  #   AL, MI Oct 1 (federal fiscal year)
  #   TX:  Sep 1
  mutate(fyear=ifelse(month(date) >= 7, year + 1, year) %>% as.integer) %>%
  group_by(vname, fyear) %>%
  summarise(gdp=mean(value, na.rm=TRUE), .groups="drop") %>%
  filter(fyear <= 2021) %>%
  select(year=fyear, gdp)
gdpfy %>% ht  # 1947, 2021

#.. gdp price index on typical July 1 state fiscal year basis ----
# use most recent full year as base for our constant dollars
# a different base year would not affect results, but 2021
# is more intuitive than a different year
baseyear <- 2021 
gdppi <- BEAData::nipa %>%
  filter(vname=="A191RG", freq=="Q") %>%
  # fix NY AL MI TX
  mutate(fyear=ifelse(month(date) >= 7, year + 1, year) %>% as.integer) %>%
  group_by(vname, fyear) %>%
  summarise(gdppi=mean(value, na.rm=TRUE), .groups="drop") %>%
  filter(fyear <= 2021) %>%
  select(year=fyear, gdppi) %>%
  mutate(igdppi=gdppi[year==baseyear] / gdppi) 
summary(gdppi)

# I want personal income on a calendar year basis because later we'll compare it to agi
# also on cy basis
pi <- BEAData::nipa %>%
  filter(vname=="A065RC", freq=="A") %>%
  select(year, pi=value)
ht(pi)

# now disposable pi, also cy
dpi <- BEAData::nipa %>%
  filter(vname=="A067RC", freq=="A") %>%
  select(year, dpi=value)
ht(dpi)



# save econ_national.RData ------------------------------------------------
save(gdpfy, gdppi, pi, dpi,
     file = here::here("data", "econ_national.RData"))


