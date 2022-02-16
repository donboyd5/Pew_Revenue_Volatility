
# ECON_NATIONAL econ_national.RData ---------------------------------------

# My package BEAData includes virtually all major data from the Bureau of Economic Analysis (BEA) in relation to the National Income and Products Accounts (NIPA) and in relation to regional data.
# For details on data creation see https://github.com/donboyd5/BEAData
# Package is stored locally in the renv/local folder
# data(package="BEAData")` shows the files available
# NIPA national data are in dataframe nipa
# comment(nipa) shows creation date

# create data frames with national GDP (nominal) and the GDP price index, state fiscal year basis
#.. determine desired variables ----
glimpse(nipa)
# find desired variables
nipa %>%
  filter(freq=="Q", str_detect(vdesc, "Gross domestic product, Current")) %>%
  group_by(vname, vdesc) %>%
  filter(date==max(date))
# A191RC US nominal gdp
# A191RG US GDP price index

#.. nominal gdp on state fiscal year basis ----
# put them on a state fiscal year basis, for typical July 1 fiscal year
gdpfy <- nipa %>%
  filter(vname=="A191RC", freq=="Q") %>%
  # fix NY AL MI TX
  mutate(fyear=ifelse(month(date) >= 7, year + 1, year) %>% as.integer) %>%
  group_by(vname, fyear) %>%
  summarise(gdp=mean(value, na.rm=TRUE), .groups="drop") %>%
  filter(fyear <= 2020) %>%
  select(year=fyear, gdp)
gdpfy %>% ht  # 1947, 2019

#.. gdp price index on state fiscal year basis ----
baseyear <- 2021
gdppi <- nipa %>%
  filter(vname=="A191RG", freq=="Q") %>%
  # fix NY AL MI TX
  mutate(fyear=ifelse(month(date) >= 7, year + 1, year) %>% as.integer) %>%
  group_by(vname, fyear) %>%
  summarise(gdppi=mean(value, na.rm=TRUE), .groups="drop") %>%
  filter(fyear <= 2021) %>%
  select(year=fyear, gdppi) %>%
  mutate(igdppi=gdppi[year==baseyear] / gdppi) 
summary(gdppi)


# package bdata has NBER recession dates - take a look
# they are pulled from the NBER recessions page, updated in May 2020
# https://www.nber.org/research/data/us-business-cycle-expansions-and-contractions
# see my github page https://github.com/donboyd5/bdata
# particularly https://github.com/donboyd5/bdata/blob/master/DataConversionPrograms/bdata_recessions.R
# data(package="bdata")
# comment(recessions)
recessions # 1857, ..., 2020

# get quarterly nipa data and add recession markings

# https://apps.bea.gov/iTable/iTable.cfm
# we want two kinds of breakdowns:
#    C + I + G + (X - M)
#    Industrial breakdown

#.. CIGXM ----
# Table 1.1.6. Real Gross Domestic Product, Chained Dollars
tab <- NIPAvars %>%
  filter(tabnum == "1.1.6") %>%
  arrange(line)

# cigxm_vars <- tab %>%
#   filter(line %in% c(1:6, 7, 8, 14, 15, 22))
# cigxm_vars

# define the variables we will use to characterize recessions
# A020RX Exports of goods and services, Chained Dollars, Level T10106
# A021RX Imports of goods and services, Chained Dollars, Level

xwalk <- read_csv("vname,	vdesc, name, junk
A191RX, Gross domestic product, rgdp, GDP
DPCERX, Personal consumption expenditures, rpce, C
DGDSRX, Goods, rpcegoods,
DDURRX, Durable goods, rpcedgoods,
DNDGRX, Nondurable goods, rpcendgoods,
DSERRX, Services, rpceservices,
A006RX, Gross private domestic investment, rgpdi, I
A007RX, Private fixed investment, rpfi
A014RX, Change in private inventories, rinventories
A019RX, External balance of goods and services, rnetx, XM
A020RX, Exports of goods and services, rx, X
A021RX, Imports of goods and services, rm, M
A822RX, Government consumption expenditures and gross investment, rgov, G
A960RX, Residual, rres, na
")
xwalk

# tmp <- nipa %>%
#   filter(freq=="A", vname %in% xwalk$vname, year==2018) %>%
#   mutate(vname=factor(vname, levels=xwalk$vname)) %>%
#   arrange(vname) %>%
#   write_excel_csv(here::here("temp.csv"))

# get NIPA quarterly data
cigxmchain <- nipa %>%
  filter(freq=="Q", vname %in% xwalk$vname) %>%
  right_join(xwalk %>% select(vname, name), by="vname")
glimpse(cigxmchain)
count(cigxmchain, name, vname, vdesc)
# note that we only have breakdown of rpce into goods, services for 79 quarters (2002)

# we can go back further using quantity indexes - good for growth rates but not
# relative importance of variables (indexed to 2012=100)
# Table 1.1.3. Real Gross Domestic Product, Quantity Indexes
tabqi <- NIPAvars %>%
  filter(tabnum == "1.1.3") %>%
  arrange(line)
tabqi

# define the variables we will use to characterize recessions
xwalkqi <- read_csv("vname,	vdesc, name, junk
A191RA, Gross domestic product, rgdp, GDP         
DPCERA, Personal consumption expenditures, rpce, C
DGDSRA, Goods, rpcegoods,                
DDURRA, Durable goods, rpcedgoods,
DNDGRA, Nondurable goods, rpcendgoods,       
DSERRA, Services, rpceservices,
A006RA, Gross private domestic investment, rgpdi, I
A007RA, Private fixed investment, rpfi
B020RA, Exports of goods and services, rx, X
B021RA, Imports of goods and services, rm, M
B822RA, Government, rgov, G
")
xwalkqi
# get NIPA quarterly data


# cigxmqi_vars <- tabqi %>%
#   filter(vname %in% xwalkqi$vname)
# cigxmqi_vars

cigxmqi <- nipa %>%
  filter(freq=="Q", vname %in% xwalkqi$vname) %>%
  # right_join(cigxmqi_vars %>% 
  #              select(vname), 
  #            by = "vname") %>%
  right_join(xwalkqi %>% select(vname, name), by="vname") %>%
  mutate(vname=factor(vname, levels=xwalkqi$vname)) %>%
  arrange(vname, date)
glimpse(cigxmqi)
count(cigxmqi, vname)
count(cigxmqi, vname, name, vdesc)

#.. combine the chained cigxm data and the quantity indexed data ----
cigxm <- bind_rows(cigxmchain %>% 
                     mutate(valtype="chain"),
                   cigxmqi %>%
                     mutate(valtype="qidx"))

#.. Industrial breakdown ----
# A150RX
# NIPAvars %>%
#   filter(vname == "A150RX") 
# 
# tmp <- NIPAvars %>%
#   filter(str_detect(vdesc, coll("manuf", ignore_case = TRUE))) 
#  
# mtables <- count(tmp, tabname)
# https://apps.bea.gov/industry/Release/XLS/GDPxInd/ValueAdded.xlsx
# https://apps.bea.gov//industry/iTables%20Static%20Files/AllTablesHist.zip


# Table 5.8.6B. Real Private Inventories and Real Domestic Final Sales by Industry, Chained Dollars
# Table 6.16D. Corporate Profits by Industry
# Table 6.5D. Full-Time Equivalent Employees by Industry

#.. Employment breakdown ----
# https://www.bls.gov/web/empsit/cesseriespub.htm
# https://download.bls.gov/pub/time.series/ce/ce.data.01a.CurrentSeasAE  
# https://download.bls.gov/pub/time.series/ce/ce.txt says:
# ce.data.01a.CurrentSeasAE - contains every seasonally adjusted all employee series and its complete history.    

#.. Causes and characteristics ----
# https://www.investopedia.com/articles/economics/08/past-recessions.asp
# inflation: 1969
# oil and energy crises: 1973, 1980, 1981
# war: 1990 (Gulf)
# 9/11:  2001
# financial crisis: 2007 Great Recession
# real estate: 2007 GR
# 
#
# variables:
# oil prices
# real estate prices
# stock market
# manufacturing, tourism, other sectors
# unemployment rate


# National capital gains
# historical capital gains for the U.S., annual, calendar (tax) year
# primary sourdces are U.S. Treasury for data before 1995, and CBO for 1995+
# note that ~ 2020+ is CBO forecasts
# see spreadsheet for further details on sources
fn <- "NationalCapitalGains.xlsx"
sheet <- "CapGains"
capgains <- read_excel(here::here("raw_data", "soi", fn), sheet=sheet, range="A5:B83", 
                       col_types = c())
capgains <- capgains %>%
  mutate(year=as.integer(year),
         capgains=capgains * 1000)  # put it in millions of dollars
glimpse(capgains)


# create agi time series
# $ billions
agi1 <- read_excel(here::here("raw_data", "soi", "histab6.xls"),
                   col_names = c("year", "aginipa", "agi"),
                   range="A7:C62")

# $ thousands
agi2 <- read_excel(here::here("raw_data", "soi", "19intaba.xls"),
                   range="A4:AE117")
agi2a <- agi2 %>%
  filter(row_number() == 113)

agi2b <- agi2a %>%
  select(-1) %>%
  pivot_longer(cols=everything(), names_to = "year", values_to = "agi") %>%
  mutate(year=as.integer(year), agi=as.numeric(agi))

# combine
agi3 <- bind_rows(
  agi1 %>%
    select(year, agi) %>%
    mutate(agi=agi * 1000, src="histab6.xls"),
  agi2b %>%
    mutate(agi=agi / 1000, src="19intaba.xls"))

# compare
agi3 %>%
  ggplot(aes(year, agi, colour=src)) +
  geom_line() +
  geom_point()

# looks good so splice and save
agi <- agi3 %>%
  group_by(year) %>%
  mutate(keep=(n()==2 & src=="19intaba.xls") | n()==1) %>%
  ungroup %>%
  filter(keep) %>%
  select(-keep)

saveRDS(agi, here::here("raw_data", "soi", "agi.rds"))
rm(agi1, agi2, agi2a, agi2b, agi3)


# mark_recessionss 
#.. create data frame with each quarter as peak, trough, expand, or contract
firstdate <- as.Date("1947-01-01")  # min(cigxm1$date)
lastdate <- as.Date("2022-07-01") # max(cigxm1$date) + years(1) # allow an extra year

ptrecs <- recessions %>%
  filter(!is.na(rec_year)) %>%
  select(rec_year, peak=peak_quarter, trough=trough_quarter) %>%
  pivot_longer(-c(rec_year), names_to="qtype", values_to = "date")

rec_qtrs <- tibble(date=seq(firstdate,
                            lastdate,
                            by="1 quarter")) %>%
  left_join(ptrecs, by="date") %>%
  mutate(qtype=case_when(is.na(qtype) & lag(qtype)=="peak" ~ "contract",
                         is.na(qtype) & lag(qtype)=="trough" ~ "expand",
                         TRUE ~ qtype),
         qtype=case_when(is.na(qtype) & lead(qtype)=="peak" ~ "expand",
                         is.na(qtype) & lead(qtype)=="trough" ~ "contract",
                         TRUE ~ qtype)) %>%
  fill(qtype, .direction="downup") %>%
  # now fill in recession year, making it NA during expansions
  fill(rec_year) %>%
  mutate(rec_year=ifelse(qtype != "expand", rec_year, NA_character_))
# CAUTION: the 2020 recession had a peak quarter (2020q1), and a trough
# quarter(2020q2), with no contraction quarter in between. Make sure when
# selecting recession quarters to select the contract, and trough
# quarters. I think best NOT to select the peak quarter, too, but think
# more about this.

#.. collapse quarters to fiscal years ----
# assuming recession if at least 1 quarter is recession
rec_fyears <- rec_qtrs %>%
  mutate(year=ifelse(month(date) <= 6, year(date), year(date) + 1)) %>%
  group_by(year) %>%
  summarise(nrecq=sum(!is.na(rec_year)),
            rec_year=rec_year[!is.na(rec_year)][1])


# Recession characteristics ----
# for each recession from 1969 through 2007, treating 1981 as part of 1980, get
# duration months
# depth, real gdp %ch
# real PI % change
# consumption... % change
# manuf emp % change
# cg % change

rec_duration <- recessions %>%
  filter(rec_year >= 1969) %>%
  mutate(duration=interval(peak, trough) %/% months(1)) %>%
  select(recyear=rec_year, peak=peak_quarter, trough=trough_quarter, duration)

rec_peaktrough <- rec_duration %>%
  select(-duration) %>%
  pivot_longer(c(peak, trough), names_to = "qtype", values_to = "date")

# rgdp and rpce
rec_nipa <- rec_peaktrough %>%
  left_join(cigxm %>%
              filter(valtype=="chain", name %in% c("rgdp", "rpce")),
            by = "date") %>%
  select(recyear, qtype, name, value) %>%
  pivot_wider(names_from = qtype) %>%
  mutate(pch=trough / peak - 1) %>%
  select(recyear, name, pch) %>%
  pivot_wider(values_from = pch)

unrate <- readRDS(here::here("raw_data", "unrate.rds"))
rec_unrate1 <- unrate %>%
  arrange(date) %>%
  select(date, unrate=value) %>%
  mutate(unrate=unrate / 100,
         urminpre = slide_dbl(unrate, ~min(.x, na.rm=TRUE), .before=1*4, .after=1*4),
         urmaxpost=slide_dbl(unrate, ~max(.x, na.rm=TRUE), .before=1*4, .after=2*4)) %>%
  right_join(rec_peaktrough, by = "date")
rec_unrate1

rec_unrate <- rec_unrate1 %>%
  mutate(value=case_when(qtype=="peak" ~ urminpre,
                         qtype=="trough" ~ urmaxpost,
                         TRUE ~ NA_real_)) %>%
  select(recyear, qtype, value) %>%
  pivot_wider(names_from = qtype) %>%
  mutate(urchange=trough - peak)

# for cap gains, get the 
rec_cg <- capgains %>%
  arrange(year) %>%
  # cgmaxpre will trigger a warning but it's ok we want to use this period
  # this says we want to look back from 2 years before to 1 year before (-1 does that)
  # the warning comes in 1954 because there are no years before it
  # even so, this is imperfect - sometimes we might want to include the
  # recession year in the prior max -- if it started late in year, for example
  mutate(cgmaxpre = slide_dbl(capgains, ~max(.x), .before=2, .after=-1),
         cgminpost=slide_dbl(capgains, ~min(.x), .before=0, .after=2),
         cgpch=cgminpost / cgmaxpre - 1) %>%
  filter(year %in% rec_duration$recyear)
rec_cg

# combine the recession files
rec_features <- rec_duration %>%
  select(-peak, -trough) %>%
  left_join(rec_nipa, by = "recyear") %>%
  left_join(rec_unrate %>%
              select(recyear, urchange), 
            by = "recyear") %>%
  left_join(rec_cg %>%
              select(recyear=year, cgpch), 
            by = "recyear")
rec_features  


# we do not save the recessions data frame because it is already included in package bdata
# save(gdpfy, gdppi,
#      cigxm, capgains, agi,
#      rec_qtrs, rec_fyears, rec_features,
#      file = here::here("data", "econ_national.RData"))

