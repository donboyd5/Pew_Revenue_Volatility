# Data prep for state tax revenue volatility


# define system-specific variables (e.g., folders) ------------------------
# load or define system-specific constants
#   for example, folders that store raw data

# you will need the following locations:
#   beadir # for BEA (Bureau of Economic Analysis) data
#   qcewdir # for BLS (Bureau of Labor Statistics) 
#     Quarterly Census of Employment and Wages (QCEW)
#   scratchdir # for temporary files
source(here::here("r", "constants_system.r"))


# get libs ----------------------------------------------------------------
source(here::here("r", "libs_base.r"))
source(here::here("r", "libs_ts.r"))
# examine, look for conflicts
devtools::session_info()
# devtools::package_info()
tidyverse_conflicts()


# get functions -----------------------------------------------------------
# source(here::here("r", "functions_maps.r"))
# source(here::here("r", "functions_measures.r"))
# source(here::here("r", "functions_plots.r"))
# source(here::here("r", "functions_utility.r"))


# program outline ---------------------------------------------------------

# NOTE: Certain section names start with "ONETIME". In those sections, I create and
# save data that will be later read from files. The data only need to be created
# one time (or updated and re-saved if I decide to update the data).

#..   General files saved in (general.rdata) ----
#     -   State fiscal year for each state (sfy_startmonths)

#..   NIPA national data saved in (econ_national.rdata) ----
#     -   Data frames on state fiscal year basis, 1947-2019
#     -   national nominal GDP, state fiscal year basis, 1947-2019 (gdpfy)
#     -   national GDP price index, state fiscal year basis, 1947-2019 (gdppi)
#     -   Recession dates data frame (recessions)

#..   NIPA national data for classifying recessions saved in (not yet saved) ----
#     -   CIGXM quarterly 1929- mid 2020 ()
#     -   Industrial data ()

#..   State economic data saved in (econ_state.rdata) ----
#     -   state gdp, state fiscal year basis, real and nominal (sgdpfy)

#..   Tax data saved in (taxdata.rdata) ----
#     -   Census by state and tax type, summarized by category, annual (taxkeep)


# test load data -- only for testing purposes -----------------------------
# DO NOT RUN UNTIL AFTER FILES HAVE BEEN CREATED
# useful for verifying that what's in each file is what's intended
load(file = here::here("data", "general.RData"), verbose=TRUE)
load(file = here::here("data", "econ_national.RData"), verbose=TRUE)
# load(file = here::here("data", "econ_state.RData"), verbose=TRUE)
load(file = here::here("data", "gdp_state.RData"), verbose=TRUE)
load(file = here::here("data", "taxdata.RData"), verbose=TRUE)


# GENERAL general.rdata ----

#.. data frame indicating start of each state's fiscal year ----
# -   46 states have a July 1 fiscal year
# -   NY has April 1
# -   TX has Sept 1
# -   AL, MI have Oct 1
# -   See https://www.ncsl.org/research/fiscal-policy/fy-2021-state-budget-status.aspx

sfy_startmonths <- tibble(stabbr=state.abb) %>%
  mutate(startmonth=case_when(stabbr=="NY" ~ 4,
                              stabbr=="TX" ~ 9,
                              stabbr %in% c("AL", "MI") ~ 10,
                              TRUE ~ 7))

# as of now this is the only object to put in general.RData
save(sfy_startmonths, file = here::here("data", "general.RData"))


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


# package bdata has NBER recession dates
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

```

```{r national_capital_gains}
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

```


```{r ONETIME_more_downloads}
#.. ONETIME: get and save unemployment rate ----
# https://fred.stlouisfed.org/series/UNRATE
unrate <- fredr("UNRATE", frequency = "q")
saveRDS(unrate, here::here("raw_data", "unrate.rds"))

#.. ONETIME: agi ----
# agi pre-1990
download.file(url="https://www.irs.gov/pub/irs-soi/histab6.xls",
              here::here("raw_data", "soi", "histab6.xls"),
              mode="wb")

# agi 1990-2019
download.file(url="https://www.irs.gov/pub/irs-soi/19intaba.xls",
              here::here("raw_data", "soi", "19intaba.xls"),
              mode="wb")

```



```{r national_agi}

# also see https://www.irs.gov/pub/irs-soi/05in01an.xls
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

```


```{r mark_recessionss}
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

```


```{r}
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

```


```{r save_econ_national}
save(gdpfy, gdppi,
     cigxm, capgains, agi,
     recessions, rec_qtrs, rec_fyears, rec_features,
     file = here::here("data", "econ_national.RData"))

# used to include: marked_qtrs, marked_fyears,

```

# ECON_STATE -- create econ_state.RData

## BEA state GDP data

```{r sgdp_sfy}
# data(package="BEAData")
# glimpse(sgdp.q)
summary(sgdp.q)  # starts in 2005-01-01 so this will not be good
summary(sgdp.a) # 1997-2020, only a bit better
summary(sgdp_spliced.a)  # 1963-2020 -- good
count(sgdp_spliced.a, name) # gdp and rgdp

df <- sgdp_spliced.a %>%
  left_join(sfy_startmonths, by = "stabbr") %>%
  group_by(stabbr, name) %>%
  mutate(lagvalue=value[match(year - 1, year)],
         current_share=(startmonth - 1) / 12,
         value_sfy=value * current_share + lagvalue * (1 - current_share))
df
summary(df) # 1963-2020

sgdpfy <- df %>%
  ungroup %>%
  filter(!is.na(value_sfy)) %>%
  select(year, stabbr, name, value=value_sfy)
summary(sgdpfy) # 1964-2020


```

```{r sgdpdetail_sfy}
# https://apps.bea.gov/regional/downloadzip.cfm
# SQGDP1__definition just has real, quant index, nominal gdp
# SQGDP2 nominal (?) by sector
# SQGDP8" Name="Chain-type quantity indexes for real GDP by state (2012=100.0), sectors
# SQGDP9" Name="Real GDP by state, sectors
# SQGDP11" Name="Contributions to percent change in real GDP", sector
# thus we want SQGDP9

# dir <- here::here("raw_data", "bea", "/")
afiles <- unzip(file.path(beadir, "SAGDP.zip"), list=TRUE)
qfiles <- unzip(file.path(beadir, "SQGDP.zip"), list=TRUE)

# SAGDP9N__ALL_AREAS_1997_2020.csv  # NAICS annual
# SAGDP9S__ALL_AREAS_1997_2020.csv  # SIC annual

# df <- read_table(archive_read(file.path(dir_eesm, fn_data),
#                               path_ext_remove(fn_data),
#                               format="raw",
#                               filter="gzip"),

fn <- "SQGDP9__ALL_AREAS_2005_2021.csv"
# con <- unz(paste0(dir, "/", "SQGDP.zip"), fn) 
# read directly from zip file
df <- read_csv(archive_read(file.path(beadir, "SQGDP.zip"), fn), 
               col_types = cols(.default = "c"))
problems(df)
# close(con=con)

names(df)
glimpse(df)
count(df, GeoFIPS, GeoName)

df2 <- df %>%
  mutate(stfips=str_sub(GeoFIPS, 1, 2)) %>%
  left_join(stcodes %>% select(stfips, stabbr, stname), by="stfips")
count(df2, GeoFIPS, stabbr, stname, GeoName)  

dfl <- df2 %>%
  filter(!is.na(stabbr)) %>%
  select(stabbr, stname, line=LineCode, ind=IndustryClassification,
         indname=Description,  `2005:Q1`:`2021:Q1`) %>%
  pivot_longer(cols=`2005:Q1`:`2021:Q1`) %>%
  mutate(date=yq(name), value=as.numeric(value), line=as.integer(line))
ht(dfl)  
count(dfl, line, ind, indname)

check <- dfl %>% 
  filter(is.na(value))
# na's are mostly small industries in small states, suppressed to prevent disclosure

# create a clean quarterly series, then state fiscal year
sgdpdetail_qtr <- dfl %>%
  select(stabbr, date, line, ind, indname, value)
summary(sgdpdetail_qtr) # 2005-01-01 - 2021-01-01


sgdpdetail_sfy <- sgdpdetail_qtr %>%
  mutate(year=ifelse(month(date) >= 7,
                     year(date) + 1,
                     year(date)) %>%
           as.integer) %>%
  group_by(stabbr, year, line, ind, indname) %>%
  summarise(value=mean(value, na.rm=TRUE),
            .groups="drop")
summary(sgdpdetail_sfy) # 2005-2021

check <- sgdpdetail_sfy %>% 
  filter(is.na(value))
# na's are mostly small industries in small states, suppressed to prevent disclosure

```

```{r gdp_shares}
# ONETIME - write US for a recent year, to figure out hierarchical structure of industry data
# sgdpdetail_sfy %>%
#   filter(stabbr=="US", year==2020) %>%
#   write.xlsx(here::here("scratch", "gdpshares.xlsx"))

xwalk <- read_csv2("line;	ind;	indname; group	
1;	...;	All industry total; total
2;	...;	Private industries;
3;	11;	Agriculture, forestry, fishing and hunting; agri
6;	21;	Mining, quarrying, and oil and gas extraction;	mining
10;	22;	Utilities;	other
11;	23;	Construction;	construct
12;	31-33;	Manufacturing;	manuf
13;	321,327-339;	Durable goods manufacturing;
25;	311-316,322-326;	Nondurable goods manufacturing;
34;	42;	Wholesale trade;	other
35;	44-45;	Retail trade;	other
36;	48-49;	Transportation and warehousing;	other
45;	51;	Information;	other
51;	52;	Finance and insurance;	finance
56;	53;	Real estate and rental and leasing;	restate
60;	54;	Professional, scientific, and technical services;	other
64;	55;	Management of companies and enterprises;	other
65;	56;	Administrative and support and waste management and remediation services;	other
69;	61;	Educational services;	other
70;	62;	Health care and social assistance;	health
76;	71;	Arts, entertainment, and recreation;	artsfood
79;	72;	Accommodation and food services;	artsfood
82;	81;	Other services (except government and government enterprises);	other
83;	...;	Government and government enterprises;	govt
84;	...;	Federal civilian;
85;	...;	Military;
86;	...;	State and local;
") %>% 
  mutate(line=as.integer(line))
xwalk

# collapse state gdp data into these categories and calculate shares
sgdpgrouped_sfy <- sgdpdetail_sfy %>%
  left_join(xwalk %>% select(line, group),
            by="line") %>%
  filter(!is.na(group)) %>%
  group_by(stabbr, year, group) %>%
  summarise(value=sum(value, na.rm=TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group) %>%
  select(-c(other, total), everything(), other, total) %>%
  # add calculated sum, to compare to reported total
  mutate(sum=rowSums(across(-c(stabbr, year, total)), na.rm=TRUE))
  
check <- sgdpgrouped_sfy %>%
  mutate(pdiff=sum / total * 100 - 100)
check  
check %>% filter(stabbr=="US")
summary(check)

sgdpshares_sfy <- sgdpgrouped_sfy %>%
  # use the calculated sum for computing shares, not the reported totals
  # however, the distinction is trivial because we verified above that the
  # calculated sum usually is extremely close to the reported total
  select(-total) %>%
  mutate(across(-c(stabbr, year), ~ .x / sum))

sgdpshares_sfy %>%
  filter(stabbr=="US")

sgdpshares_sfy %>%
  filter(year==2020) %>%
  arrange(-manuf)

sgdpshares_sfy %>%
  filter(year==2020) %>%
  arrange(-mining)

sgdpshares_sfy %>%
  filter(year==2020) %>%
  arrange(-finance)

```


```{r save_gdp_state}
save(sgdpfy, sgdpdetail_qtr, sgdpdetail_sfy, sgdpgrouped_sfy, sgdpshares_sfy,
     file = here::here("data", "gdp_state.RData"))
 
load(file = here::here("data", "gdp_state.RData"), verbose=TRUE)

```


## BLS QCEW
```{r download_qcew}
# https://data.bls.gov/cew/data/files/1990/csv/1990_annual_by_area.zip
# dir <- "/media/don/data/qcew/"

for(year in 1990:2020){
  zipname <- paste0(year, "_annual_by_area.zip")
  # print(zipname)
  url <- paste0("https://data.bls.gov/cew/data/files/", year, "/csv/", zipname)
  print(url)
  savename <- file.path(qcewdir, zipname)
  # print(savename)
  download.file(url, savename, mode="wb")
}

```

```{r qcew_extract_and_save}
# year <- 2015
read_states <- function(year){
  # read all of the statewide files and the US file for a single year
  print(year)
  
  # get path to zip archive
  zname <- paste0(year, "_annual_by_area.zip")
  zpath <- file.path(qcewdir, zname)
  
  # file names
  stfiles <- str_subset(unzip(zpath, list = TRUE)$Name, coll("Statewide"))
  usfile <-  str_subset(unzip(zpath, list = TRUE)$Name, coll("U.S. TOTAL"))
  files <- c(usfile, stfiles)
  
  # we are going to use "map" to create connections to all these files
  # the function we map over must have file name as its first argument
  # but our connection function archive_read takes the path first
  # so we create a small function that reverses the order, taking filename first
  # archive_read2 <- function(fname, zpath) archive_read(zpath, fname)
  # now we can create the connections to all the desired files
  # we're going to need ~50 connections per year and we'll do this for 20 years
  # so we close all connections before starting so we don't run out of them
  # closeAllConnections()
  # cons <- purrr::map(files, archive_read2, zpath)
  # # finally, read the files en masse
  # df <- vroom(cons[[1]], col_types = cols(.default = "c"))
  # vroom(archive_read(zpath, files[1]), col_types = cols(.default = "c"))
  # vroom(unz(zpath, file=files[1]), col_types = cols(.default = "c"))
  # read_csv(unz(zpath, file=files[1]), col_types = cols(.default = "c"))
  # 
  # read_all_zip <- function(zpath, ...) {
  #   filenames <- unzip(zpath, list = TRUE)$Name
  #   vroom(purrr::map(files, ~ unz(zpath, .x)), ...)
  # }
  # 
  df <- vroom::vroom(purrr::map(files, ~ unz(zpath, .x)), 
              col_types = cols(.default = "c"))
  
 #  read_one_file <- function(fpath){
 #    print(fpath) 
 #    read_csv(unz(zpath, file=fpath), 
 #             col_types = cols(.default = "c")) 
 #  }
 # map_dfr(files, read_one_file)
 #  
 saveRDS(df, file.path(qcewdir, paste0("qcew", year, ".rds")))
}

# save a file for each year
walk(1990:2020, read_states)
# after checking files, detach vroom because it can cause problems
detach("package:vroom")

# read those files and combine
# we do this in 2 steps because vroom is a little buggy and reading and
# combining in one step appears to cause memory problems
f <- function(year, qcewdir, stcodes){
  # year <- 2012
  print(year)
  df <- readRDS(file.path(qcewdir, paste0("qcew", year, ".rds")))
  # slim the file down before combining
  df2 <- df %>%
    filter(as.integer(agglvl_code) %in% c(10:13, 50:53)) %>% # go down to the supersector level
    mutate(stfips=str_sub(area_fips, 1, 2),
           stfips=ifelse(stfips=="US", "00", stfips),
           own_code=as.integer(own_code),
           agglvl_code=as.integer(agglvl_code),
           year=as.integer(year)) %>%
    left_join(stcodes %>% select(stfips, stabbr), by="stfips") %>%
    select(stabbr,
           year,
           own=own_code,
           ownf=own_title,
           ind=industry_code,
           indf=industry_title,
           agg=agglvl_code,
           aggf=agglvl_title,
           emp=annual_avg_emplvl,
           wages=total_annual_wages) %>%
    mutate(across(c(emp, wages), as.numeric))

}
# yq("2020Q4")
# df11 <- f(2011, qcewdir, stcodes)
# df12 <- f(2012, qcewdir, stcodes)
# df13 <- f(2013, qcewdir, stcodes)
# bind_rows(df11, df12, df13)

df <- map_dfr(1990:2020, f, qcewdir, stcodes)
count(df, year)
count(df %>% 
        filter(year %in% c(2000, 2020)), year, stabbr) %>% 
  pivot_wider(names_from = year, values_from = n)
# memory()

xwalk <- read_delim(
" ind; vname; vdesc
10; allind; 10 Total, all industries
101; goods; 101 Goods-producing
1011; natres; 1011 Natural resources and mining
1012; constr; 1012 Construction
1013; manuf; 1013 Manufacturing
102; service; 102 Service-providing
1021; tpu; 1021 Trade, transportation, and utilities
1022; info; 1022 Information
1023; finact; 1023 Financial activities
1024; profbus; 1024 Professional and business services
1025; edhealth; 1025 Education and health services
1026; leisure; 1026 Leisure and hospitality
1027; othersvc; 1027 Other services
1028; pubadmin; 1028 Public administration
1029; unclass; 1029 Unclassified
", delim=";", trim_ws=TRUE)
xwalk

df2 <- df %>%
  filter(agg %in% c(10, 13, 50, 53)) %>%
  mutate(level=case_when(agg %in% c(10, 50) ~ "total",
                         agg %in% c(13, 53) ~ "supersector"),
         vname=factor(ind, levels=xwalk$ind, labels=xwalk$vname))

# collapse
df3 <- df2 %>%
  group_by(stabbr, year, vname) %>%
  summarise(emp=sum(emp), wages=sum(wages), .groups = "drop")

qcew_slim <- df3 %>%
  select(-wages) %>%
  pivot_wider(names_from = vname, 
              values_from = emp) %>%
  mutate(empsum = select(., -c(stabbr, year, allind)) %>% rowSums(na.rm=TRUE),
         pct=empsum / allind)
summary(qcew_slim) # not too bad

# saveRDS(qcew_slim, paste0(qcewdir, "qcew_slim.rds"))

# make a slimmed down file because we'll rarely want all this data

st <- "NV"
sts <- c("FL", "ND", "NV", "MI")
qcew_slim %>%
  filter(stabbr %in% sts) %>%
  group_by(stabbr) %>%
  mutate(ivalue=constr / constr[year==2006]) %>%
  ungroup %>%
  ggplot(aes(year, ivalue, colour=stabbr)) +
  geom_line() +
  geom_point()

```


```{r save_qcew_state}
save(qcew_slim, file = here::here("data", "qcew_state.RData"))
 
load(file = here::here("data", "qcew_state.RData"), verbose=TRUE)

```

## Census Bureau tax data

### Clean tax data

Steps:

1.  Get Census state govt tax data from bdata (previously created)
2.  Keep desired taxes and create an other taxes category
3.  Identify and explore anomalies

```{r get_tax}
# tax1 has untouched data from Census, only the items we want, long format
# tax2 is same, but with the other tax variable


glimpse(sgtax.a)  # 2020 end year; most values from ~1951+
# count(sgtax.a, year) %>% as.data.frame()
# count(sgtax.a, item, desc)

# determine the tax items we want
taxnames <- sgtax.a %>% select(item, desc) %>% distinct()
items <- c("C105", "T09", "C109", "T40", "T41", "T53")
varnames <- c("tottax", "gst", "selsalestax", "iit", "cit", "sevtax")
sumvars <- setdiff(varnames, "tottax")

tax1 <- sgtax.a %>%
  filter(year >= 1951) %>%
  filter(item %in% items) %>%
  mutate(name=factor(item, levels=items, labels=varnames)) %>%
  select(stabbr, name, year, value) %>%
  arrange(stabbr, name, year)
summary(tax1)  # no missing values

# tax2 calculates other tax
tax2 <- tax1 %>%
  # now calculate other tax
  pivot_wider(names_from = name) %>%
  mutate(nonother = rowSums(across(all_of(sumvars)), na.rm=TRUE),
         othertax=tottax - nonother) %>%
  select(-nonother) %>%
  pivot_longer(-c(year, stabbr)) %>%
  filter(!is.na(value))  # we can safely drop NA values because we didn't have any in tax1
summary(tax2)

```

Identify and explore anomalies:

-   missing
-   zero
-   missing or zero at start (always assumed bad)
-   missing or zero at end (always assumed bad)
-   missing in middle
-   zero in middle
-   negative in middle (logs won't work -- only 1 found)
-   short runs (fewer than X positive values in a row)

```{r anomalies}
# identify different anomalies
# CO sevtax  2017 0
# OH cit 2014 -118 
tax2 %>% filter(stabbr=="AK", name=="iit")  # nz 1959-1988
tax2 %>% filter(stabbr=="AK", name=="gst") # only zeros
tax2 %>% filter(stabbr=="AZ", name=="sevtax") # nz 2001-2020
tax2 %>% filter(stabbr=="CO", name=="sevtax") # nz, but 2017 oddly zero
tax2 %>% filter(stabbr=="OH", name=="cit")  # negative 2014, zero pre-1972
tax2 %>% filter(stabbr=="NH", name=="sevtax") # nz 1951-1995 z in 1988, 1994
tax2 %>% filter(stabbr=="SD", name=="sevtax")  # z 1970-1975

# vectors used to test the functions below
a1 <- c(0, 0, NA, 1, 2, 3, 0, 4, 5, 6, 0, 0)
a2 <- c(0, 0, 0, 0)
a3 <- c(0, NA, 0, 0)
a4 <- c(7, 9, 0, 0, NA, 1, 2, 3, 0, 4, 5, 6)

firstnz <- function(x) {
  # find index of first nonzero value, to help us identify zeroes at the start
  ifelse(all(x==0, na.rm=TRUE), 
         Inf, 
         which(x != 0)[[1]])
}

# test firstnz
firstnz(a1)
firstnz(a2)
firstnz(a3)
firstnz(a4)

lastnz <- function(x) {
  # find index of last nonzero value, to help us identify zeroes at the end
  ifelse(all(x==0, na.rm=TRUE),
         -Inf, 
         tail(which(x != 0), 1))
}

# test lastnz
lastnz(a1)
lastnz(a2)
lastnz(a3)
lastnz(a4)


firstgap <- function(gap) {
  # gap is a a vector of 0 or 1 indicators for a series
  # 0 indicates the observation is not part of a gap and 1 indicates it is part of a gap
  # firstgap finds the index at which a gap in a series starts (if a gap exists)
  ifelse(all(gap !=TRUE, na.rm=TRUE),
         Inf, # return Inf if there is no gap in the series
         which(gap==TRUE)[[1]]) # index of the start of the first gap
}


lastgap <- function(gap) {
  # index of the final observation in last gap
  ifelse(all(gap !=TRUE, na.rm=TRUE),
         -Inf, # return -Inf if there is no gap in the series
         tail(which(gap==TRUE), 1)) # index of the final observation in the last gap of a series
}


# identify anomalies within each state-tax combination
# also, for series that have gaps in them, identify observations that occur before the first gap or
# after the last gap, as we will only want to keep one or the other
anomalies <- tax2 %>%
  group_by(stabbr, name) %>%
  mutate(missval = is.na(value),  # we should not have any missing values but check anyway
         zeroval = value == 0,
         negval = value < 0,
         leadzero = row_number() < firstnz(value),   # identify all leading-zero rows
         trailzero = row_number() > lastnz(value),  # identify all trailing-zero rows
         gap = zeroval & !(leadzero | trailzero),  # a gap occurs if we have a zero that is not leading or trailing
         pregap = row_number() < firstgap(gap),   # identify data that occurs before the first gap
         postgap = row_number() > lastgap(gap))   # identify data that occurs after the last gap
summary(anomalies)
# what should we do if a series has multiple gaps?? look to make sure it doesn't happen; I don't think so

anomalies %>% filter(stabbr=="AK", name=="iit")
anomalies %>% filter(stabbr=="AK", name=="gst")
anomalies %>% filter(stabbr=="AZ", name=="sevtax")
anomalies %>% filter(stabbr=="AZ", name=="cit") # gap in 1957
anomalies %>% filter(stabbr=="CO", name=="sevtax") # gap 2017
anomalies %>% filter(stabbr=="OH", name=="cit")  # neg 2014
anomalies %>% filter(stabbr=="NH", name=="sevtax") # gap 1988 1994
anomalies %>% filter(stabbr=="NV", name=="sevtax")
anomalies %>% filter(stabbr=="SD", name=="sevtax")
anomalies %>% filter(gap)


anomalies %>% 
  filter(stabbr=="NV", name=="sevtax") %>%
  lastgap()
# note that NV sevtax jumped way up after 1987 -- investigate

# rules:
# - drop all lead zero and trail zero
# - take the post gap periods
# individually and decide - take the post gap

# get rid of leading and trailing zeros
anomalies_v2 <- anomalies %>%
  filter(!(leadzero | trailzero))

```

```{r anomalies_gaps}
# explore the gaps -- the only bad items -- and decide what to keep
summary(anomalies_v2)

# what gaps do we have?
gaps <- anomalies_v2 %>%
  group_by(stabbr, name) %>%
  filter(any(gap != 0)) %>%
  ungroup

count(gaps, stabbr, name) # only 7 combinations

gaps %>%
  group_by(stabbr, name) %>%
  summarise(gap_start=min(year[gap==TRUE]))

anomalies_v2 %>% filter(stabbr=="AZ", name=="cit")
anomalies_v2 %>% filter(stabbr=="CO", name=="sevtax")  # 2017 is zero; some big changes in early years
anomalies_v2 %>% filter(stabbr=="MO", name=="sevtax")  # a string of zeroes before 1978
anomalies_v2 %>% filter(stabbr=="NH", name=="sevtax") # we'll want to drop this for other reasons
anomalies_v2 %>% filter(stabbr=="NM", name=="cit")  # 1967+ is good
anomalies_v2 %>% filter(stabbr=="NV", name=="sevtax") # 1988+ is good - low values or zero prior
anomalies_v2 %>% filter(stabbr=="SD", name=="sevtax") # 1976+ is good

# there are only 7 so identify them by hand
gapkeep <- tribble(~stabbr, ~name, ~year1, ~year2,
                   "AZ", "cit", 1958, 2019,
                   "CO", "sevtax", 1951, 2016,
                   "MO", "sevtax", 1978, 2019,
                   "NH", "sevtax", 1951, 1987,
                   "NM", "cit", 1967, 2019,
                   "NV", "sevtax", 1988, 2019,
                   "SD", "sevtax", 1976, 2019,
                   ) 
gapkeep

anomalies_v3 <- anomalies_v2 %>%
  left_join(gapkeep, by=c("stabbr", "name")) %>%
  mutate(drop=ifelse(!is.na(year1) & {(year < year1) | (year > year2)},
                     TRUE, FALSE)) %>%
  filter(!drop)
anomalies_v3 %>% filter(stabbr=="SD", name=="sevtax") 


```

```{r census_tax_save}
taxkeep <- anomalies_v3 %>%
  select(stabbr, name, year, value) %>%
  ungroup
summary(taxkeep)
glimpse(taxkeep)
census_clean_tax <- taxkeep
saveRDS(census_clean_tax, here::here("data", "details", "census_clean_tax.rds"))

```

## adjust Census iit and gst for rates (calculated externally)

```{r}
census_clean_tax <- readRDS(here::here("data", "details", "census_clean_tax.rds"))
summary(census_clean_tax) # 1951-2020

gst_rates <- readRDS(here::here("data", "gst_rates.rds"))
summary(gst_rates) # 1941-2021

iit_rates <- readRDS(here::here("data", "iit_rates.rds"))
summary(iit_rates) # 1977-2018

gstiitadj1 <- census_clean_tax %>%
  filter(name %in% c("iit", "gst")) %>%
  left_join(gst_rates %>%
              mutate(name="gst") %>%
              rename(gstrate=value),
            by = c("stabbr", "name", "year")) %>%
  left_join(iit_rates %>%
              mutate(name="iit") %>%
              select(-srate_gains),
            by = c("stabbr", "name", "year")) %>%
  mutate(rate=ifelse(name=="iit", iit_toprate,
                     gstrate)) %>%
  filter(!is.na(rate)) %>%
  group_by(stabbr, name) %>%
  mutate(valueadj=value * rate[year==max(year)] / rate) %>%
  ungroup


summary(gstiitadj1)
gstiitadj1 %>%
  filter(stabbr=="NY", name=="iit") %>%
  arrange(name, year)

# traditional non pit states
nonpit <- c("AK", "FL", "NV", "NH", "TN", "SD", "TX", "WA", "WY")
gstiitadj <- gstiitadj1 %>%
  mutate(name=case_when(name=="iit" ~ "iitadj",
                        name=="gst" ~ "gstadj",
                        TRUE ~ "ERROR")) %>%
  select(stabbr, name, year, value=valueadj) %>%
  filter(!is.na(value)) %>%
  filter(!(name=="iitadj" & stabbr %in% nonpit))
count(gstiitadj, name)
summary(gstiitadj)
count(gstiitadj, name, stabbr) %>%
  pivot_wider(values_from = n)

saveRDS(gstiitadj, here::here("data", "details", "census_gstiitadj.rds"))

```

## Pew data on enacted revenue changes

-   Pew constructed estimates of the percent change in major taxes, by year, state, and major tax type for work it did in \_\_\_
-   In a 10/8/2015 email, on my request Brenna Erford [BErford\@pewtrusts.org](mailto:BErford@pewtrusts.org){.email} provided the file "Boyd Data Request 100815.xlsx" to me ([donald.boyd\@rockinst.suny.edu](mailto:donald.boyd@rockinst.suny.edu){.email}), which had data underlying the project
-   Pew's short description of the data, in the "Cover" tab of the workbook, was:

<blockquote style="color:grey;font-size:100%;margin-left: 4em">

<p>

Volatility scores for each state's total tax revenue and specific tax sources were calculated using the U.S. Census Bureau's State Government Tax Collections historical data series for 1994 to 2014, accessed April 21, 2015. Data were adjusted to control for the effects of tax policy change using the National Conference of State Legislatures (NCSL) State Tax Action reports for 1994 to 2013, accessed in January 2015.

</p>

</blockquote>

Note that in these data, adjusted = raw - polichy They include separate tabs for total and for major. The majors are:

`Major Revenue Source` n <chr> <int> 1 Amusements 9 2 Corporation 9 3 Corporation net income 90 4 Documentary and stock transfer 6 5 General sales and gross receipts 135 6 Individual income 123 7 Motor fuels 99 8 Motor vehicle 12 9 Occupation and business, NEC 6 10 Property taxes 27 11 Public utilities 9 12 Severance 27 13 Tobacco products 3 14 Total 153

The numbers are percent changes only. There are no levels. There are no US totals for the individual majors.

Pew_1995_2014_2015-10-08(Boyd Data Request 100815).xlsx has the policy adjustments, no US totals for major taxes

Pew_2000_2019_2020-10-13(RevenueVolatilityData).xlsx, and Pew_1998_2017_2018-08-29(RevenueVolatilityData).xlsx

have the annual volatility score (I think this is policy adjusted growth) by state and major tax. Does not have policy adjustments separately.

```{r ONETIME_pew_revenue_data, eval=FALSE}
# data file Boyd Data Request 100815.xlsx
# fnames <- c("", "", "")


xlfn <- here::here("raw_data", "pew", "Boyd Data Request 100815.xlsx")
sheets <- excel_sheets(xlfn)
sheets

get_pewsheet <- function(sheet){
  print(sheet)
  range <- ifelse(str_detect(sheet, "Major"), "A5:V190", "A5:V56")
  read_excel(xlfn, sheet=sheet, range=range) %>%
    mutate(sheet=sheet)
}

pewdf <- map_dfr(sheets[-1], get_pewsheet)
glimpse(pewdf)
count(pewdf, sheet)
count(pewdf, `Major Revenue Source`)

#    `Major Revenue Source`               n
#    <chr>                            <int>
#  1 Amusements                           9
#  2 Corporation                          9
#  3 Corporation net income              90
#  4 Documentary and stock transfer       6
#  5 General sales and gross receipts   135
#  6 Individual income                  123
#  7 Motor fuels                         99
#  8 Motor vehicle                       12
#  9 Occupation and business, NEC         6
# 10 Property taxes                      27
# 11 Public utilities                     9
# 12 Severance                           27
# 13 Tobacco products                     3
# 14 Total                              153

type_map <- read_delim("revsource; taxtype
Amusements; amusetax
Corporation; corptax
Corporation net income; cit
Documentary and stock transfer; doctax
General sales and gross receipts; gst
Individual income; iit
Motor fuels; mft
Motor vehicle; mvl
Occupation and business, NEC; occtax
Property taxes; proptax
Public utilities; utiltax
Severance; sevtax
Tobacco products; cigtax
Total; tottax", 
delim=";", trim_ws = TRUE)

type_map

pewtax_base <- pewdf %>%
  select(stname=1, revsource=2, everything()) %>%
  left_join(bdata::stcodes %>% select(stabbr, stname), by="stname") %>%
  left_join(type_map, by="revsource") %>%
  mutate(vartype=word(sheet, 3)) %>%
  pivot_longer(-c(stabbr, stname, revsource, sheet, vartype, taxtype), names_to = "year", values_to = "pch") %>%
  mutate(year=as.integer(year)) %>%
  select(stabbr, stname, sheet, vartype, revsource, taxtype, year, pch) %>%
  arrange(stabbr, vartype, taxtype, year)
  
glimpse(pewtax_base)

levels <- pewtax_base %>%
  filter(vartype %in% c("raw", "adjusted")) %>%
  group_by(stabbr, vartype, taxtype) %>%
  arrange(year) %>%
  do(add_row(.,
             stabbr=.$stabbr[1], 
             stname=.$stname[1],
             sheet=.$sheet[1],
             vartype=.$vartype[1], 
             revsource=.$revsource[1],
             taxtype=.$taxtype[1],
             year=1994, 
             pch=0,
             .before=0)) %>%
  mutate(level=cumprod(1 + pch),
         pch=ifelse(year==1994, NA_real_, pch)) %>%
  ungroup

levels %>% 
  filter(stabbr=="NY", taxtype=="tottax")

pewtax <- pewtax_base %>%
  full_join(levels %>% select(-pch), 
            by = c("stabbr", "stname", "sheet", "vartype", "revsource", "taxtype", "year")) %>%
  arrange(stabbr, vartype, taxtype, year)

# caution: US data do not have major taxes, just the total
count(pewtax, year)
count(pewtax, stabbr, stname)

pewtax %>% filter(stabbr=="US")
saveRDS(pewtax, here::here("data", "details", "pewtax.rds"))

```

## State sales tax bases

```{r}
# Table 2.4.5U. Personal Consumption Expenditures by Type of Product
tpcfn <- "TPC State Sales Tax Mapping.xlsx"
tpc_namerows <- read_excel(here::here("raw_data", tpcfn), sheet="Tax treatment by PCE", range="E3:BD5")
tpc_namerows
colnames <- tpc_namerows %>%
  filter(row_number()==1) %>%
  select(Alabama:Wyoming) %>% 
  unlist(., use.names=FALSE)
colnames

tpcdf <- read_excel(here::here("raw_data", tpcfn), sheet="Tax treatment by PCE", range="A11:BD117",
                 col_names=c("line", "level", "pcename",  "tpc_id", "tpc_category", colnames))
tpcdf
glimpse(tpcdf)

stmap <- tpcdf %>%
  mutate(line=as.integer(line)) %>%
  pivot_longer(all_of(colnames), names_to = "stabbr", values_to = "taxstatus") %>%
  filter(!is.na(taxstatus))
count(stmap, line)

stmap %>% filter(stabbr=="NY")

# we need a mapping between the tpc line numbers and the new bea line numbers
remapfn <- "TPC_NIPA_remapped.xlsx"
remap <- read_excel(here::here("raw_data", remapfn), sheet="remap", range="A7:J116",
                 col_names=TRUE)
glimpse(remap)
tail(remap)


glimpse(nipa)
data(package="BEAData")
NIPAvars %>%
  filter(tabnum=="2.4.5U")

tabpce <- NIPAvars %>%
  filter(tabnum=="2.4.5U") %>%
  select(tabnum, tabname, vname, line, vdesc2=vdesc) %>%
  left_join(nipa %>% filter(freq=="A"), by="vname") %>%
  # put total pce on every record
  group_by(year) %>%
  mutate(pce=value[line==1]) %>%
  ungroup

tabpce %>% 
  filter(line %in% 1:2, year %in% c(1990, 2020)) %>% 
  arrange(year, line)
# tabpce %>% filter(line==190) %>%
#   select(vname, vdesc, vdesc2) %>%
#   distinct

gstbase1 <- stmap %>%
  rename(line_tpc=line) %>% 
  left_join(remap %>% select(line=line_new_numeric, line_tpc=line_tpc_numeric), by="line_tpc") %>%  # line is bea's new line number
  left_join(tabpce, by="line")
glimpse(gstbase1)

check <- gstbase1 %>%
  select(line, line_tpc, vdesc, pcename, tpc_category) %>%
  distinct()

# save and then create final sales tax base
saveRDS(gstbase1, here::here("data", "details", "gstbase_details.rds"))

```

```{r}
gstbase_details <- readRDS(here::here("data", "details", "gstbase_details.rds"))
count(gstbase_details, taxstatus)

gstbase <- gstbase_details %>%
  mutate(gststatus=ifelse(taxstatus %in% c("T", "TX"),
                          "taxable", 
                          "untaxed")) %>%
  select(stabbr, year, gststatus, value, pce) %>%
  group_by(stabbr, year, gststatus, pce) %>%
  summarise(value=sum(value), .groups = "drop") %>%
  pivot_wider(names_from = gststatus, values_fill = 0) %>%
  mutate(pct_potential=taxable / (taxable + untaxed) * 100,
         pct_pce=taxable / pce * 100,
         name="gstbase") %>% 
         # realnom="nominal") %>%
  select(stabbr, year, name, value=taxable, pct_potential, pct_pce)
summary(gstbase)

# some quick exploration and checks
sts <- c("CA", "NY", "AZ", "FL")
gstbase %>%
  filter(year >= 1990, stabbr %in% sts) %>%
  ggplot(aes(year, pct_pce, colour=stabbr)) +
  geom_line() +
  geom_point()

# interesting that the taxbase volatility is only slightly related to the taxable percent
# gstbase %>%
#   filter(year >= 1989) %>%
#   group_by(stabbr) %>%
#   arrange(year) %>%
#   mutate(pch=pchya(value, year)) %>%
#   summarise(pchsd=sd(pch, na.rm=TRUE), pct_pce=median(pct_pce, na.rm=TRUE), .groups="drop") %>%
#   filter(pct_pce > 0) %>%
#   # filter(!stabbr %in% c("HI")) %>%
#   ggplot(aes(x=pct_pce, y=pchsd, label=stabbr)) +
#   geom_point(colour="blue", size=0.5) +
#   geom_text(colour="blue", size=2) +
#   geom_smooth(method = "lm") +
#   theme_bw()

saveRDS(gstbase, here::here("data", "details", "gstbase.rds"))  

```

## assemble and save taxdata

```{r save_taxdata}
censustax <- readRDS(here::here("data", "details", "census_clean_tax.rds"))
census_gstiitadj <- readRDS(here::here("data", "details", "census_gstiitadj.rds"))
pewtax <- readRDS(here::here("data", "details", "pewtax.rds"))
gstbase <- readRDS(here::here("data", "details", "gstbase.rds"))

save(censustax, census_gstiitadj, pewtax, gstbase, file=here::here("data", "taxdata.RData"))

load(file = here::here("data", "taxdata.RData"), verbose=TRUE)

```

# OLD QCEW BELOW HERE
```{r qcew_read_raw}
df <- readRDS(paste0(qcewdir, "qcew_raw.rds"))
glimpse(df)
count(df, own_code, own_title)  # 0-9, we want all, unfortunately -- or maybe sum 5 private, 8 govt
count(df, size_code, size_title)  # all 0
count(df, industry_code, industry_title) # ~ 4,700 - need to clean this up
count(df, agglvl_code, agglvl_title)
count(df, year)
# do we need disclosure_code? N means not disclosed
count(df, disclosure_code)
check <- df %>% filter(disclosure_code=="N")
count(check, total_annual_wages)  # all zero
count(check, as.numeric(annual_avg_estabs_count))
count(check, industry_title)

```

```{r qcew_clean_industry_codes}
# clean the industry codes
icodes1 <- df %>% select(industry_code, industry_title) %>% distinct
icodes1 %>% filter(industry_code=="21112")

icodes2 <- icodes1 %>%
  arrange(industry_code, desc(industry_title)) %>%
  group_by(industry_code) %>%
  # delete codes that start with numeric, where possible
  mutate(n=n(), numcode=str_sub(industry_title, 1, 1) %in% as.character(0:9), nnum=sum(numcode),
         drop=(n==2) & (nnum==1) & numcode) %>%
  filter(!drop) %>%
  ungroup %>% 
  select(industry_code, industry_title)

icodes3 <- icodes2 %>%
  arrange(industry_code, desc(industry_title)) %>%
  group_by(industry_code) %>%
  mutate(n=n(), naics=(str_sub(industry_title, 1, 5)=="NAICS"), nnaics=sum(naics),
         drop=(n==2) & (nnaics==1) & naics) %>%
  filter(!drop) %>%
  ungroup %>% 
  select(industry_code, industry_title)

# we have a few oddball code titles that start with naics, so edit the titles
pattern <- "^[-[:digit:][:punct:][:space:]]*"
icodes4 <- icodes3 %>%
  mutate(bad_title=ifelse(str_sub(industry_title, 1, 5)=="NAICS", TRUE, FALSE),
         industry_title=ifelse(bad_title, str_remove(industry_title, "NAICS "), industry_title),
         industry_title=ifelse(bad_title, str_remove(industry_title, pattern), industry_title))

indcodes <- icodes4 %>%
  select(industry_code, industry_title) %>%
  arrange(industry_code)
# verify that we have every code and that there are not duplicates  
misscodes <- setdiff(unique(icodes1$industry_code), unique(indcodes$industry_code))  # good, empty
indcodes %>%
  count(industry_code) %>%
  filter(n > 1)  # good nothing
saveRDS(indcodes, here::here("data", "details", "qcew_indcodes.rds"))

```

```{r qcew_clean_and_save}
# make a slimmed-down file with all industries, but just own_code 0, size_code 0
# with just the necessary variables
indcodes <- readRDS(here::here("data", "details", "qcew_indcodes.rds"))

# I think we can drop disclosure_code for our purposes
df1 <- df %>%
  filter(size_code=="0") %>%
  select(-c(starts_with("lq_"), starts_with("oty_"), size_code, size_title, qtr, disclosure_code))
glimpse(df1)
count(df1, year)
count(df1, industry_code, industry_title)
count(df1, agglvl_code, agglvl_title)

# bring in state abbreviations, convert numeric variables to numeric, drop more unneeded variables, save
df2 <- df1 %>%
  mutate(stfips=str_sub(area_fips, 1, 2),
         stfips=ifelse(stfips=="US", "00", stfips),
         year=as.integer(year)) %>%
  mutate(across(c(annual_avg_estabs_count, annual_avg_emplvl,
                  total_annual_wages, taxable_annual_wages,
                  annual_contributions, annual_avg_wkly_wage, avg_annual_pay), as.numeric)) %>%
  left_join(stcodes %>% select(stfips, stabbr), by="stfips") %>%
  select(-industry_title) %>%
  left_join(indcodes, by="industry_code")
glimpse(df2)
count(df2, stfips, stabbr, area_fips, area_title)


df3 <- df2 %>%
  select(-c(stfips, area_fips, area_title)) %>%
  select(c(stabbr, year, agglvl_code, agglvl_title, industry_code, industry_title,
           own_code, own_title, everything()))
glimpse(df3)

# save this before filtering out observations
saveRDS(df3, here::here("data", "details", "qcew_states_details.rds"))

# rm(df1, df2, df3, dfl, df2)
# rm(icodes1, icodes2, icodes3, icodes4)

```

```{r qcew_slim_down}
# now slim it down further to just what we need
qcew1 <- readRDS(here::here("data", "details", "qcew_states_details.rds"))
glimpse(qcew1)
count(qcew1, own_code, own_title)
#   own_code own_title                                              n
#   <chr>    <chr>                                              <int>
# 1 0        Total Covered                                       1643
# 2 1        Federal Government                                289022
# 3 2        State Government                                  211598
# 4 3        Local Government                                  394429
# 5 5        Private                                          3000725
# 6 8        Total Government                                    1643
# 7 9        Total U.I. Covered (Excludes Federal Government)      31

# ONETIME - write US for a recent year, to figure out hierarchical structure of industry data
# qcew1 %>%
#   filter(stabbr=="US", year==2019) %>%
#   write.xlsx(here::here("scratch", "qcewus.xlsx"))

qcew1 %>%
  filter(stabbr=="US", year==2019) %>%
  group_by(agglvl_code, agglvl_title) %>%
  summarise(total_annual_wages=sum(total_annual_wages))
# 10-14 are identical, 15-18 are close, 94 is just UI, 95, just US govt
# 13 is NAICS supersector
# 14 is NAICS 3-digit

# a large state
qcew1 %>%
  filter(stabbr=="NY", year==2019) %>%
  group_by(agglvl_code, agglvl_title) %>%
  summarise(total_annual_wages=sum(total_annual_wages))
# 50, 51 (both totals) are identical
# 52 by Domain by ownership sector is ~10% smaller (what is this??) Goods-producing, Service-producing
# 53 NAICS supersector is close to total
# 54 NAICS sector pretty close
# 55 NAICS 3-digit pretty close

# a small state
qcew1 %>%
  filter(stabbr=="RI", year==2019) %>%
  group_by(agglvl_code, agglvl_title) %>%
  summarise(total_annual_wages=sum(total_annual_wages))
# 50, 51 (both totals) are identical
# 52 by Domain by ownership sector is IDENTICAL
# 53 NAICS supersector is close to total
# 54 NAICS sector pretty close
# 55 NAICS 3-digit pretty close

# spot checking other states and years looks similar
# It looks like I can reach down to 3-digit without much trouble

# what industries and owner codes do we have in our preferred agglvl_codes?
qcew1 %>%
  filter(stabbr=="US", year==2019, agglvl_code=="13") %>%
  count(industry_code, industry_title)
# 13 supersector has 12 industries for US

qcew1 %>%
  filter(stabbr=="NY", year==2019, agglvl_code=="53") %>%
  count(industry_code, industry_title)
# 53 supersector has 12 industries in NY


qcew1 %>%
  filter(stabbr=="US", year==2019, agglvl_code=="13") %>%
  count(own_code, own_title)
# fed gov, state gov, local gov, private

qcew1 %>%
  filter(stabbr=="NY", year==2019, agglvl_code=="53") %>%
  count(own_code, own_title)
# fed gov, state gov, local gov, private

# looks like us agg 13, state agg 53, summed over own codes, is perfect
qcew2 <- qcew1 %>%
  filter((stabbr=="US" & agglvl_code=="13") |
           (stabbr!="US" & agglvl_code=="53")) %>%
  rename(indcode=industry_code, indname=industry_title) %>%
  group_by(stabbr, year, indcode, indname) %>%
  summarise(estabs=sum(annual_avg_estabs_count),
            emp=sum(annual_avg_emplvl),
            wages=sum(total_annual_wages), .groups="drop")

# add a sum over industries for each state and year, to allow easy checking
qsums <- qcew2 %>%
  group_by(stabbr, year) %>%
  summarise(across(c(estabs, emp, wages), sum), .groups="drop") %>%
  mutate(indcode="0000", indname="Total")

qcew3 <- bind_rows(qcew2, qsums) %>%
  arrange(stabbr, year, indcode, indname)

# how do the totals for states and years comport with those in the raw data?
rawcheck <- qcew1 %>%
  filter((stabbr=="US" & agglvl_code=="10") |
           (stabbr!="US" & agglvl_code=="50")) %>%
  select(stabbr, year, estabs=annual_avg_estabs_count,
         emp=annual_avg_emplvl, wages=total_annual_wages) %>%
  mutate(type="raw")

calccheck <- qcew3 %>%
  filter(indcode=="0000") %>%
  select(-indcode, -indname) %>%
  mutate(type="calc")

check <- bind_rows(rawcheck, calccheck)
glimpse(check)

check %>%
  filter(stabbr=="US") %>%
  ggplot(aes(year, estabs, colour=type)) +
  geom_line() +
  geom_point()

# work through each set of states and each variable
stnums1 <- 1:12
stnums2 <- 13:24
stnums3 <- 25:36
stnums4 <- 37:48
stnums5 <- 49:53

stnums <- stnums5
vname <- "wages"  # estabs, emp, wages
check %>%
  filter(stabbr %in% state.abb[stnums]) %>%
  select(stabbr, year, type, value=all_of(vname)) %>%
  ggplot(aes(year, value, colour=type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~stabbr, ncol=3, scales="free") +
  ggtitle(vname)

# notes:
#  estabs
#     2000 or 2001 calc a little funny in some states
#     NJ 1999 raw is odd
#     VT RI 2001 odd
#     WY 2001, 2002 odd
#  emp
#     DE calc odd
#     MI calc a little rough, not bad
#     MT calc 2009 a little off
#     RI, VT calc a little rough
#     WY calc rough, way off in 2002
#  wages
#     generally very good
#     WY way off in 2002, a little rough in other years

# maybe some smoothing for emp needed, but not much
# wages prob very good except pay attention to WY near 2002

# to give us more options, bring in the raw totals
qcew4 <- qcew3 %>%
  mutate(type="calc") %>%
  bind_rows(rawcheck %>% mutate(indcode="0000", indname="Total"))
glimpse(qcew4)

qcew4 %>%
  filter(stabbr=="US", year %in% c(1990, 2020)) %>%
  arrange(year, indcode, type)

qcew4 %>%
  filter(stabbr=="NY", year %in% c(1990, 2020)) %>%
  arrange(year, indcode, type)

# save this file and include it in the state economic dataset
saveRDS(qcew4, here::here("data", "details", "qcew_states.rds"))

```

```{r qcew_shares}
qcew_states <- readRDS(here::here("data", "details", "qcew_states.rds"))
count(qcew_states, indcode, indname)
#  1 0000    Total                                 3286
#  2 1011    Natural resources and mining          1643
#  3 1012    Construction                          1643
#  4 1013    Manufacturing                         1643
#  5 1021    Trade, transportation, and utilities  1643
#  6 1022    Information                           1643
#  7 1023    Financial activities                  1643
#  8 1024    Professional and business services    1643
#  9 1025    Education and health services         1643
# 10 1026    Leisure and hospitality               1643
# 11 1027    Other services                        1643
# 12 1028    Public administration                 1643
# 13 1029    Unclassified                           803

qcew_states %>%
  filter(stabbr=="US", year==2019, type=="calc") %>%
  mutate(pctemp=emp / emp[indcode=="0000"] * 100,
         pctwages=wages / wages[indcode=="0000"] * 100) %>%
  arrange(-pctwages)


qxwalk <- read_csv2("indcode;	indname; group
0000; Total; total
1011; Natural resources and mining; mining
1012; Construction; construct
1013; Manufacturing; manuf
1021; Trade, transportation, and utilities; ttu
1022; Information; info
1023; Financial activities; finance
1024; Professional and business services; profbus
1025; Education and health services; edhealth
1026; Leisure and hospitality; lesiure
1027; Other services; other
1028; Public administration; other
1029; Unclassified; other
")
qxwalk

qcew_grouped <- qcew_states %>%
  filter(type=="calc") %>%
  left_join(qxwalk %>% select(-indname), by="indcode") %>%
  select(stabbr, year, emp, wages, group) %>%
  pivot_longer(c(emp, wages)) %>%
  group_by(stabbr, year, group, name) %>%
  summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from = group, values_from = value, values_fill = 0) %>%
  select(-c(other, total), stabbr, year, name, everything())

# check sums
check <- qcew_grouped %>%
  mutate(checksum = rowSums(across(construct:other)),
         pdiff=checksum / total * 100 - 100) 
check %>%
  filter(abs(pdiff) > 1e-6)
# good


qcew_shares <- qcew_grouped %>%
  mutate(across(construct:total, ~ .x / total))
check <- qcew_shares %>%
  mutate(checksum = rowSums(across(construct:other)),
         diff=checksum / total * 100 - 100) 
check %>%
  filter(abs(diff) > 1e-6)

qcew_shares %>%
  filter(name=="wages") %>%
  filter(stabbr %in% c("AL", "MS", "CT", "NY")) %>%
  ggplot(aes(year, finance, colour=stabbr)) +
  geom_line() +
  geom_point()

qcew_shares %>%
  filter(stabbr %in% c("AL", "MS", "CT", "NY")) %>%
  ggplot(aes(year, finance, colour=name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ stabbr, ncol=2, scales = "fixed")

summary(qcew_shares)
  
# looks good for saving
saveRDS(qcew_grouped, here::here("data", "details", "qcew_grouped.rds"))
saveRDS(qcew_shares, here::here("data", "details", "qcew_shares.rds"))

```


# explore selected data ---------------------------------------------------
# not sure I'm going to use this
# http://ippsr.msu.edu/public-policy/correlates-state-policy
# x_sales_taxes
# cspp_june_2021.csv
fn <- "cspp_june_2021.csv"

df <- read_csv(file.path(scratchdir, fn))
df2 <- df %>%
  select(year:state_icpsr, x_sales_taxes)
df2 %>% tail(20)
df2 %>%
  filter(!is.na(x_sales_taxes)) %>%
  count(year) # ~1946-2014, 2019 (???)
# Caughey, Devin, and Christopher Warshaw. 2015. “The Dynamics of State
# Policy Liberalism, 1936–2014.” American Journal of Political Science,
# September. doi: 10.1111/ajps.12219.
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZXZMJB
# For 2019: “State Sales Tax Rates” (provided by the Sales Tax Institute)
# https://www.salestaxinstitute.com/resources/rates
