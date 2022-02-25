
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
  select(year, di=value)
ht(dpi)



# save econ_national.RData ------------------------------------------------
save(gdpfy, gdppi, pi, dpi,
     file = here::here("data", "econ_national.RData"))



# CIGXM for recession features ----
#    CIGXM chained dollar GDP (or, for certain early data, quantity indexes)
#       quarterly basis, for characterizing recessions
#       where GDP = C + I + G + (X - M)
#             C = consumption
#             I = investment
#             G = government
#         X - M = exports minus imports

# Table 1.1.6. Real Gross Domestic Product, Chained Dollars
tab <- BEAData::NIPAvars %>%
  filter(tabnum == "1.1.6") %>%
  arrange(line)
tab

# define the CIGXM variables we will use to characterize recessions
xwalk <- read_csv("vname,	vdesc, name, junk
A191RX, Gross domestic product, rgdp, GDP
DPCERX, Personal consumption expenditures, rpce, C
DGDSRX, Goods, rpcegoods,
DDURRX, Durable goods, rpcedgoods,
DNDGRX, Nondurable goods, rpcendgoods,
DSERRX, Services, rpceservices,
A006RX, Gross private domestic investment, rgpdi, I
A007RX, Private fixed investment, rpfi,
A014RX, Change in private inventories, rinventories,
A019RX, External balance of goods and services, rnetx, XM
A020RX, Exports of goods and services, rx, X
A021RX, Imports of goods and services, rm, M
A822RX, Government consumption expenditures and gross investment, rgov, G
A960RX, Residual, rres, na
")
xwalk
# problems(xwalk)


# get NIPA quarterly data (chained dollars) for the xwalk variables
cigxmchain <- BEAData::nipa %>%
  filter(freq=="Q", vname %in% xwalk$vname) %>%
  right_join(xwalk %>% select(vname, name), by="vname")
glimpse(cigxmchain)
count(cigxmchain, name, vname, vdesc)
# note that we only have breakdown of rpce into goods, services for 79 quarters (2002)

# we can go back further using quantity indexes - good for growth rates but not
# relative importance of variables (indexed to 2012=100)
# Table 1.1.3. Real Gross Domestic Product, Quantity Indexes
tabqi <- BEAData::NIPAvars %>%
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
A007RA, Private fixed investment, rpfi,
B020RA, Exports of goods and services, rx, X
B021RA, Imports of goods and services, rm, M
B822RA, Government, rgov, G
")
xwalkqi
# get NIPA quarterly data

cigxmqi <- BEAData::nipa %>%
  filter(freq=="Q", vname %in% xwalkqi$vname) %>%
  right_join(xwalkqi %>% select(vname, name), by="vname") %>%
  mutate(vname=factor(vname, levels=xwalkqi$vname)) %>%
  arrange(vname, date)
glimpse(cigxmqi)
count(cigxmqi, vname)
count(cigxmqi, vname, name, vdesc)

#.... combine the chained cigxm data and the quantity indexed data ----
cigxmq <- bind_rows(cigxmchain %>% 
                     mutate(valtype="chain"),
                   cigxmqi %>%
                     mutate(valtype="qidx"))

saveRDS(cigxmq, here::here("data", "details", "cigxmq.rds"))

