
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
