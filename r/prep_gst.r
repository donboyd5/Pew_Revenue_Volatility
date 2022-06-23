
glimpse(vbase)
count(vbase, name)
# gst, gstadj, gstbase, gstpew
summary(vbase)


data(package="BEAData")

glimpse(sgdp.a_all)
tmp <- sgdp.a_all
count(tmp, line, indname)

glimpse(nipa)
glimpse(NIPAvars)
# Table 1.1.5. Gross Domestic Product - dur, ndur, goods, service
# Table 2.3.5. Personal Consumption Expenditures by Major Type of Product

pcevars <- NIPAvars |> 
  filter(table=="T20305") |> 
  select(line, vname, vdesc) |> 
  arrange(line)
vlist <- unique(pcevars$vname)

rpcevars <- NIPAvars |> 
  filter(table=="T20306") |> 
  select(line, vname, vdesc) |> 
  arrange(line)
rvlist <- unique(rpcevars$vname)
setdiff(rvlist, paste0(str_sub(vlist, 1, -2), "X"))
#  D236RX Residual, Chained Dollars, Level  

pcedata <-
  nipa |> 
  filter(freq=="A", vname %in% vlist) |> 
  select(vname, vdesc, year, value) |> 
  arrange(vname, year)

# add pch, pdtrend
pcebase <- pcedata %>%
  select(name=vname, year, value) %>%
  mutate(stabbr="US", realnom="nominal") %>%
  arrange(stabbr, name, realnom, year) %>% 
  group_by(stabbr, name, realnom) %>%
  filter(year %in% 1959:2020) %>% # keep 1959 so we have percent change
  mutate(pch=value / value[match(year - 1, year)] - 1) %>%
  # drop the 1959 values which will have NA for pch
  filter(year %in% 1960:2020) %>%
  mutate(pchtrend=hptrend(pch),
         dpchtrend=pch - pchtrend,
         trend=hptrend(value),
         pdtrend=value / trend - 1) %>%
  ungroup

gstbase <- vbase %>%
  filter(realnom=="nominal", name %in% c("gdp", "gst"), stabbr=="US")

pcenames <- pcevars |>
  select(line, vname, vdesc) |> 
  distinct()

# 1     1 DPCERC Personal consumption expenditures, Current Dollars, Level                                                   
# 2     2 DGDSRC Goods, Current Dollars, Level                                                                               
# 3     3 DDURRC Durable goods, Current Dollars, Level                                                                       
# 4     4 DMOTRC Motor vehicles and parts, Current Dollars, Level                                                            
# 5     5 DFDHRC Furnishings and durable household equipment, Current Dollars, Level                                         
# 6     6 DREQRC Recreational goods and vehicles, Current Dollars, Level                                                     
# 7     7 DODGRC Other durable goods, Current Dollars, Level                                                                 
# 8     8 DNDGRC Nondurable goods, Current Dollars, Level                                                                    
# 9     9 DFXARC Food and beverages purchased for off-premises consumption, Current Dollars, Level                           
# 10    10 DCLORC Clothing and footwear, Current Dollars, Level                                                               
# 11    11 DGOERC Gasoline and other energy goods, Current Dollars, Level                                                     
# 12    12 DONGRC Other nondurable goods, Current Dollars, Level                                                              
# 13    13 DSERRC Services, Current Dollars, Level   

ns(pcebase)
ns(gstbase)

gstpce <- bind_rows(gstbase, pcebase) |> 
  left_join(pcenames |> rename(name=vname), by = "name")
saveRDS(gstpce, here::here("data", "gstpce.rds"))

gstvol <- map_dfr(list(c(1970, 2020)), 
                  f_vol, 
                  vars=unique(gstpce$name),
                  vbase=gstpce |> 
                    select(stabbr, name, realnom, year, value, pch, pchtrend, dpchtrend, trend, pdtrend)) |> 
  left_join(pcenames |> rename(name=vname), by = "name")
saveRDS(gstvol, here::here("data", "gstvol.rds"))


summary(gstpce)
count(gstpce, year)
count(gstpce, name)

pcenames

vars <- c("gst", "gdp", "DPCERC", "DDURRC")
vars <- c("gst", "gdp", "DPCERC", "DDURRC")
vars <- c("gst", "DFXARC", "DPCERC")
vars <- c("gst", "DDURRC", "DNDGRC", "DSERRC")
# vars <- c("gst", "DFXARX", "DPCERX")
gstpce |> 
  filter(name %in% vars, year >= 1970) |> 
  ggplot(aes(year, pdtrend, colour=name)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0)
  
gstvol |> 
  select(name, period, sre, srese, pdtrendiqr, pchiqr) |> 
  left_join(pcenames |> rename(name=vname), by = "name") |> 
  arrange(desc(pdtrendiqr)) |> 
  filter(is.na(line) | line %in% c(1, 2, 3, 9, 14))


tmp <- count(nipa, vname, vdesc)
tmp2 <- tmp %>% 
  filter(str_detect(vdesc, coll("Consumption", ignore_case = TRUE)))

tmp2 <- tmp %>% 
  filter(str_detect(vdesc, coll("Consumption expenditures", ignore_case = TRUE)))

