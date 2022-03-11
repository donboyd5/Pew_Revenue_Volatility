
# OLD QCEW BELOW HERE
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
