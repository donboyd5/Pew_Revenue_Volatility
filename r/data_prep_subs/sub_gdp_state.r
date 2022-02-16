# ECON_STATE -- create econ_state.RData ----

#.. BEA state GDP data, fiscal year basis; total, no details by industry ----
# data(package="BEAData")
# glimpse(sgdp.q)
# summary(sgdp.q)  # starts in 2005-01-01 so this will not be good
summary(sgdp.a) # 1997-2020, only a bit better
summary(sgdp_spliced.a)  # 1963-2020 -- good; my spliced SIC and NAICS data
count(sgdp_spliced.a, name) # gdp and rgdp

sgdpfy1 <- sgdp_spliced.a %>%
  left_join(sfy_startmonths, by = "stabbr") %>%
  group_by(stabbr, name) %>%
  mutate(lagvalue=value[match(year - 1, year)],
         current_share=(startmonth - 1) / 12,
         value_sfy=value * current_share + lagvalue * (1 - current_share))
sgdpfy1
summary(sgdpfy1) # 1963-2020

sgdpfy <- sgdpfy1 %>%
  ungroup %>%
  filter(!is.na(value_sfy)) %>%
  select(year, stabbr, name, value=value_sfy)
summary(sgdpfy) # 1964-2020

#.. sgdpdetail_sfy industry detail ----
# https://apps.bea.gov/regional/downloadzip.cfm
# SQGDP1__definition just has real, quant index, nominal gdp
# SQGDP2 nominal (?) by sector
# SQGDP8" Name="Chain-type quantity indexes for real GDP by state (2012=100.0), sectors
# SQGDP9" Name="Real GDP by state, sectors
# SQGDP11" Name="Contributions to percent change in real GDP", sector
# thus we want SQGDP9

# dir <- here::here("raw_data", "bea", "/")
afiles <- unzip(file.path(beadir, "SAGDP.zip"), list=TRUE)  # annual
qfiles <- unzip(file.path(beadir, "SQGDP.zip"), list=TRUE) # quarterly

afiles %>% filter(str_detect(Name, "ALL_AREAS"))

# SAGDP9N__ALL_AREAS_1997_2020.csv  # N -- NAICS annual
# SAGDP9S__ALL_AREAS_1977_1997.csv  # S -- SIC annual


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
         indname=Description,  `2005:Q1`:ncol(df2)) %>%
  select(-stfips) %>%
  pivot_longer(-c(stabbr, stname, line, ind, indname)) %>%
  mutate(date=yq(name), value=as.numeric(value), line=as.integer(line))
ht(dfl)  
count(dfl, line, ind, indname)

check <- dfl %>% 
  filter(is.na(value))
# na's are mostly small industries in small states, suppressed to prevent disclosure

# create a clean quarterly series, then state fiscal year
sgdpdetail_qtr <- dfl %>%
  select(stabbr, date, line, ind, indname, value)
summary(sgdpdetail_qtr) # 2005-01-01 - 2021-07-01


sgdpdetail_sfy <- sgdpdetail_qtr %>%
  mutate(year=ifelse(month(date) >= 7,
                     year(date) + 1,
                     year(date)) %>%
           as.integer) %>%
  filter(year < 2022) %>%
  group_by(stabbr, year, line, ind, indname) %>%
  summarise(value=mean(value, na.rm=TRUE),
            .groups="drop")
summary(sgdpdetail_sfy) # 2005-2021

check <- sgdpdetail_sfy %>% 
  filter(is.na(value))
# na's are mostly small industries in small states, suppressed to prevent disclosure


# gdp_shares
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


# save_gdp_state.RData
save(sgdpfy, sgdpdetail_qtr, sgdpdetail_sfy, sgdpgrouped_sfy, sgdpshares_sfy,
     file = here::here("data", "gdp_state.RData"))
# verify loading
# load(file = here::here("data", "gdp_state.RData"), verbose=TRUE)

