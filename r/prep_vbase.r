
# prep vbase -- a stabbr-name-year file with volatility-related values on each record
# this is the precursor to volatility calculations



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
source(here::here("r", "functions", "functions_runs.r"))
source(here::here("r", "functions", "functions_measures.r"))


# get data ----------------------------------------------------------------
load(file = here::here("data", "general.RData"), verbose=TRUE)
load(file = here::here("data", "econ_national.RData"), verbose=TRUE)
load(file = here::here("data", "recession_features.RData"), verbose=TRUE)
load(file = here::here("data", "capgainsagi.RData"), verbose=TRUE)
load(file = here::here("data", "gdp_state.RData"), verbose=TRUE)
load(file = here::here("data", "taxdata.RData"), verbose=TRUE)


# Stack the data ----------------------------------------------------------
# -   Stack files so that we can construct sdpch and hppch volatility measures for any item.
# -   We need stabbr, name, year, value for all files
# -   Then bring in any rhs vars we need


#.. prep national data ---------------------------------------------------
# gdpfy gdppi
gdpfy_prep <- gdpfy %>%
  mutate(stabbr="US", name="gdp", realnom="nominal") %>%
  select(stabbr, name, realnom, year, value=gdp)

gdppi_prep <- gdppi %>%
  mutate(stabbr="US", name="gdppi", realnom="price") %>%
  select(stabbr, name, realnom, year, value=gdppi)

cg_prep <- capgains %>%
  mutate(stabbr="US", name="capgains", realnom="nominal") %>%
  select(stabbr, name, realnom, year, value=capgains)
glimpse(cg_prep)

summary(cg_prep) # 1954-2031


#.. prep state GDP data ---------------------------------------------------
# sgdpfy   count(sgdpfy, name)
# sgdpgrouped_sfy
# qcew_grouped

sgdp_prep <- sgdpfy %>%  # has gdp, rgdp
  # indicate if data were real in original source
  mutate(name=ifelse(name=="rgdp", "rgdp_source", name),  
         realnom=case_when(name=="gdp" ~ "nominal",
                           name=="rgdp_source" ~ "real",
                           TRUE ~ "ERROR")) %>%
  select(stabbr, name, realnom, year, value)
summary(sgdp_prep)  # 1964-2020
count(sgdp_prep, name)
count(sgdp_prep, realnom)
count(sgdp_prep, stabbr) # 50 states

gspsectors_prep <- sgdpgrouped_sfy %>%  # 2005+
  select(-c(total, sum)) %>%
  pivot_longer(-c(stabbr, year)) %>%
  mutate(name=paste0("sgdp_", name),
         realnom="nominal") %>%
  select(stabbr, name, realnom, year, value)
summary(gspsectors_prep)
count(gspsectors_prep, stabbr)  # 50 states, DC, US
count(gspsectors_prep, name)


#.. prep state tax data --------------------------------------------------
tax_prep <- bind_rows(censustax, census_gstiitadj) %>%
  # we do not have gstadj data for the period excluded, so it is safe to do this
  filter(!(stabbr == "DE" & name == "gst")) %>% # only 3 obs, 1951-53
  # negative in 2014, maybe later figure out how to salvage this
  filter(!(stabbr == "OH" & name == "cit")) %>% 
  mutate(realnom="nominal")

glimpse(tax_prep)
summary(tax_prep)  # 1959-2020

# gstbase, created in prep_data, maps BEA consumption components to state
# sales tax bases
gst_prep <- gstbase %>%
  filter(stabbr != "DC") %>%
  mutate(realnom="nominal") %>%
  select(stabbr, name, realnom, year, value)
glimpse(gst_prep)
summary(gst_prep)  # 1929-2020


# stack -------------------------------------------------------------------
stack1 <- bind_rows(
  gdpfy_prep, gdppi_prep, cg_prep,  # US economy
  sgdp_prep, gspsectors_prep, # maybe qcew_slim?? qcew_prep,  # state economies
  tax_prep, gst_prep) %>%  # state tax-related variables
  filter(year %in% 1959:2020) %>%
  # drop any vectors that have no positive values
  group_by(stabbr, name, realnom) %>%
  mutate(nopos=nopos(value)) %>%
  ungroup %>%
  filter(!nopos) %>%
  select(-nopos)
glimpse(stack1)
summary(stack1)
count(stack1, stabbr)
count(stack1, name)


# create real versions of the nominal variables, 2021 $
gdppi
summary(gdppi)
rstack1 <- stack1 %>%
  filter(realnom=="nominal") %>%
  left_join(gdppi %>% select(year, igdppi),
            by="year") %>%
  mutate(value=value * igdppi,
         realnom="real") %>%
  select(stabbr, name, realnom, year, value)
summary(rstack1)

# combine into a single stacked file just US states
stack <- bind_rows(stack1, rstack1) %>%
  filter(stabbr %in% c("US", state.abb))

glimpse(stack)
summary(stack) # goes back as far as 1929
count(stack, realnom)
check <- count(stack, name)

stack %>%
  filter(is.na(value))

stack %>%
  filter(stabbr=="US") %>%
  count(name)

stack %>%
  filter(stabbr=="US", name=="gdp") %>% ht


## Create vbase - THIS IS A CRUCIAL DATA FRAME -----

# vbase has:
# -   stabbr
# -   name (type of data item - econ variables, tax variables)
# -   realnom (categorical - nominal, price, real)
# -   value
# -   pch (vs year ago)
# -   trend (computed as hptrend)
# -   pdtrend (pct difference from trend)


# start in 1959 so we will have % change from 1960 forward
vbase <- stack %>%
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
summary(vbase)
saveRDS(vbase, here::here("data", "vbase.rds"))


# OLD stuff -----

# vbase_old <- readRDS(here::here("data", "vbase_old.rds"))
# 
# vcheck <- bind_rows(
#   readRDS(here::here("data", "vbase.rds")) %>%
#     mutate(type="new"),
#   readRDS(here::here("data", "vbase_old.rds")) %>%
#     mutate(type="old")) %>%
#   select(stabbr, name, realnom, year, type, trend) %>%
#   pivot_wider(names_from = type,
#               values_from = trend) %>%
#   mutate(diff=new - old,
#          pdiff=diff / old) %>%
#   arrange(-abs(pdiff))
# 
# vbase_old %>%
#   filter(stabbr=="KY", realnom=="real", name=="sevtax") %>%
#   select(year, value, trend) %>%
#   pivot_longer(-year) %>%
#   ggplot(aes(year, value, colour=name)) +
#   geom_line() +
#   geom_point()
