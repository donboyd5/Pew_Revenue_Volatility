# This subprogram prepares data about the features (characteristics)
# of recessions.

# inputs:
#   bdata::recessions


# package bdata has NBER recession dates
# they are pulled from the NBER recessions page, updated in July 2021
# https://www.nber.org/research/data/us-business-cycle-expansions-and-contractions
# see my github page https://github.com/donboyd5/bdata
# particularly https://github.com/donboyd5/bdata/blob/master/DataConversionPrograms/bdata_recessions.R
# data(package="bdata")
# recessions # 1857, ..., 2020


# mark recessions ---------------------------------------------------------

#.. create data frame with each quarter as peak, trough, expand, or contract ----
firstdate <- as.Date("1947-01-01")  # min(cigxm1$date)
lastdate <- as.Date("2022-07-01") # max(cigxm1$date) + years(1) # allow an extra year

ptrecs <- bdata::recessions %>%
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
# selecting recession quarters to select the contraction AND trough
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


#.. get df with peak and trough quarter of each recession ----------------
rec_peaktrough <- bdata::recessions %>%
  filter(rec_year >= 1969) %>%
  select(recyear=rec_year, peak=peak_quarter, trough=trough_quarter) %>%
  pivot_longer(c(peak, trough), names_to = "qtype", values_to = "date")


#.. duration of each recession, in months --------------------------------
rec_duration <- bdata::recessions %>%
  filter(rec_year >= 1969) %>%
  mutate(duration=interval(peak, trough) %/% months(1)) %>%
  select(recyear=rec_year, duration) # peak=peak_quarter, trough=trough_quarter, 


#.. real gdp (rgdp) and real personal consumtion (rpce) for each recession ----
cigxmq <- readRDS(here::here("data", "details", "cigxmq.rds"))
rec_nipa <- rec_peaktrough %>%
  left_join(cigxmq %>%
              filter(valtype=="chain", name %in% c("rgdp", "rpce")),
            by = "date") %>%
  select(recyear, qtype, name, value) %>%
  pivot_wider(names_from = qtype) %>%
  mutate(pch=trough / peak - 1) %>%
  select(recyear, name, pch) %>%
  pivot_wider(values_from = pch)

unrate <- readRDS(here::here("raw_data", "bls", "unrate.rds"))
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

# cap gains
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
  left_join(rec_nipa, by = "recyear") %>%
  left_join(rec_unrate %>%
              select(recyear, urchange), 
            by = "recyear") %>%
  left_join(rec_cg %>%
              select(recyear=year, cgpch), 
            by = "recyear")
rec_features  

# rec_qtrs, rec_fyears, rec_features,
save(rec_features,
     file = here::here("data", "recession_features.RData"))

