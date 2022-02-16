## Census Bureau tax data

### Clean tax data
# Steps:

# 1.  Get Census state govt tax data from bdata (previously created)
# 2.  Keep desired taxes and create an other taxes category
# 3.  Identify and explore anomalies

# get_tax}
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

# Identify and explore anomalies:
# 
# -   missing
# -   zero
# -   missing or zero at start (always assumed bad)
# -   missing or zero at end (always assumed bad)
# -   missing in middle
# -   zero in middle
# -   negative in middle (logs won't work -- only 1 found)
# -   short runs (fewer than X positive values in a row)

# anomalies
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


taxkeep <- anomalies_v3 %>%
  select(stabbr, name, year, value) %>%
  ungroup
summary(taxkeep)
glimpse(taxkeep)
census_clean_tax <- taxkeep
saveRDS(census_clean_tax, here::here("data", "details", "census_clean_tax.rds"))
