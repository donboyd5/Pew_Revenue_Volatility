## Census Bureau tax data

### Clean tax data
# Steps:

# 1.  Get raw Census state govt tax data from bdata package
# 2.  Keep desired taxes and create an other taxes category
# 3.  Identify and explore anomalies

# get_tax}
# tax1 has untouched data from Census, only the items we want, long format
# tax2 is same, but with the other tax variable


# glimpse(sgtax.a)  # 2020 end year; most values from ~1951+
# count(sgtax.a, year) %>% as.data.frame()
# count(sgtax.a, item, desc) # identify key tax items



#**************************************************************************
#                                                                         
# functions for examining runs of data ------------------------------------
#
#**************************************************************************

allpos <- function(x) {
  # TRUE if ALL elements in a vector are (1) not NA, and (2) strictly positive
  all(!is.na(x) & x > 0)
  # check allpos:
  # allpos(c(1, 2, 3)) # TRUE
  # allpos(c(-1, 2, 3)) # FALSE
  # allpos(c(0, 2, 3)) # FALSE
  # allpos(c(NA, 2, 3)) # FALSE
}

nopos <- function(x){
  # TRUE if there are NO positive values in a vector
  !any(x > 0, na.rm=TRUE)
  # check nopos:
  # nopos(c(0, 0, 0)) # TRUE - no positive values
  # nopos(c(0, 0, 1)) # FALSE - there is a positive
  # nopos(c(NA, 0, 0)) # TRUE - no positives
  # nopos(c(NA, 0, 1)) # FALSE - there is a positiver value
}


# this next function detects consecutive zero and/or NA values in a vector
# (for example, in tax X in state Y)
# based on previous examination of the data, I have
# concluded these represent gaps in the data, and that in such a situation
# we will want to keep only the more-recent run of the data
# identifying these consec values and later converting them to NA makes it
# easy to drop them
consec_zna <- function(x){
  # identify consecutive zero or NA values in a vector
  # TRUE for a vector element if this element is zero or NA, AND: 
  #   previous (i.e., lag) element, or
  #   next (i.e., lead) element, or
  #   both (lag and lead) are either zero or NA
  # in other words it returns a vector that is TRUE in the places where
  # there are any consecutive zero--NA values
  zna <- function (x1) is.na(x1) | x1==0 # is each vector element zero or NA?
  
  # define lag and lead; note that:
  #   lag of first element doesn't exist, so can't be zero or NA
  #   same for lead of last element
  lastx <- length(x)
  xlag <- c(Inf, lag(x)[-1]) 
  xlead <- c(lead(x)[-lastx], Inf) # lead of last element doesn't exist, ...
  
  z <- zna(x)
  zlag <- zna(xlag)
  zlead <- zna(xlead)
  consec_zna <- z & (zlag | zlead)
  consec_zna
  # uncomment this next line to show the details
  # tibble(x, z, zlag, zlead, consec_zna)
  
  # tests:
  # x <- c(1, 2, 3, 4)  # all FALSE
  # x <- c(0, 1, 2, 3)  # all FALSE
  # x <- c(0, 0, 1, 2)  # TRUE TRUE FALSE FALSE
  # x <- c(NA, NA, 1, 2) # TRUE TRUE FALSE FALSE
  # x <- c(NA, 0, 1, 2) # TRUE TRUE FALSE FALSE
  # x <- c(0, 1, NA, NA, 1, 2) # FALSE FALSE  TRUE  TRUE FALSE FALSE
  # x <- c(0, 1, 0, NA, 1, 2) # FALSE FALSE  TRUE  TRUE FALSE FALSE
  # consec_zna(x)
}


last_na <- function(x) {
  # identify the rightmost NA in a vector
  #   e.g., for c(1, NA, 2, NA, 5) the 4th element is the last NA
  #   so the function would return 4
  inax <- which(is.na(x)) # get indexes of the NA values of x
  # get the maximum index; inax will be empty if there are no NA values,
  # which would trigger a warning, so suppress warnings
  lna <- suppressWarnings(max(inax)) 
  lna
  
  # tests:
  # x <- c(1, 2, 3, 4, 5)
  # x <- c(1, 2, NA, NA, 5)
  # x <- c(1, NA, 2, NA, 5)
  # x <- c(1, 2, NA, NA, NA)
  # last_na(x)
}


first_notna <- function(x){
  # identify the first non-NA value in a vector
  #   e.g., for c(1, NA, 2, NA, 5) the 4th element is the last NA
  #   so the function would return 4
  inotnax <- which(!is.na(x)) # get indexes of the NA values of x
  # get the minimum index; inotnax will be empty if there are no not-NA values,
  # which would trigger a warning, so suppress warnings
  fnotna <- suppressWarnings(min(inotnax)) 
  fnotna
  
  # tests:
  # x <- c(1, 2, 3, 4, 5)
  # x <- c(NA, 2, NA, NA, 5)
  # x <- c(1, NA, 2, NA, 5)
  # x <- c(1, 2, NA, NA, NA)
  # first_notna(x)
  
}


# the next two functions examine the tail end of a vector (for example the
# last 20 years of a tax X in state Y) to count the length of an uninterrupted
# run of data at the tail end
#   recent_gez counts the number of consecutive values that are greater
#     or equal to zero at the tail end of a state-tax vector

recent_gez <- function(x){
  # number of consecutive >= zero values at end of vector
  gez <- !is.na(x) & x >= 0 # which elements are >= zero?
  revgez <- rev(gez)  # reverse the vector so we can start from the back
  recent_gez <- cumsum(revgez) == 1:length(revgez) # count consecutive >= zero from back
  sum(recent_gez)
  # tests:
  # x <- c(1, 1, 1, 2, 2, 2, -1, 1, 2, 3)
  # x <- c(1, 2, 3, 4, -1, NA, 1, 2, 3)
  # x <- 1:6
  # recent_gez(x)
}

recent_notna <- function(x){
  # number of consecutive non-NA values at end of vector
  notna <- !is.na(x) # which elements are not NA?
  revnotna <- rev(notna)  # reverse the vector so we can start from the back
  recent_notna <- cumsum(revnotna) == 1:length(revnotna) # count consecutive not NA from back
  sum(recent_notna)
  # tests:
  # x <- c(1, 1, 1, 2, 2, 2, -1, 1, 2, 3)
  # x <- c(1, 2, 3, 4, -1, NA, 1, 2, 3)
  # x <- 1:6
  # recent_notna(x)
}


#**************************************************************************
#                                                                         
# START CLEANING ------------------------------------
#
#**************************************************************************

# Keep desired taxes 1959+ and create an other taxes category -------------------
# determine the tax items we want
taxnamedf <- sgtax.a %>% select(item, desc) %>% distinct()
taxnamedf

# define items to keep and new variable names we will give them
# the following tax items chosen based on:
#   1) taxnamedf list of taxes in the data,
#   2) my knowledge of tax structures in the states
#   (These are the major tax items plus a super-volatile category, sevtax)
items <- c("C105", "T09", "C109", "T40", "T41", "T53")
varnames <- c("tottax", "gst", "selsalestax", "iit", "cit", "sevtax")

basetax1 <- bdata::sgtax.a %>%
  filter(year >= 1959) %>% # start early enough to allow % change in 1960
  filter(item %in% items) %>%
  mutate(name=factor(item, levels=items, labels=varnames)) %>%
  select(stabbr, name, year, value) %>%
  arrange(stabbr, name, year)
summary(basetax1)  # no missing values but at least one negative

# calculate other tax
basetax <- basetax1 %>%
  # to calc othertax:
  #    calc the sum of the taxes we have selected
  #    subtract this sum from total tax
  
  # to implement this, make a wide file, do the calc, and make it long again
  
  pivot_wider(names_from = name) %>%
  mutate(sumtaxes = 
           rowSums(across(-c(stabbr, year, tottax)),  na.rm=TRUE),
         othertax=tottax - sumtaxes) %>%
  select(-sumtaxes) %>%
  # repivot
  pivot_longer(-c(year, stabbr))
summary(basetax)
count(basetax, name) # good
ht(basetax)


# identify and keep "good" time series ---------------------------------------
# We want the longest-possible run for each state-tax combination,
# from 1959-present, of positive tax values,
# for combinations that still exist 
# (we don't want to study a tax a state no longer uses)

# We will exclude:
#   - state-tax combinations that no longer exist
#   - early periods where the data are all zero or NA
#   - early periods of otherwise-good data that are followed by a
#     gap where the data are zero or NA, and then by good data - in this
#     case we just want the data after the gap

# In other words, keep the longest most-recent run of good data that is
# continuous to the present. (We could consider filling small gaps via
# interpolation but won't unless it salvages an important state-tax combination.)


# create fulltax1 - data frame of of all possible year-state-name combinations ----
# start by creating "gapless" stub of all possible combinations
#  name here refers to type of tax
firstyear <- 1959
lastyear <- 2020
years <- firstyear:lastyear
stus <- c(state.abb, "US")
names <- unique(basetax$name)

stub <- expand_grid(stabbr=stus,
                    year=years,
                    name=names)
stub # all POSSIBLE combinations including those that don't exist in practice
# for example, includes the NJ-sevtax-year combination even though NJ does not
# have a severance tax; 
# also, it includes empty HI records in 1959, even though HI was not a state
# until 1960

# left-join the stub with the basedata, so we have possible combos filled with data
fulltax1 <- stub %>%
  left_join(basetax, by=c("stabbr", "name", "year")) %>%
  arrange(stabbr, year, name) %>%
  select(stabbr, name, year, value)
# count(fulltax1, name)
# ht(fulltax1)
# fulltax1 has all possible combinations of stabbr-name-year
# we're going to examine it to see which runs of data to keep for our analysis

# Examine and clean runs of data -----------------------------------------------------

#.. First, drop vectors where EVERY element is bad, and mark definite keepers ----

# The most-obvious definition of a bad series is one where all values are
# NA or zero - for example, if a state never had an income tax - call this allbad.

# strictest possible definition of a good series is one where all values are
# positive - we will keep all of these series. I call this group allgood.

# However, I also want to keep series that have been good recently -- I define
# this as being continuously good from 1999-2020 (meaning we can calc good
# % change beginning in 2000). I call this group allposrecent.

#   I want to keep these series that are all good recently but not for their entire
#   period. However, I need to examine them and remove bad periods.

# My strictest definition of a good tax series is one where all values
# from 1999-2020 are positive - call this allposrecent. However, we may 
# find some earlier values are bad -- for example a series of all zeros in early
# years that we want to exclude. So we still need to examine them.

# Let's drop allbad, mark allposrecent, mark those that are allposrecent but
# not allgood (they have some bad periods), and decide what to do about these.

# I do this step by step, creating interim files, because
#   (1) this allows examination of results after each step (I don't show all examinations)
#   (2) the files are small so it is not very wasteful

# in the following, if a series has even 1 good value I keep the entire series
# including the NA values


#.. drop HI 1959, identify allbad and allgood, drop allbad --------------------------
fulltax2 <- fulltax1 %>%
  filter(!(stabbr=="HI" & year==1959)) %>% 
  group_by(stabbr, name) %>%
  mutate(allbad=nopos(value),
         allgood=allpos(value),
         ) %>%
  ungroup %>%
  filter(!allbad) %>%
  select(-allbad)

#inspect and move on
summary(fulltax2)

# summarize by stabbr-name and allgood status
check <- fulltax2 %>%
  select(stabbr, name, allgood) %>%
  distinct
count(check, allgood)

# these are the data items we'll need to check:
count(check %>% filter(!allgood), name, sort=TRUE)

# look at the vectors that are not all good
var <- "sevtax"  # sevtax, iit, gst, cit
fulltax2 %>%
  filter(!allgood, name==var) %>%
  ggplot(aes(year, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  facet_wrap(~stabbr, scales="free", ncol=4) +
  ggtitle(var)
# Clearly we want to:
#   get rid of series that do not have positive runs at the end
#   for remaining series, get rid of zeros / NAs at the start
# the problems are primarily in severance tax


#.. get rid of series without positive runs at end --------------------------
fulltax3 <- fulltax2 %>%
  group_by(stabbr, name) %>%
  #  drop series that don't have enough good recent values
  mutate(
    # convert consecutive zeros or NA to missing
    value=ifelse(consec_zna(value), NA_real_, value),
    # convert all values before the last (most recent) NA to missing
    value=ifelse(row_number() <= last_na(value), NA_real_, value)
         ) %>%
  ungroup

#inspect and move on
summary(fulltax3)

# summarize by stabbr-name and allgood status
check <- fulltax3 %>%
  select(stabbr, name, allgood) %>%
  distinct
count(check, allgood)

# these are the data items we'll need to check:
count(check %>% filter(!allgood), name, sort=TRUE)

# look at the vectors that are not all good
var <- "sevtax"  # sevtax, iit, gst, cit
fulltax3 %>%
  filter(!allgood, name==var) %>%
  ggplot(aes(year, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  facet_wrap(~stabbr, scales="free", ncol=4) +
  ggtitle(var)


# look at a few oddballs
fulltax3 %>% filter(stabbr=="KS", name=="sevtax")
fulltax3 %>% filter(stabbr=="MI", name=="sevtax")


#**************************************************************************
#                                                                         
# FINAL CLEANING AND SAVE ------------------------------------
#
#**************************************************************************

# look for and then delete any series that are entirely NA
allna <- fulltax3 %>%
  group_by(stabbr, name) %>%
  summarise(allna=all(is.na(value)), .groups="drop") %>%
  filter(allna)

keeptax1 <- fulltax3 %>%
  left_join(allna, by = c("stabbr", "name")) %>%
  filter(is.na(allna)) %>% # avoid trouble with NA by doing this
  select(stabbr, name, year, value)

# do we have any gaps within the interior of a series (after NA or before )
check <- keeptax1 %>%
  group_by(stabbr, name) %>%
  filter(row_number() >= first_notna(value))
summary(check) # we have no NA values in the interior, but we do have a neg value

check %>%
  filter(any(value < 0))
# the OH CIT in 2014 is negative

keeptax <- keeptax1 %>%
  filter(!(stabbr=="OH" & name=="cit"))

saveRDS(keeptax, here::here("data", "details", "census_keeptax.rds"))  


#**************************************************************************
#                                                                         
# SAVE GRAPH FOR EVERY DATA SERIES WE KEEP ------------------------------------
#
#**************************************************************************
keeptax <- readRDS(here::here("data", "details", "census_keeptax.rds"))  

f <- function(st){
  p <- keeptax %>%
    filter(stabbr==st) %>%
    ggplot(aes(year, value)) +
    geom_line(colour="blue") +
    geom_point(colour="blue") +
    facet_wrap(~name, scales="free", ncol=2) +
    ggtitle(st)
  p
}

pdf(width = 12, height = 8, here::here("scratch", "graphs_rawtax.pdf"))
for(st in unique(keeptax$stabbr)){
  print(st)
  print(f(st))
}
dev.off()


#**************************************************************************
#                                                                         
# COMPARE OLD AND NEW ------------------------------------
#
#**************************************************************************
# compare old and new ----
dfold <- readRDS(here::here("data", "details", "census_clean_tax.rds"))
df <- dfold %>%
  filter(year >= 1959) %>%
  mutate(type="old") %>%
  bind_rows(keeptax %>% mutate(type="new")) %>%
  arrange(stabbr, name, type, year)

comp <- df %>% 
  pivot_wider(names_from = type)

# which series have at least one diff?
diffs1 <- comp %>%
  mutate(newnotna = is.na(old) & !is.na(new),
         oldnotna = !is.na(old) & is.na(new),
         valdiff =  !is.na(old) & !is.na(new) & old != new,
         diff=newnotna | oldnotna | valdiff) %>%
  filter(diff) %>%
  select(stabbr, name) %>%
  distinct
diffs1

# force NV sevtax   pre 1988 to NA
diffs2 <- diffs1 %>%
  left_join(df, by = c("stabbr", "name"))
diffs2

var <- "sevtax"   # sevtax, cit, iit
diffs2 %>%
  filter(name==var) %>%
  ggplot(aes(year, value, colour=type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~stabbr, scales="free", ncol=4) +
  ggtitle(var)

comp %>% filter(stabbr=="AZ", name=="sevtax")
comp %>% filter(stabbr=="NV", name=="sevtax")
comp %>% filter(stabbr=="WY", name=="sevtax")
# I have eliminated:
#   OH cit -- should I have??
#   sevtax:
#      NV we need to force values < 1990 to NA!!!
#      NH dropped and it should be

# saveRDS(census_clean_tax, here::here("data", "details", "census_clean_tax.rds"))


