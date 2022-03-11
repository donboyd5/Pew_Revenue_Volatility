
# Create state-year datbase of sales tax rate, combining information on
# sales tax rates from:
#     TPC: the Brookings-Urban Tax Policy Center;
#          TPC tax-rate data used here were downloaded in sub_ONETIME_downloads.r
#          they are in an excel file that is read below
#     CSPP: The Correlates of State Policy Project database 
#          data are in the CRAN package csppData, accessed through the CRAN
#          package csppData


library(cspp)

# get TPC sales tax rates -------------------------------------------------

f <- function(year){
  print(year)
  if(year <= 2004){
    cols <- cell_cols("A:C")
    cnames <- c("stname", "junk", "srate")
  } else if(year <= 2012) {
    cols <- cell_cols("A:B")
    cnames <- c("stname", "srate")
  } else if(year <= 2020){
    cols <- cell_cols("A:C")
    cnames <- c("stname", "junk", "srate")
  } else if(year==2021){
    cols <- cell_cols("B:D")
    cnames <- c("stname", "junk", "srate")
  }
  # read from TPC Excel file with sales tax rates
  df <- read_excel(here::here("data", "raw_data", "tpc", "state_sales_tax_2.xlsx"), sheet=as.character(year),
                   range=cols, col_names=cnames, col_types="text")
  df %>%
    select(stname, srate) %>% 
    mutate(year=year)
}
# f(2000)

years <- setdiff(2000:2021, c(2001, 2002, 2005, 2009))
df <- purrr::map_dfr(years, f)

# clean the state name and keep states and DC
df2 <- df %>%
  mutate(stname2=str_remove_all(stname, "[0-9,()]") %>% str_trim(),
         stabbr=stabbr(stname2))

check <- count(df2, stabbr, stname2, stname)  # inspect check
check %>% filter(is.na(stabbr))
count(df2, stabbr) %>%
  filter(n!=18)

# 1 DE        15
# 2 MT        15
# 3 NH        15
# 4 OR        15
expand.grid(stabbr=sts, year=years) %>%
  left_join(df2 %>% select(stabbr, year) %>% 
              mutate(included=TRUE), by = c("stabbr", "year")) %>%
  filter(is.na(included)) %>%
  arrange(stabbr, year)
# all 4 states are missing 2000, 2003, 2004
# inspection shows they were not in the spreadsheet in those years
# so it looks good

df3 <- df2 %>%
  filter(!is.na(stabbr)) %>%
  select(stabbr, year, srate)

# srate:
#   drop any footnotes
#   convert srate to numeric
#   round to 4 decimal places


df4 <- df3 %>%
  mutate(sraten=str_trim(srate),
         sraten=str_split(sraten, pattern=" ", simplify=TRUE)[,1],
         sraten=as.numeric(sraten),
         sraten=round(sraten, 4))

df4 %>%
  filter(stabbr=="UT", year==2011) # UT had a footnote in 2011

# by inspection, df4 looks good
# fill in missing rates, downward
df5 <- expand.grid(stabbr=c(state.abb, "DC"), year=2000:2021) %>%
  left_join(df4 %>% select(stabbr, year, gstrate=sraten), by = c("stabbr", "year")) %>%
  mutate(rawrate=gstrate) %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  fill(gstrate) %>%
  ungroup %>%
  mutate(fillin=is.na(rawrate) & !is.na(gstrate))


df5 %>% 
  filter(fillin)

df5 %>%
  select(stabbr, year, gstrate) %>%
  pivot_wider(names_from = year, values_from = gstrate)


gst_rates_tpc <- df5 %>%
  select(stabbr, year, fillin, gstrate) %>%
  filter(!is.na(gstrate))

gst_rates_tpc %>%
  select(-fillin) %>%
  pivot_wider(names_from = year, values_from = gstrate)

summary(gst_rates_tpc)
saveRDS(gst_rates_tpc, here::here("data", "details", "gst_rates_tpc.rds"))


# get CSPP rate data ----
all_variables <- cspp::get_var_info()
all_variables %>%
  filter(str_detect(long_desc, pattern=coll("tax", ignore_case=TRUE))) %>%
  filter(str_detect(long_desc, pattern=coll("rate", ignore_case=TRUE))) %>%
  select(variable, years, short_desc)

tmp <- all_variables %>% filter(variable=="x_sales_taxes")
tmp$sources
glimpse(tmp)


# x_sales_taxes 1946-2014
# x_tax_rate_rich       1977-2012

# Full dataset
all_data <- get_cspp_data()
glimpse(all_data)
# check <- ns(all_data)
# str_subset(check, "st")
# all_data %>% select(st, st_ec, st_soc, state)

df <- all_data %>%
  select(stabbr=st, year, gstrate=x_sales_taxes)
summary(df)  # 1900-2020 w/2614 NAs
summary(df %>% filter(!is.na(gstrate)))  # 1946-2019

# in one version of cspp there were duplicate records
# they appear to have fixed that, but the code below makes sure we only have one
# do we have any cases with more than one record per state per year?
df %>%
  group_by(stabbr, year) %>%
  mutate(n=n()) %>%
  ungroup %>%
  filter(n > 1)

df2 <- df %>%
  distinct() %>%
  filter(!(is.na(gstrate) | gstrate==0))
# check again
df2 %>%
  group_by(stabbr, year) %>%
  mutate(n=n()) %>%
  ungroup %>%
  filter(n > 1)
# good, no dups

df2 %>% filter(stabbr=="NY") # sanity check

df3 <- expand.grid(stabbr=unique(df2$stabbr), year=min(df$year):max(df$year)) %>%
  left_join(df2, by=c("stabbr", "year")) %>%
  mutate(gstrate_raw=gstrate) %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  fill(gstrate) %>%
  ungroup %>%
  mutate(fillin=is.na(gstrate_raw) & !is.na(gstrate)) 
summary(df3)  

df3 %>%
  filter(fillin) %>%
  group_by(year) %>%
  summarise(n=n())

df3 %>%
  filter(fillin, !year %in% 2015:2018) %>%
  count(stabbr)  # AK

gst_rates_cspp <- df3 %>%
  select(stabbr, year, fillin, gstrate) %>%
  filter(!is.na(gstrate))

check <- gst_rates_cspp %>%
  select(-fillin) %>%
  pivot_wider(names_from = year, values_from = gstrate)
# inspect the data frame


summary(gst_rates_cspp)
saveRDS(gst_rates_cspp, here::here("data", "details", "gst_rates_cspp.rds"))


# compare and then combine the tpc and cspp sales tax rates ----
df1 <- readRDS(here::here("data", "details", "gst_rates_tpc.rds")) %>%
  mutate(type="tpc")

df2 <- readRDS(here::here("data", "details", "gst_rates_cspp.rds")) %>%
  mutate(type="cspp")

df3 <- bind_rows(df1, df2) %>%
  select(-fillin) %>%
  arrange(stabbr, type, year)

st <- "NY"

df3 %>%
  filter(stabbr==st) %>%
  ggplot(aes(year, gstrate, colour=type)) +
  geom_line() +
  geom_point()

df3 %>%
  filter(stabbr==st) %>%
  pivot_wider(names_from = type, values_from = gstrate)

df3 %>%
  pivot_wider(names_from = type, values_from = gstrate) %>%
  filter(!is.na(cspp), !is.na(tpc)) %>%
  filter(cspp != tpc)  # 63 differences

# create a combined file, favoring tpc for 2000+
df4 <- df3  %>%
  pivot_wider(names_from = type, values_from = gstrate) %>%
  mutate(name="gstrate",
         value=ifelse(year >= 2000, tpc, cspp),
         value=ifelse(stabbr=="AK", NA_real_, value)) %>%
  filter(!is.na(value))
summary(df4)

gst_rates <- df4 %>%
  select(stabbr, name, year, value) %>%
  arrange(stabbr, name, year)
summary(gst_rates)
saveRDS(gst_rates, here::here("data", "gst_rates.rds"))


# check ----
gst_rates <- readRDS(here::here("data", "gst_rates.rds"))
glimpse(gst_rates)
gst_rates
summary(gst_rates) # 1946-2021

