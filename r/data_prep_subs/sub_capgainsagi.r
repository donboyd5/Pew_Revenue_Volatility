

# National capital gains --------------------------------------------------
# historical capital gains for the U.S., annual, calendar (tax) year
# primary sources are U.S. Treasury for data before 1995, and CBO for 1995+
# note that ~ 2020+ is CBO forecasts
# see spreadsheet for further details on sources
fn <- "NationalCapitalGains.xlsx"
sheet <- "CapGains"
capgains1 <- read_excel(here::here("data", "raw_data", "soi", fn), sheet=sheet, range="A5:B83", 
                       col_types = c())

capgains2 <- capgains1 %>%
  mutate(year=as.integer(year),
         capgains=capgains * 1000)  # put it in millions of dollars
glimpse(capgains2)
summary(capgains2)  # 1954-2031

capgains <- capgains2 %>%
  filter(year <= 2020)


# create agi time series directly from IRS data -----------------------------------------
# This is agi only not its components
# $ billions
agi1 <- read_excel(here::here("data", "raw_data", "soi", "histab6.xls"),
                   col_names = c("year", "aginipa", "agi"),
                   range="A7:C62")

# $ thousands
agi2 <- read_excel(here::here("data", "raw_data", "soi", "19intaba.xls"),
                   range="A4:AE117")
agi2a <- agi2 %>%
  filter(row_number() == 113)

agi2b <- agi2a %>%
  select(-1) %>%
  pivot_longer(cols=everything(), names_to = "year", values_to = "agi") %>%
  mutate(year=as.integer(year), agi=as.numeric(agi))

# combine and put in $ millions
agi3 <- bind_rows(
  agi1 %>%
    select(year, agi) %>%
    mutate(agi=agi * 1000, src="histab6.xls"),
  agi2b %>%
    mutate(agi=agi / 1000, src="19intaba.xls"))

# compare
agi3 %>%
  ggplot(aes(year, agi, colour=src)) +
  geom_line() +
  geom_point()

# looks good so splice and save
agi <- agi3 %>%
  group_by(year) %>%
  mutate(keep=(n()==2 & src=="19intaba.xls") | n()==1) %>%
  ungroup %>%
  filter(keep) %>%
  select(-keep)

# saveRDS(agi, here::here("data", "raw_data", "soi", "agi.rds"))
# rm(agi1, agi2, agi2a, agi2b, agi3)


# agi data from TPC -------------------------------------------------------
# TPC is the Urban-Brookings Tax Policy Center
# data were previously downloaded and saved in sub_ONETIME_downloads.r
dsoi <- here::here("data", "raw_data", "tpc")
fn <- "historical_source_0.xlsx"
fpath <- file.path(dsoi, fn)

vnames <- c("year", "nret", "agi", "wages", "interest", "dividends",
            "busincnet", "netcgll", "incother", "taxbc", "taxliab", "amt")
tpc1 <- read_excel(fpath, 
                   col_names = vnames,
                   col_types = "text", 
                   skip=6)
glimpse(tpc1)
ht(tpc1)

agitpc <- tpc1 %>%
  select(year, agi, wages, busincnet, netcgll) %>%
  mutate(year=as.integer(year)) %>%
  filter(year >= 1955) %>%
  mutate(across(-c(year), as.numeric)) %>%
  pivot_longer(-year) %>%
  arrange(name, year)
glimpse(agitpc)
ht(agitpc)
count(agitpc, name)


# save data ---------------------------------------------------------------
save(capgains, agi, agitpc,
     file = here::here("data", "capgainsagi.RData"))


