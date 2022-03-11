# ONETIME national data downloads ---------------------------

# All externally  obtained data, other than that in packages, should
# be downloaded here and saved in a project directory.
# This way the data are "frozen" at the time of download.

# CAUTION: These will overwrite existing data previously downloaded.

# In an emergency, you can retrieve copies of my original 
# versions of the data from here:

#    .../data/raw_data/backup_ONETIME_data/

# and then copy them to the appropriate subdirectories below


#.. ONETIME: get and save national unemployment rate ----
# https://fred.stlouisfed.org/series/UNRATE
fredr_set_key(fred_apikey)  # fred_apikey was defined in r/constants_system.r
unrate <- fredr("UNRATE", frequency = "q")
saveRDS(unrate, here::here("raw_data", "bls", "unrate.rds"))


#.. ONETIME: download the TPC agi data ----
url <- "https://www.taxpolicycenter.org/file/185517/download?token=cYMr2RRH"
dsoi <- here::here("data", "raw_data", "tpc")
fn <- "historical_source_0.xlsx"
fpath <- file.path(dsoi, fn)
download.file(url, fpath, mode="wb")


#.. ONETIME: create long time-series of agi directly from the IRS ----
# agi pre-1990
download.file(url="https://www.irs.gov/pub/irs-soi/histab6.xls",
              here::here("raw_data", "soi", "histab6.xls"),
              mode="wb")

# agi 1990-2019
download.file(url="https://www.irs.gov/pub/irs-soi/19intaba.xls",
              here::here("raw_data", "soi", "19intaba.xls"),
              mode="wb")

# also see https://www.irs.gov/pub/irs-soi/05in01an.xls


# ONETIME: download TPC database of sales tax rates ----
url <- "https://www.taxpolicycenter.org/file/186569/download?token=cO82Ddqm"
fn <- "state_sales_tax_2.xlsx"
download.file(url,
              # here::here("data", "raw_data", "tpc", fn),
              here::here("archive", fn),
              mode="wb")


# ONETIME: download NBER top income tax rate data ----
url <- "https://users.nber.org/~taxsim/state-rates/maxrate.dat"
download.file(url, here::here("data", "raw_data", "nber", "maxrate.dat"))


# ONETIME: download the Tax Policy Center (TPC) income tax rate file manually ----
# from https://www.taxpolicycenter.org/statistics/state-individual-income-tax-rates
# as the url they give does not seem to work
# and save it here:
#   fpath <- here::here("data", "raw_data", "tpc", "state_income_tax_rates_0.xlsx")




