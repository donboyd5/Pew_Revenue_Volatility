# ONETIME national data downloads ---------------------------

# CAUTION: These will overwrite existing data previously downloaded.

# In an emergency, you can retrieve copies of my original 
# versions of the data from here:

#    .../raw_data/backup_ONETIME_data/

# and then copy them to the appropriate subdirectories below (bls or soi)


#.. ONETIME: get and save national unemployment rate ----
# https://fred.stlouisfed.org/series/UNRATE
fredr_set_key(fred_apikey)  # fred_apikey was defined in r/constants_system.r
unrate <- fredr("UNRATE", frequency = "q")
saveRDS(unrate, here::here("raw_data", "bls", "unrate.rds"))

#.. ONETIME: create long time-series of agi ----
# agi pre-1990
download.file(url="https://www.irs.gov/pub/irs-soi/histab6.xls",
              here::here("raw_data", "soi", "histab6.xls"),
              mode="wb")

# agi 1990-2019
download.file(url="https://www.irs.gov/pub/irs-soi/19intaba.xls",
              here::here("raw_data", "soi", "19intaba.xls"),
              mode="wb")

# also see https://www.irs.gov/pub/irs-soi/05in01an.xls