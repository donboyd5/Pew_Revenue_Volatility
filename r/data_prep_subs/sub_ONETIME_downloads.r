# ONETIME national data downloads ---------------------------
#.. ONETIME: get and save national unemployment rate ----
# https://fred.stlouisfed.org/series/UNRATE
fredr_set_key(fred_apikey)
unrate <- fredr("UNRATE", frequency = "q")
saveRDS(unrate, here::here("raw_data", "unrate.rds"))

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