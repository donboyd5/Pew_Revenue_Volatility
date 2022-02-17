# Data prep for state tax revenue volatility


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
# at present no additional functions are needed
# source(here::here("r", "functions_maps.r"))
# source(here::here("r", "functions_measures.r"))
# source(here::here("r", "functions_plots.r"))
# source(here::here("r", "functions_utility.r"))

# NOTE: If a section name starts with "ONETIME", it creates and
# saves data that will later be read from files. The data only need to
# be created one time (or updated and re-saved if an update is desired).


# ONETIME download data -------------------------------
# CAUTION:

#   The following programs download files from the internet and save them
#   in the raw_data project directory.

#   If you do this you may download revised versions of data and get slightly
#   different numbers than I get.

#   To reproduce the numbers I have, you will need to use the versions of the
#   files I have downloaded, which are already stored in the project.

#   Thus, do not do the following unless you don't mind getting 
#   some different numbers...

# source(here::here("r", "data_prep_subs", "sub_ONETIME_downloads.r"))

# In an emergency, you can retrieve copies of the original 
# versions of the data from here:

#    .../raw_data/backup_ONETIME_data/


# "sub_..." r files that create the data -----------------------------
# I recommend stepping through each "sub_..." r file line by line 
# rather than sourcing the file - but if you source the files
# everything still should work as intended

# note that the "sub_..." r files do not load libraries - load them per above


# general.RData -----------------------------------------------------------
# inputs:  none, other than official R data (e.g., state.abb)
# outputs: data/general.RData
source(here::here("r", "data_prep_subs", "sub_general.r"))
# load(file = here::here("data", "general.RData"), verbose=TRUE)


# capgainsagi.Rdata ----------------------------------------------------------------
# inputs: 
#    raw_data/soi/NationalCapitalGains.xlsx
#    raw_data/soi/histab6.xls
#    raw_data/soi/19intaba.xls
# outputs:
#    data/soi/capgainsagi.RData
source(here::here("r", "data_prep_subs", "sub_capgainsagi.r")) # needs sfy_startmonths in memory
# load(file=here::here("data", "capgainsagi.RData"), verbose=TRUE)


# econ_national.RData -----------------------------------------------------
# inputs:  BEAData::nipa
# outputs: data/econ_national.RData
source(here::here("r", "data_prep_subs", "sub_econ_national.r")) # needs sfy_startmonths in memory
# load(file = here::here("data", "econ_national.RData"), verbose=TRUE)


# gdp_state.RData ---------------------------------------------------------------
#  state gdp, state fiscal year basis, real and nominal (sgdpfy)
source(here::here("r", "data_prep_subs", "sub_gdp_state.r"))
# load(file = here::here("data", "gdp_state.RData"), verbose=TRUE)


# taxdata.RData --------------------------------------------------------------
# do this in steps
source(here::here("r", "data_prep_subs", "sub_censustax.r"))
# inputs:  sgtax.a
# outputs:
#   data/details/census_clean_tax.rds

source(here::here("r", "data_prep_subs", "sub_censustax_rateadjust.r"))
# inputs: data/details/census_clean_tax.rds
# outputs: data/details/census_gstiitadj.rds

source(here::here("r", "data_prep_subs", "sub_pewtax.r"))
# inputs: data/details/census_clean_tax.rds
# outputs: data/details/census_gstiitadj.rds

source(here::here("r", "data_prep_subs", "sub_gstbase.r"))
# inputs:
# outputs:

source(here::here("r", "data_prep_subs", "sub_taxdata.r"))
# inputs: in data/details:
#   census_clean_tax.rds
#   census_gstiitadj.rds
#   pewtax.rds
#   gstbase.rds
# outputs:  data/taxdata.RData
load(file = here::here("data", "taxdata.RData"), verbose=TRUE)


# load data -- only for testing purposes -----------------------------
# DO NOT RUN UNTIL AFTER FILES HAVE BEEN CREATED
# useful for verifying that what's in each file is what's intended
load(file = here::here("data", "general.RData"), verbose=TRUE)
load(file = here::here("data", "econ_national.RData"), verbose=TRUE)
# load(file = here::here("data", "econ_state.RData"), verbose=TRUE)
load(file = here::here("data", "gdp_state.RData"), verbose=TRUE)
load(file = here::here("data", "taxdata.RData"), verbose=TRUE)


# source(here::here("r", "data_prep_subs", "sub_qcew_state.r"))
# load(file = here::here("data", "sub_qcew_state.RData"), verbose=TRUE)
# save(qcew_slim, file = here::here("data", "qcew_state.RData"))


