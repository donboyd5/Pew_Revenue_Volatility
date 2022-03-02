# Data prep for state tax revenue volatility

# This program prepares most of the data needed in the volatility analysis.

# The basic idea is:
#   for each category of data -- for example:
#     national economy,
#     state economy,
#     state tax revenue
#   retrieve the data -- possibly multiple sources for a category
#   put it into a uniform format -- generally with the following columns
#     year (generally state fiscal year)
#     stabbr (state abbreviation)
#     name (e.g., variable name)
#     value -- in xxs of dollars, if it is a money variable
#   save the data frames for a given category in an RData file for the category

# This makes it easy to retrieve the category files and "stack" several
# data frames on top of each other, since they all have the same format.



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


# NOTE: If a section name starts with "ONETIME", it creates and
# saves data that will later be read from files. The data only need to
# be created one time (or updated and re-saved if an update is desired).


# ONETIME download data -------------------------------
# CAUTION:

#   sub_ONETIME_downloads.r downloads files from the internet and saves them
#   in the raw_data project directory.

#   If you do this you may download revised versions of data and get slightly
#   different numbers than I get.

#   To reproduce the numbers I have, you will need to use the versions of the
#   files I have downloaded, which are already stored in the project.

#   Thus, do not do the following unless you don't mind getting 
#   different numbers.

# source(here::here("r", "data_prep_subs", "sub_ONETIME_downloads.r"))

# In an emergency (if you overwrite files I previously downloaded), you can
# retrieve copies of the original versions of the data from here:

#    .../raw_data/backup_ONETIME_data/

# just copy the files into the right subdirectories.



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
#          data/details/cigxmq.rds -- quarterly, for recession features
source(here::here("r", "data_prep_subs", "sub_econ_national.r")) # needs sfy_startmonths in memory
# load(file = here::here("data", "econ_national.RData"), verbose=TRUE)


# recession_features.RData -----------------------------------------------------
# inputs:  BEAData::nipa
#          bdata::recessions
#          data/details/cigxmq.rds
# outputs: data/rec_features.RData
source(here::here("r", "data_prep_subs", "sub_recession_features.r")) # needs sfy_startmonths in memory
# load(file = here::here("data", "recession_features.RData"), verbose=TRUE)


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

# This next step must be done AFTER censustax because censustax is an input
source(here::here("r", "data_prep_subs", "sub_censustax_rateadjust.r"))
# inputs: data/details/censustax.rds
# outputs: data/details/census_gstiitadj.rds

source(here::here("r", "data_prep_subs", "sub_pewtax.r"))
# inputs: data/details/census_clean_tax.rds
# outputs: data/details/pewtax.rds

source(here::here("r", "data_prep_subs", "sub_gstbase.r"))
# inputs: TPC data
# outputs: data/details/gstbase_details.rds, data/details/gstbase.rds

source(here::here("r", "data_prep_subs", "sub_taxdata.r"))
# inputs: in data/details:
#   censustax.rds
#   census_gstiitadj.rds
#   pewtax.rds
#   gstbase.rds
# outputs:  data/taxdata.RData
load(file = here::here("data", "taxdata.RData"), verbose=TRUE)


# do a quick check to make sure the shares add approximately to 1
# sharesums <- taxshares %>%
#   select(-tottax) %>%
#   pivot_longer(-c(stabbr, year)) %>%
#   group_by(stabbr, year) %>%
#   summarise(sum=sum(value), .groups="drop")
# all.equal(sharesums$sum, rep(1, nrow(sharesums)))  # checks whether the sums==1, within a tolerance
# quantile(sharesums$sum)
# sharesums %>% filter(sum < .99)  # we have 19 that are < .99; CO 2019 is the only concerning one
# rm(sharesums)


# load data -- only for testing purposes -----------------------------
# DO NOT RUN UNTIL AFTER FILES HAVE BEEN CREATED
# useful for verifying that what's in each file is what's intended
load(file = here::here("data", "general.RData"), verbose=TRUE)
load(file = here::here("data", "econ_national.RData"), verbose=TRUE)
load(file = here::here("data", "recession_features.RData"), verbose=TRUE)
load(file=here::here("data", "capgainsagi.RData"), verbose=TRUE)
# load(file = here::here("data", "econ_state.RData"), verbose=TRUE)
load(file = here::here("data", "gdp_state.RData"), verbose=TRUE)
load(file = here::here("data", "taxdata.RData"), verbose=TRUE)

