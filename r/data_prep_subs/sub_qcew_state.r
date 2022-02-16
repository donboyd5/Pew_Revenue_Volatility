


# BLS QCEW ----------------------------------------------------------------
#.. ONETIME download_qcew ----
# https://data.bls.gov/cew/data/files/1990/csv/1990_annual_by_area.zip
# dir <- "/media/don/data/qcew/"

# for(year in 1990:2020){
#   zipname <- paste0(year, "_annual_by_area.zip")
#   # print(zipname)
#   url <- paste0("https://data.bls.gov/cew/data/files/", year, "/csv/", zipname)
#   print(url)
#   savename <- file.path(qcewdir, zipname)
#   # print(savename)
#   download.file(url, savename, mode="wb")
# }

# qcew_extract_and_save
# year <- 2015
read_states <- function(year){
  # read all of the statewide files and the US file for a single year
  print(year)
  
  # get path to zip archive
  zname <- paste0(year, "_annual_by_area.zip")
  zpath <- file.path(qcewdir, zname)
  
  # file names
  stfiles <- str_subset(unzip(zpath, list = TRUE)$Name, coll("Statewide"))
  usfile <-  str_subset(unzip(zpath, list = TRUE)$Name, coll("U.S. TOTAL"))
  files <- c(usfile, stfiles)
  
  # we are going to use "map" to create connections to all these files
  # the function we map over must have file name as its first argument
  # but our connection function archive_read takes the path first
  # so we create a small function that reverses the order, taking filename first
  # archive_read2 <- function(fname, zpath) archive_read(zpath, fname)
  # now we can create the connections to all the desired files
  # we're going to need ~50 connections per year and we'll do this for 20 years
  # so we close all connections before starting so we don't run out of them
  # closeAllConnections()
  # cons <- purrr::map(files, archive_read2, zpath)
  # # finally, read the files en masse
  # df <- vroom(cons[[1]], col_types = cols(.default = "c"))
  # vroom(archive_read(zpath, files[1]), col_types = cols(.default = "c"))
  # vroom(unz(zpath, file=files[1]), col_types = cols(.default = "c"))
  # read_csv(unz(zpath, file=files[1]), col_types = cols(.default = "c"))
  # 
  # read_all_zip <- function(zpath, ...) {
  #   filenames <- unzip(zpath, list = TRUE)$Name
  #   vroom(purrr::map(files, ~ unz(zpath, .x)), ...)
  # }
  # 
  df <- vroom::vroom(purrr::map(files, ~ unz(zpath, .x)), 
                     col_types = cols(.default = "c"))
  
  #  read_one_file <- function(fpath){
  #    print(fpath) 
  #    read_csv(unz(zpath, file=fpath), 
  #             col_types = cols(.default = "c")) 
  #  }
  # map_dfr(files, read_one_file)
  #  
  saveRDS(df, file.path(qcewdir, paste0("qcew", year, ".rds")))
}

# save a file for each year
walk(1990:2020, read_states)
# after checking files, detach vroom because it can cause problems
detach("package:vroom")

# read those files and combine
# we do this in 2 steps because vroom is a little buggy and reading and
# combining in one step appears to cause memory problems
f <- function(year, qcewdir, stcodes){
  # year <- 2012
  print(year)
  df <- readRDS(file.path(qcewdir, paste0("qcew", year, ".rds")))
  # slim the file down before combining
  df2 <- df %>%
    filter(as.integer(agglvl_code) %in% c(10:13, 50:53)) %>% # go down to the supersector level
    mutate(stfips=str_sub(area_fips, 1, 2),
           stfips=ifelse(stfips=="US", "00", stfips),
           own_code=as.integer(own_code),
           agglvl_code=as.integer(agglvl_code),
           year=as.integer(year)) %>%
    left_join(stcodes %>% select(stfips, stabbr), by="stfips") %>%
    select(stabbr,
           year,
           own=own_code,
           ownf=own_title,
           ind=industry_code,
           indf=industry_title,
           agg=agglvl_code,
           aggf=agglvl_title,
           emp=annual_avg_emplvl,
           wages=total_annual_wages) %>%
    mutate(across(c(emp, wages), as.numeric))
  
}
# yq("2020Q4")
# df11 <- f(2011, qcewdir, stcodes)
# df12 <- f(2012, qcewdir, stcodes)
# df13 <- f(2013, qcewdir, stcodes)
# bind_rows(df11, df12, df13)

df <- map_dfr(1990:2020, f, qcewdir, stcodes)
count(df, year)
count(df %>% 
        filter(year %in% c(2000, 2020)), year, stabbr) %>% 
  pivot_wider(names_from = year, values_from = n)
# memory()

xwalk <- read_delim(
  " ind; vname; vdesc
10; allind; 10 Total, all industries
101; goods; 101 Goods-producing
1011; natres; 1011 Natural resources and mining
1012; constr; 1012 Construction
1013; manuf; 1013 Manufacturing
102; service; 102 Service-providing
1021; tpu; 1021 Trade, transportation, and utilities
1022; info; 1022 Information
1023; finact; 1023 Financial activities
1024; profbus; 1024 Professional and business services
1025; edhealth; 1025 Education and health services
1026; leisure; 1026 Leisure and hospitality
1027; othersvc; 1027 Other services
1028; pubadmin; 1028 Public administration
1029; unclass; 1029 Unclassified
", delim=";", trim_ws=TRUE)
xwalk

df2 <- df %>%
  filter(agg %in% c(10, 13, 50, 53)) %>%
  mutate(level=case_when(agg %in% c(10, 50) ~ "total",
                         agg %in% c(13, 53) ~ "supersector"),
         vname=factor(ind, levels=xwalk$ind, labels=xwalk$vname))

# collapse
df3 <- df2 %>%
  group_by(stabbr, year, vname) %>%
  summarise(emp=sum(emp), wages=sum(wages), .groups = "drop")

qcew_slim <- df3 %>%
  select(-wages) %>%
  pivot_wider(names_from = vname, 
              values_from = emp) %>%
  mutate(empsum = select(., -c(stabbr, year, allind)) %>% rowSums(na.rm=TRUE),
         pct=empsum / allind)
summary(qcew_slim) # not too bad

# saveRDS(qcew_slim, paste0(qcewdir, "qcew_slim.rds"))

# make a slimmed down file because we'll rarely want all this data

st <- "NV"
sts <- c("FL", "ND", "NV", "MI")
qcew_slim %>%
  filter(stabbr %in% sts) %>%
  group_by(stabbr) %>%
  mutate(ivalue=constr / constr[year==2006]) %>%
  ungroup %>%
  ggplot(aes(year, ivalue, colour=stabbr)) +
  geom_line() +
  geom_point()


# save_qcew_state
save(qcew_slim, file = here::here("data", "qcew_state.RData"))
# verify 
# load(file = here::here("data", "qcew_state.RData"), verbose=TRUE)
