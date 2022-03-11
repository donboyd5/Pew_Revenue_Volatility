
## assemble and save taxdata ----
censustax <- readRDS(here::here("data", "details", "census_keeptax.rds"))
census_gstiitadj <- readRDS(here::here("data", "details", "census_gstiitadj.rds"))
pewtax <- readRDS(here::here("data", "details", "pewtax.rds"))
gstbase <- readRDS(here::here("data", "details", "gstbase.rds"))

# one last file
taxshares <- censustax %>%
  pivot_wider(values_fill=0) %>%
  mutate(across(-c(stabbr, year, tottax), ~ .x / tottax))

# do a quick check to make sure the shares ordinarily add approximately to 1
# THEY WILL NOT ALWAYS DO SO BECAUSE I HAVE filtered out some bad data
# sharesums <- taxshares %>%
#   select(-tottax) %>%
#   pivot_longer(-c(stabbr, year)) %>%
#   group_by(stabbr, year) %>%
#   summarise(sum=sum(value), .groups="drop")
# all.equal(sharesums$sum, rep(1, nrow(sharesums)))  # checks whether the sums==1, within a tolerance
# quantile(sharesums$sum)
# sharesums %>% filter(sum < .99)  # we have 19 that are < .99; CO 2019 is the only concerning one
# rm(sharesums)

save(censustax, census_gstiitadj, pewtax, gstbase, taxshares, file=here::here("data", "taxdata.RData"))


