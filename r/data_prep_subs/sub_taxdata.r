
## assemble and save taxdata
censustax <- readRDS(here::here("data", "details", "census_clean_tax.rds"))
census_gstiitadj <- readRDS(here::here("data", "details", "census_gstiitadj.rds"))
pewtax <- readRDS(here::here("data", "details", "pewtax.rds"))
gstbase <- readRDS(here::here("data", "details", "gstbase.rds"))

save(censustax, census_gstiitadj, pewtax, gstbase, file=here::here("data", "taxdata.RData"))


