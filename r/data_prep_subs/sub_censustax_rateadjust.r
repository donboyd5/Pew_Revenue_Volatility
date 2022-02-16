
## adjust Census iit and gst for rates (calculated externally)
census_clean_tax <- readRDS(here::here("data", "details", "census_clean_tax.rds"))
summary(census_clean_tax) # 1951-2020

gst_rates <- readRDS(here::here("data", "gst_rates.rds"))
summary(gst_rates) # 1941-2021

iit_rates <- readRDS(here::here("data", "iit_rates.rds"))
summary(iit_rates) # 1977-2018

gstiitadj1 <- census_clean_tax %>%
  filter(name %in% c("iit", "gst")) %>%
  left_join(gst_rates %>%
              mutate(name="gst") %>%
              rename(gstrate=value),
            by = c("stabbr", "name", "year")) %>%
  left_join(iit_rates %>%
              mutate(name="iit") %>%
              select(-srate_gains),
            by = c("stabbr", "name", "year")) %>%
  mutate(rate=ifelse(name=="iit", iit_toprate,
                     gstrate)) %>%
  filter(!is.na(rate)) %>%
  group_by(stabbr, name) %>%
  mutate(valueadj=value * rate[year==max(year)] / rate) %>%
  ungroup


summary(gstiitadj1)
gstiitadj1 %>%
  filter(stabbr=="NY", name=="iit") %>%
  arrange(name, year)

# traditional non pit states
nonpit <- c("AK", "FL", "NV", "NH", "TN", "SD", "TX", "WA", "WY")
gstiitadj <- gstiitadj1 %>%
  mutate(name=case_when(name=="iit" ~ "iitadj",
                        name=="gst" ~ "gstadj",
                        TRUE ~ "ERROR")) %>%
  select(stabbr, name, year, value=valueadj) %>%
  filter(!is.na(value)) %>%
  filter(!(name=="iitadj" & stabbr %in% nonpit))
count(gstiitadj, name)
summary(gstiitadj)
count(gstiitadj, name, stabbr) %>%
  pivot_wider(values_from = n)

saveRDS(gstiitadj, here::here("data", "details", "census_gstiitadj.rds"))
