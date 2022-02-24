

## State sales tax bases
# Table 2.4.5U. Personal Consumption Expenditures by Type of Product
tpcfn <- "TPC State Sales Tax Mapping.xlsx"
tpc_namerows <- read_excel(here::here("raw_data", "tpc", tpcfn), sheet="Tax treatment by PCE", range="E3:BD5")
tpc_namerows
colnames <- tpc_namerows %>%
  filter(row_number()==1) %>%
  select(Alabama:Wyoming) %>% 
  unlist(., use.names=FALSE)
colnames

tpcdf <- read_excel(here::here("raw_data", "tpc", tpcfn), sheet="Tax treatment by PCE", range="A11:BD117",
                    col_names=c("line", "level", "pcename",  "tpc_id", "tpc_category", colnames))
tpcdf
glimpse(tpcdf)

stmap <- tpcdf %>%
  mutate(line=as.integer(line)) %>%
  pivot_longer(all_of(colnames), names_to = "stabbr", values_to = "taxstatus") %>%
  filter(!is.na(taxstatus))
count(stmap, line)

stmap %>% filter(stabbr=="NY")

# we need a mapping between the tpc line numbers and the new bea line numbers
remapfn <- "TPC_NIPA_remapped.xlsx"
remap <- read_excel(here::here("raw_data", "tpc", remapfn), sheet="remap", range="A7:J116",
                    col_names=TRUE)
glimpse(remap)
tail(remap)


glimpse(nipa)
data(package="BEAData")
NIPAvars %>%
  filter(tabnum=="2.4.5U")

tabpce <- NIPAvars %>%
  filter(tabnum=="2.4.5U") %>%
  select(tabnum, tabname, vname, line, vdesc2=vdesc) %>%
  left_join(nipa %>% filter(freq=="A"), by="vname") %>%
  # put total pce on every record
  group_by(year) %>%
  mutate(pce=value[line==1]) %>%
  ungroup

tabpce %>% 
  filter(line %in% 1:2, year %in% c(1990, 2020)) %>% 
  arrange(year, line)
# tabpce %>% filter(line==190) %>%
#   select(vname, vdesc, vdesc2) %>%
#   distinct

gstbase1 <- stmap %>%
  rename(line_tpc=line) %>% 
  left_join(remap %>% select(line=line_new_numeric, line_tpc=line_tpc_numeric), by="line_tpc") %>%  # line is bea's new line number
  left_join(tabpce, by="line")
glimpse(gstbase1)

check <- gstbase1 %>%
  select(line, line_tpc, vdesc, pcename, tpc_category) %>%
  distinct()

# save and then create final sales tax base
saveRDS(gstbase1, here::here("data", "details", "gstbase_details.rds"))



gstbase_details <- readRDS(here::here("data", "details", "gstbase_details.rds"))
count(gstbase_details, taxstatus)

gstbase <- gstbase_details %>%
  mutate(gststatus=ifelse(taxstatus %in% c("T", "TX"),
                          "taxable", 
                          "untaxed")) %>%
  select(stabbr, year, gststatus, value, pce) %>%
  group_by(stabbr, year, gststatus, pce) %>%
  summarise(value=sum(value), .groups = "drop") %>%
  pivot_wider(names_from = gststatus, values_fill = 0) %>%
  mutate(pct_potential=taxable / (taxable + untaxed) * 100,
         pct_pce=taxable / pce * 100,
         name="gstbase") %>% 
  # realnom="nominal") %>%
  select(stabbr, year, name, value=taxable, pct_potential, pct_pce)
summary(gstbase)

# some quick exploration and checks
sts <- c("CA", "NY", "AZ", "FL")
gstbase %>%
  filter(year >= 1990, stabbr %in% sts) %>%
  ggplot(aes(year, pct_pce, colour=stabbr)) +
  geom_line() +
  geom_point()

# interesting that the taxbase volatility is only slightly related to the taxable percent
# gstbase %>%
#   filter(year >= 1989) %>%
#   group_by(stabbr) %>%
#   arrange(year) %>%
#   mutate(pch=pchya(value, year)) %>%
#   summarise(pchsd=sd(pch, na.rm=TRUE), pct_pce=median(pct_pce, na.rm=TRUE), .groups="drop") %>%
#   filter(pct_pce > 0) %>%
#   # filter(!stabbr %in% c("HI")) %>%
#   ggplot(aes(x=pct_pce, y=pchsd, label=stabbr)) +
#   geom_point(colour="blue", size=0.5) +
#   geom_text(colour="blue", size=2) +
#   geom_smooth(method = "lm") +
#   theme_bw()

saveRDS(gstbase, here::here("data", "details", "gstbase.rds"))  

