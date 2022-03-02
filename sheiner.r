
source(here::here("r", "libs_base.r"))
source(here::here("r", "libs_ts.r"))
devtools::session_info()

data(package="BLSdata")

df <- eesm_m
summary(df)
count(df, dtype, dtypef)
count(df, ssector, ssectorf)
# count(df, area, areaf)


df2 <- df %>%
  filter(year(date)>=2000, 
         stabbr %in% state.abb, 
         dtype=="01", 
         ssector %in% c("05", "90"),
         area=="00000",
         date < "2021-12-01") %>%
  filter(!(ind=="90000000" | str_sub(ind, 1, 4)=="9091")) %>%
  select(-c(dtype, dtypef, area, areaf, stcode, ssector, ssectorf))
summary(df2)

count(df2, ind, indf)
count(df2, ind, indf, season) %>%
  pivot_wider(names_from = season, values_from = n)
# SA only for totpriv, sg, lg

df3 <- df2 %>%
  filter(season=="U") %>%
  select(-season) %>%
  arrange(stabbr, ind, indf, date) %>%
  group_by(stabbr, ind, indf) %>%
  mutate(stldf(value, 12),
         saloess=trend + remainder)

df4 <- df2 %>%
  bind_rows(df3 %>% 
              select(-value) %>%
              pivot_longer(c(trend, seasonal, remainder, saloess), names_to = "season"))
count(df4, season)
count(df4, ind, indf)
vnames <- read_csv(
"ind, vname
05000000, private
90920000, sg
90921611, sged
90922000, sgxed
90922622, sghosp
90930000, lg
90931611, lged
90932000, lgxed
90932622, lghosp
90936111, lgesed
")
vnames

df5 <- df4 %>%
  filter(season %in% c("S", "U", "trend", "saloess")) %>%
  mutate(vname=factor(ind, levels = vnames$ind, labels=vnames$vname)) %>%
  select(-ind, -indf)
# count(df5, ind, indf, vname)

df5 %>%
  filter(stabbr=="CA", vname=="private", year(date) >= 2015) %>%
  ggplot(aes(date, value, colour=season)) +
  geom_line() +
  geom_point()

df5%>%
  filter(vname=="private") %>%
  pivot_wider(names_from = season) %>%
  tail
  
  
df5 %>%
  filter(stabbr=="CA",
         vname %in% c("private", "sged", "sgxed", "lged", "lgxed"),
         season=="saloess",
         year(date) >= 2017) %>%
  group_by(vname) %>%
  mutate(ivalue=value / value[date=="2020-03-01"]) %>%
  ggplot(aes(date, ivalue, colour=vname)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1)


sts <- 1:12
sts <- 13:24
sts <- 25:36
sts <- 37:48
sts <- 49:50

df5 %>%
  filter(stabbr %in% state.abb[sts],
         # vname %in% c("private", "sged", "sgxed", "lged", "lgxed"),
         vname %in% c("private", "lged", "lgxed"),
         season=="saloess",
         year(date) >= 2017) %>%
  group_by(stabbr, vname) %>%
  mutate(ivalue=value / value[date=="2020-03-01"]) %>%
  ggplot(aes(date, ivalue, colour=vname)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1) +
  facet_wrap(~stname, ncol=3, scales="free")


sts <- 1:12
sts <- 13:24
sts <- 25:36
sts <- 37:48
sts <- 49:50
df5 %>%
  filter(stabbr %in% state.abb[sts],
         vname %in% c("lged"),
         season %in% c("S", "U", "saloess"),
         year(date) >= 2017) %>%
  group_by(stabbr, vname) %>%
  ggplot(aes(date, value, colour=season)) +
  geom_line() +
  geom_point() +
  facet_wrap(~stname, ncol=3, scales="free")

# make a map
# % change of SA loess from Mar 2020 to:
# May 2020, Oct 2020, Apr 2021, Nov 2021
bdate <- as.Date("2020-03-01")
dates <- as.Date(c("2019-03-01", "2020-03-01", "2020-05-01", "2020-10-01", "2021-04-01", "2021-11-01"))
mbase <- df5 %>%
  filter(stabbr %in% state.abb,
         vname %in% c("lged"),
         season %in% c("saloess"),
         date %in% dates) %>%
  select(date, stabbr, stname, value) %>%
  group_by(stabbr) %>%
  mutate(diff=value - value[date==bdate],
         pdiff=diff / value[date==bdate],
         datef=format(date, "%Y-%b")) %>%
  ungroup
count(mbase, date, datef)
setdiff(state.abb, unique(mbase$stabbr)) # HI, MO

mbase %>%
  filter(date > bdate) %>%
  group_by(datef) %>%
  summarise(qtiledf(pdiff))
# good breaks (?):
# -11%, -8.4%, -6.9%

brks <- c(-Inf, -.11, -.0844, -.0687, Inf)
brks <- c(-Inf, -.1, -.05, 0, Inf)

mbase <- mbase %>%
  mutate(pcut=cut(pdiff, brks))
count(mbase, pcut)

mdata <- states51 %>%
  left_join(mbase %>%
              filter(date > bdate), by="stabbr")
count(mdata, pcut)

# clrs <- c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
clrs <- c('#d73027', '#fc8d59', '#fee08b', '#1a9850')
m <- mdata %>%
  filter(!is.na(pcut)) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = pcut)) +
  geom_polygon(color = "black", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values=clrs) +
  # scale_fill_brewer(palette = "RdBu", na.translate = FALSE) + # , na.value="grey90") +
  # scale_fill_manual(breaks=as.integer(1:4), values=c("red", "green", "yellow", "blue")) +
  theme_map() + 
  theme(legend.position = "right") +
  labs(fill="% change group: ") +
  # legend_notitle +
  facet_wrap(~datef, ncol = 2) +
  ggtitle("% change loess-adjusted local govt education employment vs Mar 2020")
m
