
# libraries ----
library(fredr)

# API keys ----
bea_apikey <- "21F782AD-56A6-439D-B3D5-9A592F020E26"
bls_apikey <- "e1a32c87a90f4d889f5342174e275470"
brbls_apikey <- "2ec4ce7e7f5b4934a8477539715adbae"
census_apikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
fred_apikey <- "a5e1199baac333154cbffcba3b263c28"

fredr_set_key(fred_apikey)
# 1963-2013
# change 2017 2013

# functions ----
fpoly <- function(value, degree) {
  time <- 1:length(value)
  lm(value ~ poly(time, degree))$fitted.values
}

fpoly2 <- function(value, degree) {
  time <- 1:length(value)
  lm(value ~ poly(time, degree))$residuals
}

hptrend <- function(vec, smooth=NULL){
  vts_hp <- hpfilter(ts(vec), freq=smooth, type=c("lambda"), drift=FALSE)
  as.numeric(vts_hp$trend)
}

fpoly2 <- function(value) {
  time <- 1:length(value)
  lm(value ~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5))$fitted.values
}


time(1:10)
time(20:30)
time(c(10.12, 20, 30.45, 900.6))

# analysis ----

df <- taxkeep %>%
  filter(stabbr=="US", name=="tottax", year >= 1970) %>%
  select(year, value) %>%
  arrange(year) %>%
  mutate(time=row_number(),
         pch=value / lag(value) * 100 - 100,
         hptrend6p25=hptrend(value, smooth=6.25),
         hptrend100=hptrend(value, smooth=100),
         ptrend5=fpoly(value, time, degree=5),
         trend=hptrend6p25,
         vol1=log(abs(value - trend)),
         vol2=(value - trend) / trend * 100)

yrscale <- scale_x_continuous(breaks=seq(1960, 2020, 2))

df %>%
  select(year, actual=value, hptrend6p25, hptrend100, ptrend5) %>%
  mutate(ygroup=ntile(year, 4)) %>%
  pivot_longer(-c(year, ygroup)) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ygroup, scales="free") +
  yrscale

df %>%
  mutate(across(c(hptrend6p25, hptrend100, ptrend5), ~ value - .)) %>%
  select(year, hptrend6p25, hptrend100, ptrend5) %>%
  mutate(ygroup=ntile(year, 4)) %>%
  pivot_longer(-c(year, ygroup)) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ygroup, scales="free") +
  yrscale

df %>%
  mutate(across(c(hptrend6p25, hptrend100, ptrend5), ~ (value - .) / value * 100)) %>%
  select(year, hptrend6p25, hptrend100, ptrend5) %>%
  mutate(ygroup=ntile(year, 4)) %>%
  pivot_longer(-c(year, ygroup)) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ygroup, scales="free") +
  yrscale



mod <- lm(value ~ poly(time, 5), data=df)
fitted(mod)
mod$fitted.values

df %>%
  filter(year %in% 1970:2013) %>%
  ggplot(aes(year, vol2)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = c(2007, 2013), linetype = "dashed") 


df <- taxkeep %>%
  filter(stabbr=="US", name=="tottax", year >= 1969) %>%
  select(year, value) %>%
  arrange(year) %>%
  mutate(pch=value / lag(value) * 100 - 100,
         trend=hptrend(value, smooth=6.25),
         vol1=log(abs(value - trend)),
         vol2=(value - trend) / trend * 100)

df %>%
  filter(year %in% 1970:2014) %>%
  mutate(gt2k=ifelse(year <= 2000, 0, 1),
         volsq=vol2 ^2) %>%
  group_by(gt2k) %>%
  summarise(vol=mean(volsq))


taxkeep %>%
  filter(stabbr=="US", name=="tottax", year >= 1969) %>%
  select(year, value) %>%
  arrange(year) %>%
  mutate(pch=value / lag(value) * 100 - 100,
         trend=hptrend(value, smooth=6.25),
         vol=(value - trend) / trend * 100,
         volsq=vol ^2,
         period=ifelse(year <= 2000, 0, 1)) %>%
  group_by(period) %>%
  summarise(vol=mean(volsq))


taxkeep %>%
  filter(stabbr!="US", name=="tottax", year >= 1969) %>%
  select(stabbr, year, value) %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate(pch=value / lag(value) * 100 - 100) %>%
  filter(year %in% 1970:2014) %>%
  mutate(trend=hptrend(value, smooth=6.25),
         vol=(value - trend) / trend * 100,
         volsq=vol^2,
         pchsq=pch^2,
         period=case_when(year <= 2000 ~ "first",
                          TRUE ~ "second")) %>%
  group_by(period, stabbr) %>%
  summarise(pch=mean(pchsq),
            vol=mean(volsq)) %>%
  pivot_longer(-c(stabbr, period)) %>%
  pivot_wider(names_from = period) %>%
  mutate(change=second - first) %>%
  arrange(name, stabbr) %>%
  ungroup %>%
  filter(name=="vol")


regdat <- taxkeep %>%
  filter(stabbr!="US", name %in% c("iit", "cit", "gst"), year >= 1970) %>%
  group_by(stabbr,  year) %>%
  summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate(pch=value / lag(value) * 100 - 100,
         apch=abs(pch),
         sqpch=pch^2,
         period=case_when(year <= 2000 ~ "first",
                          TRUE ~ "second"),
         ydum=ifelse(year > 1999, 1, 0) %>% as.factor()) %>%
  filter(year %in% 1971:2014) %>%
  ungroup()



mod1 <- lm(pch ~ stabbr + stabbr:ydum - 1, data=regdat)
summary(mod1)


mod2 <- lm(apch ~ stabbr + stabbr:ydum -1, data=regdat)
summary(mod2)

mod3 <- lm(sqpch ~ stabbr + stabbr:ydum -1, data=regdat)
summary(mod3)

regdat %>%
  group_by(stabbr, period) %>%
  summarise(apch=mean(apch)) %>%
  pivot_wider(names_from=period, values_from = apch) %>%
  mutate(change=second - first)


# check data vs Seegert
# he says in 2000 income, sales, and corporate tax revenues accounted for roughly
# 75% of state government revenues
taxkeep %>%
  filter(stabbr=="US", year == 2000) %>%
  pivot_wider() %>%
  mutate(big3=iit + gst + cit) %>%
  pivot_longer(cols=-c(stabbr, year)) %>%
  mutate(pct=value / value[name=="tottax"] * 100)
# looks good


# Seegert Figure A.1 2020 paper
# adjust for inflation??
df <- taxkeep %>%
  filter(stabbr=="US", year %in% 1970:2014) %>%
  pivot_wider() %>%
  mutate(big3=iit + gst + cit) %>%
  left_join(gdppi %>% select(year, igdppi), by="year") %>%
  mutate(rbig3=big3 * igdppi) %>%
  select(year, value=rbig3) %>%
  arrange(year) %>%
  mutate(time=row_number(),
         lvalue=value[match(year - 1, year)],
         pch=value / lvalue * 100 - 100,
         hptrend6p25=hptrend(value, smooth=6.25),
         hptrend100=hptrend(value, smooth=100),
         ptrend5=fpoly(value, time, degree=5)) %>%
  mutate(vyoy=abs(pch),
         vsqr=abs(ptrend5 - value) / value * 100)

df %>%
  filter(year %in% 1970:2014) %>%
  select(year, vyoy, vsqr) %>%
  pivot_longer(-year) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line()

df %>%
  filter(year >= 1990) %>%
  select(year, vyoy, vsqr) %>%
  pivot_longer(-year) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line()

# Fig A.1 can I reproduce?? ----
a1 <- taxkeep %>%
  filter(stabbr=="US", year %in% 1970:2014, name %in% c("tottax", "iit", "gst", "cit")) %>%
  pivot_wider() %>%
  mutate(big3=iit + gst + cit) %>%
  pivot_longer(cols=-c(stabbr, year))
a1 %>% filter(name=="tottax", year %in% c(1970, 1980, 1990, 2000, 2010))

# FRED https://fred.stlouisfed.org/series/USTOTLTAX
# USTOTLTAX USINCTAX USCORPINCTX USSALESTAX
vars <- c("USTOTLTAX", "USINCTAX", "USCORPINCTX", "USSALESTAX")
vnames <- c("tottax", "iit", "cit", "gst")

price <- fredr("A191RG3A086NBEA", frequency = "a")
price2 <- price %>%
  mutate(year=year(date),
         igdppi=value[year==2019] / value) %>%
  select(year, series_id, gdppi=value, igdppi)
ht(price2)
ht(gdppi)

check1 <- map_dfr(vars, fredr)
check2 <- check1 %>%
  mutate(year=year(date),
         name=factor(series_id, levels=vars, labels = vnames)) %>%
  filter(!is.na(value), year >= 1949) %>%
  select(year, name, value) %>%
  pivot_wider() %>%
  left_join(price2 %>% select(year, gdppi, igdppi), by="year") %>%
  mutate(big3=iit + cit + gst,
         rbig3=big3 * igdppi) %>%
  select(-igdppi) %>%
  select(year, gdppi, everything()) %>%
  arrange(year)

check2 %>%
  mutate(across(-c(year), ~ . / lag(.) * 100 - 100))


check2 %>%
  select(year, value=tottax) %>%
  arrange(year) %>%
  mutate(lvalue=value[match(year - 1, year)],
         pch=(value - lvalue) / lvalue * 100,
         apch=abs(pch),
         v4=rollsd(pch, 4),
         v8=rollsd(pch, 8),
         v10=rollsd(pch, 10),
         v4pct=rollsd(value, 4) / value * 100,
         v8pct=rollsd(value, 8) / value * 100) %>%
  ggplot(aes(year, v10)) +
  geom_line(colour="blue") +
  geom_point(colour="blue", size=0.5) +
  ggtitle("Sum of individual income, general sales, and corporate income taxes from FRED,
          inflation-adjusted with chain-weighted GDP price index",
          subtitle="Absolute value of percent change") +
  theme_bw()

check2 %>%
  select(year, value=tottax) %>%
  arrange(year) %>%
  mutate(lvalue=value[match(year - 1, year)],
         pch=(value - lvalue) / lvalue * 100,
         apch=abs(pch)) %>%
  ggplot(aes(year, pch)) +
  geom_line(colour="blue") +
  geom_point(colour="blue", size=0.5) +
  ggtitle("Total taxes",
          subtitle="percent change") +
  theme_bw()



check %>%
  select(year, value) %>%
  arrange(year) %>%
  mutate(pch=(value - lag(value)) / value * 100,
         apch=abs(pch)) %>%
  ggplot(aes(year, apch)) +
  geom_line() +
  geom_point()


check %>%
  select(year, value) %>%
  arrange(year) %>%
  mutate(pch=(value - lag(value)) / value * 100,
         apch=abs(pch)) %>%
  ggplot(aes(year, apch)) +
  geom_line() +
  geom_point()

#.. could it be that he used national averages not sums?? ----
a1a <- taxkeep %>%
  filter(stabbr!="US", year %in% 1970:2014, name %in% c("tottax", "iit", "gst", "cit")) %>%
  pivot_wider() %>%
  mutate(big3=rowSums(across(c(iit, gst, cit)), na.rm=TRUE)) %>%
  select(stabbr, year, big3) %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate(pch=big3 / big3[match(year - 1, year)] * 100 - 100) %>%
  group_by(year) %>%
  summarise(pch=mean(pch))
a1a

# figure 1 ----
fig1 <- taxkeep %>%
  filter(stabbr=="US", year %in% 1970:2014) %>%
  pivot_wider() %>%
  mutate(big3=iit + gst + cit) %>%
  select(year, big3) %>%
  left_join(gdpfy, by="year") %>%
  pivot_longer(-year) %>%
  group_by(name) %>%
  arrange(year) %>%
  mutate(ptrend=fpoly(value, 5),
         htrend=hptrend(value, smooth=100),
         trend=ptrend,
         detrend=(value - trend) /  value * 100)

fig1 %>%
  filter(year %in% 1970:2014) %>%
  ggplot(aes(year, detrend, colour=name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0)

var <- "gdp"  # gdp big3
fig1 %>%
  filter(name==var) %>%
  mutate(pch=value / lag(value) * 100 - 100)

fig1 %>%
  filter(name==var) %>%
  ungroup %>%
  select(year, value, trend) %>%
  pivot_longer(-year, names_to = "measure") %>%
  ggplot(aes(year, value, colour=measure)) +
  geom_line() +
  geom_point() +
  ggtitle(var)

taxcomp <- taxkeep %>%
  filter(stabbr=="US", year %in% 1970:2014) %>%
  pivot_wider() %>%
  mutate(big3=iit + gst + cit) %>%
  select(year, iit, gst, cit, big3) %>%
  pivot_longer(-year) %>%
  group_by(name) %>%
  arrange(year) %>%
  mutate(ptrend=fpoly(value, 5),
         htrend=hptrend(value, smooth=100),
         htrend65=hptrend(value, smooth=6.5),
         trend=htrend65,
         detrend=(value - trend) /  trend * 100)

taxcomp %>%
  filter(year %in% 1970:2014) %>%
  ggplot(aes(year, detrend, colour=name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0)


taxcomp %>%
  filter(year %in% 1971:2014) %>%
  filter(name %in% c("iit")) %>%
  ggplot(aes(year, detrend, colour=name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0)


# Table 1 - can I reproduce? ----
t1 <- taxkeep %>%
  filter(stabbr!="US", name %in% c("iit", "cit", "gst"), year %in% 1970:2014) %>%
  group_by(stabbr,  year) %>%
  summarise(value=sum(value, na.rm=TRUE), .groups="drop") %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate(pch=value / lag(value) * 100 - 100,
         apch=abs(pch),
         period=case_when(year <= 2000 ~ "first",
                          TRUE ~ "second"),
         ydum=ifelse(year > 1999, 1, 0) %>% as.factor()) %>%
  filter(year %in% 1971:2014) %>%
  ungroup()

mod <- lm(apch ~ stabbr + stabbr:ydum -1, data=t1)
summary(mod)

t1 %>%
  group_by(stabbr, period) %>%
  summarise(apch=mean(apch)) %>%
  pivot_wider(names_from=period, values_from = apch) %>%
  mutate(change=second - first)



