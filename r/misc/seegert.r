
# produce a plot similar to Figure 1 in his NTJ paper

glimpse(stack)
count(stack, name)

df <- stack %>%
  filter(stabbr=="US", realnom=="nominal", year %in% 1970:2013, name %in% c("gdp", "tottax")) %>%
  mutate(lvalue=log(value)) %>%
  select(-value) %>%
  pivot_wider(values_from = lvalue) %>%
  mutate(trend=row_number())

# values here are in logs....
tpmod <- lm(tottax ~ poly(trend, 7, raw = TRUE), data = df)
gpmod <- lm(gdp ~ poly(trend, 7, raw = TRUE), data = df)

df2 <- df %>%
  mutate(tresids=residuals(tpmod),
         gresids=residuals(gpmod))

df3 <- df2 %>%
  pivot_longer(-c(stabbr, realnom, year, trend))

df3 %>%
  filter(name %in% c("tresids", "gresids")) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks=seq(-.2, .2, .025))

# repeat, but subtract iit and cit ----
df <- stack %>%
  filter(stabbr=="US", realnom=="nominal", year %in% 1970:2013, name %in% c("gdp", "tottax", "iit", "cit")) %>%
  pivot_wider() %>%
  mutate(tottax2=tottax - iit - cit,
         inctaxes=iit + cit,
         across(-c(stabbr, realnom, year), log)) %>%
  mutate(trend=row_number())

txpmod <- lm(tottax2 ~ poly(trend, 7, raw = TRUE), data = df)
tincpmod <- lm(inctaxes ~ poly(trend, 7, raw = TRUE), data = df)
gpmod <- lm(gdp ~ poly(trend, 7, raw = TRUE), data = df)

df2 <- df %>%
  mutate(txresids=residuals(txpmod),
         tincresids=residuals(tincpmod),
         gresids=residuals(gpmod))

df3 <- df2 %>%
  pivot_longer(-c(stabbr, realnom, year, trend))

df3 %>%
  filter(name %in% c("txresids", "tincresids", "gresids")) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks=seq(-.2, .2, .025))


# repeat, with HP filter, subtract iit and cit ----
hpseeg <- stack %>%
  filter(stabbr=="US", realnom=="nominal", name %in% c("gdp", "tottax", "iit", "cit")) %>%
  filter(year %in% 1970:2020) %>%
  pivot_wider() %>%
  mutate(xinctax=tottax - iit - cit,
         inctax=iit + cit,
         gdptrend=hptrend(gdp, smooth=6.25),
         xinctrend=hptrend(xinctax, smooth=6.25),
         inctrend=hptrend(inctax, smooth=6.25),
         
         gdppd=gdp / gdptrend - 1,
         xincpd=xinctax / xinctrend - 1,
         incpd=inctax / inctrend - 1
         )

hpseeg2 <- hpseeg %>%
  select(year, ends_with("pd")) %>%
  pivot_longer(-year)

hpseeg2 %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks=seq(-.2, .2, .025))


