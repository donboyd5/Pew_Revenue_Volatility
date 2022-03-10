

library(portopt)

# no restrictions on asset allocation - shorting and leverage allowed
minvport(.09, stalebrink$ersd, stalebrink$cormat)$portfolio

glimpse(vbase)

df <- vbase %>%
  filter(year %in% 2000:2020, stabbr=="NY", 
         realnom=="nominal", name %in% taxnames$name)
count(df, name)
df

gm <- function(pch){
  lpch <- length(pch)
  cumprod(1 + pch)[lpch]^(1 / lpch) - 1
}

x <- c(.02, .05, .1, .09)
gm(x)
(1 + gm(x))^4


ersd <- df %>%
  group_by(name) %>%
  summarise(er=gm(pch),
            sd=sd(pch), .groups="drop")
ersd

cormat <- df %>%
  select(name, year, pch) %>%
  pivot_wider(values_from=pch) %>%
  select(-year, -tottax) %>%
  cor()

taxmat <- cor(xmat %>% select(-year, -tottax))


res <- minvport(.0425, 
         ersd %>% filter(name != "tottax"),
         taxmat)
res$portfolio
ersd

taxshares %>%
  filter(stabbr=="NY", year==2020)

st <- "NY"

port <- function(st, years=2000:2020){
  gm <- function(pch){
    lpch <- length(pch)
    cumprod(1 + pch)[lpch]^(1 / lpch) - 1
  }
  
  df <- vbase %>%
    filter(year %in% years, stabbr==st, 
           realnom=="nominal", name %in% taxnames$name)
  
  ersd <- df %>%
    group_by(name) %>%
    summarise(er=gm(pch),
              sd=sd(pch), .groups="drop")
  
  cormat <- df %>%
    select(name, year, pch) %>%
    pivot_wider(values_from=pch) %>%
    select(-year, -tottax) %>%
    cor()
  
  er_actual <- ersd %>%
    filter(name=="tottax") %>%
    pull(er)
  
  sd_actual <- ersd %>%
    filter(name=="tottax") %>%
    pull(sd)
  
  shares_actual <- taxshares %>%
    filter(stabbr==st, year==2020) %>% 
    select(-c(year, tottax)) %>%
    pivot_longer(-stabbr,
                 values_to="weight")
  
  portres <- minvport(er_actual, 
                  ersd %>% filter(name != "tottax"),
                  cormat)
  
  output <- portres$portfolio %>%
    left_join(shares_actual) %>%
    mutate(sd_actual=!!sd_actual,
           sdiff=psd - sd_actual,
           wdiff=asset.weight - weight) %>%
    select(stabbr, name, er, sd, sd_actual, psd, sdiff, weight, asset.weight, wdiff, per)
  
  res <- list()
  res$output <- output
  res$cormat <- cormat
  # res$ersd <- ersd
  res
}

port("VA")
port("VT")
port("MA")
port("NY")
port("TX")
port("FL")
port("CA")
port("NJ")

# Why is illinois revenue so volatile

vbase

df <- vbase %>%
  filter(realnom=="nominal",
         year %in% 2000:2020,
         name %in% taxnames$name,
         stabbr %in% c("IL", "US"))





vbase %>%
  filter(realnom=="nominal",
         year %in% 2000:2020,
         name %in% c(taxnames$name, "gdp"),
         stabbr %in% c("AR", "US")) %>%
  ggplot(aes(year, pch, colour=stabbr)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~name, scales="free", ncol=2)


hline <- vbase %>%
  filter(realnom=="nominal",
         year %in% 2000:2020,
         name %in% c(taxnames$name, "gdp"),
         stabbr %in% c("IL", "US")) %>%
  filter(stabbr=="IL") %>%
  group_by(name) %>%
  summarise(yint=mean(pch, na.rm=TRUE))
hline

vbase %>%
  filter(realnom=="nominal",
         year %in% 2000:2020,
         name %in% c(taxnames$name, "gdp"),
         stabbr %in% c("IL", "US")) %>%
  ggplot(aes(year, pch, colour=stabbr)) +
  geom_point() +
  geom_line() +
  # scale_y_continuous(breaks=seq(-1, 1, .02)) +
  facet_wrap(~name, scales="free", ncol=2) +
  geom_hline(aes(yintercept = yint), data=hline %>% mutate(yint=ifelse(name=="gdp", .15, yint))) 
  # geom_hline(aes(yintercept = mean(pch, na.rm=TRUE)), data=. %>% filter(TRUE)) 

# geom_text(
#   data=pdata %>%
#     filter(year==1987, name=="ldcgagi"),
#   mapping=aes(x=year, y=value + .025),
#   label="1986 tax\nreform impact", 
#   hjust=0.5,
#   colour="darkgreen",
#   size=2,
#   inherit.aes = FALSE, show.legend = FALSE) 

count(volall, name)
glimpse(volall)

volall %>%
  filter(period %in% decades,
         stabbr=="US",
         name=="tottax") %>%
  ggplot(aes(period, pdtrendiqr, colour=realnom)) +
  geom_line() +
  geom_point()

volall %>%
  filter(period %in% decades,
         stabbr=="US",
         name != "gdp") %>%
  select(name, period, realnom, pdtrendiqr) %>%
  pivot_wider(names_from = realnom, values_from = pdtrendiqr) %>%
  group_by(period) %>%
  mutate(nrank=rank(nominal), rrank=rank(real), d=rrank - nrank) %>%
  select(period, name, nrank, rrank, d, nominal, real) %>%
  arrange(period, nrank)

volall %>%
  filter(period %in% decades,
         stabbr!="US",
         realnom=="nominal",
         name != "gdp") %>%
  select(stabbr, name, period, pdtrendiqr, pchiqr) %>%
  group_by(name, period) %>%
  summarise(cor=cor(pdtrendiqr, pchiqr, use = "pairwise.complete.obs"))
  

# how does the actual portfolio volatility compare to the weighted volatility?
count(volall, name)
count(volall, period)

df <- volall %>%
  filter(period=="2000-2020",
         stabbr!="US",
         realnom=="nominal",
         name %in% taxnames$name)

df2 <- df %>%
  select(stabbr, name, pdtrendiqr) %>%
  left_join(taxshares %>%
              filter(year==2010) %>%
              select(-c(year, tottax)) %>%
              pivot_longer(-stabbr, values_to="share"),
            by=c("stabbr", "name")) %>%
  mutate(share=ifelse(name=="tottax", 1, share))

df3 <- df2 %>%
  filter(name != "tottax") %>%
  group_by(stabbr) %>%
  summarise(wpdtrendiqr=sum(pdtrendiqr * share))

comp <- df2 %>%
  filter(name=="tottax") %>%
  left_join(df3, by = "stabbr") %>%
  mutate(diff=wpdtrendiqr - pdtrendiqr)

df4 <- df2 %>%
  select(-share) %>%
  pivot_wider(values_from = pdtrendiqr) %>%
  arrange(-tottax) %>%
  left_join(df3, by="stabbr") %>%
  select(stabbr, tottax, wpdtrendiqr, everything())
df4

port("NM")

volall %>%
  filter(name=="tottax", realnom=="nominal") %>%
  select(stabbr, name, period, sre, pdtrendiqr, pchiqr) %>%
  group_by(period) %>%
  mutate(rsre=rank(sre),
         rpdtrend=rank(pdtrendiqr), 
         rpch=rank(pchiqr)) %>%
  ungroup %>%
  filter(stabbr=="US") %>%
  arrange(period)

volall %>%
  filter(name=="tottax", realnom=="nominal")



# check
# us is ranked 16 on plot

# recalc for plot
df <- vbase %>%
  filter(year %in% 2000:2020, realnom=="nominal", name=="tottax") %>%
  group_by(stabbr) %>%
  summarise(pdt=IQR(pdtrend, na.rm=TRUE)) %>%
  arrange(desc(pdt))
df  # yes, US is 16

# now go to volall
volall %>%
  filter(period=="2000-2020",
         realnom=="nominal",
         name=="tottax") %>%
  select(stabbr, pdtrendiqr) %>%
  arrange(desc(pdtrendiqr)) # US is 16, good (I guess)

censustax


df <- volall %>%
  filter(period=="2000-2020",
         realnom=="nominal",
         name=="tottax") %>%
  select(stabbr, pdtrendiqr) %>%
  left_join(censustax %>%
              filter(year==2020, name=="tottax") %>%
              select(stabbr, tax=value),
            by="stabbr") %>%
  mutate(ltax=log(tax))

df %>%
  filter(stabbr != "AK") %>%
  ggplot(aes(ltax, pdtrendiqr)) +
  geom_point(size=1, colour="blue") +
  geom_text(aes(label=stabbr), size=2.5, hjust=0, colour="blue") +
  geom_hline(yintercept = df %>% filter(stabbr=="US") %>% pull(pdtrendiqr))




df <- volall %>%
  filter(realnom=="nominal",
         name=="tottax") %>%
  select(stabbr, period, year2, pdtrendiqr) %>%
  left_join(censustax %>%
              filter(name=="tottax") %>%
              select(stabbr, year2=year, tax=value),
            by=c("stabbr", "year2")) %>%
  mutate(ltax=log(tax))

df %>%
  filter(stabbr != "AK") %>%
  ggplot(aes(ltax, pdtrendiqr)) +
  geom_point(size=1, colour="blue") +
  geom_text(aes(label=stabbr), size=2.5, hjust=0, colour="blue") +
  facet_wrap(~period, scales="free", ncol=3)
# +  geom_hline(yintercept = df %>% filter(stabbr=="US") %>% pull(pdtrendiqr)) +
  



vbase %>%
  filter(year %in% 2000:2020, realnom=="nominal", name %in% taxnames$name,
         name != "tottax") %>%
  select(stabbr, name, year, pdtrend) %>%
  pivot_wider(values_from = pdtrend) %>%
  group_by(stabbr) %>%
  summarise(cor(. %>% select(-year)))


