
install.packages("gitcreds")
library(gitcreds)
gitcreds_set()


library(tidyr)
library(dplyr)
library(repurrrsive)


d1 <- pewtax %>%
  # filter(stabbr=="NY", taxtype=="tottax", vartype=="raw") %>%
  # filter(stabbr=="NY", taxtype=="tottax") %>%
  filter(taxtype=="tottax", vartype != "policy") %>%
  left_join(sgdpfy %>%
              filter(name=="gdp") %>%
              rename(gdp=value) %>%
              select(year, stabbr, gdp),
            by=c("stabbr", "year")) %>%
  arrange(stabbr, taxtype, vartype, year) %>% 
  group_by(stabbr, taxtype, vartype) %>%
  mutate(pch=pch * 100,
         dlvalue=dl(level),
         dlgdp=dl(gdp)) %>%
  ungroup %>%
  group_nest(stabbr, taxtype, vartype) %>%
  mutate(hpf=map2(data, "level", safely(hpdf)),
         sre=map(data, safely(sredf)))

d2 <- d1 %>%
  hoist(data,
        year="year",
        level="level") %>%
  select(-data) %>%
  hoist(hpf, trend=c("result", "trend")) %>%
  select(-hpf) %>%
  unnest(cols=c(year, level, trend))

d2


users <- tibble(user = gh_users)  
# tibble, 1 column named user that is list column, 6 rows each a named list of length 30; each of 30 is atomic
str(users)
# tibble [6 × 1] (S3: tbl_df/tbl/data.frame)
# $ user:List of 6
# ..$ :List of 30
# .. ..$ login              : chr "gaborcsardi"
# .. ..$ id                 : int 660288

glimpse(users)

names(users$user[[1]])

# unpack each of 30 elements
users %>% 
  unnest_wider(user)
# tibble 6 rows 30 columns

# get selected elements
users %>% 
  hoist(user,
        followers = "followers",
        login = "login",
        url = "html_url")

repos <- tibble(repo = gh_repos)
repos


check2$data
check2 # 1 row
check2 %>%
  unnest_longer(data)

check2 %>%
  unnest_wider(data) %>%
  select(stabbr, taxtype, vartype, year, level, hpf) %>%
  unnest_auto(hpf)
  unnest(cols=c(year, level))


djb2 <- check2 %>%
  hoist(data,
        year="year",
        level="level")
  
  
  # unnest(cols=c("year", "level")) %>%
  # select(-data, -sre)

djb3 <- djb2 %>%
  unnest_wider(hpf) %>%  # wider
  unnest_wider(result) %>%
  unnest(cols=c(year, level, trend))
  
glimpse(djb3)
djb3$result

djb4 <- djb3 %>% 
  unnest_wider(result)


djb3 %>% 
  hoist(trend = c("result)

djb4 %>%
  unnest_longer(trend[[1]])

djb2$hpf[[1]]

tmp <- df2 %>%
  # select(-sre) %>%
  unnest_wider(hpf) %>%
  unnest_wider(result) %>%
  unnest(c(data, trend)) %>%
  select(stabbr, taxtype, vartype, year, value, pch, trend, sre)
tmp
