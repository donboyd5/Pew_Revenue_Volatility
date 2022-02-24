
# regimes

keeptax <- readRDS(here::here("data", "details", "census_keeptax.rds"))  
test <- keeptax %>%
  filter(name=="sevtax")

test %>%
  ggplot(aes(year, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  facet_wrap(~stabbr, scales="free", ncol=4)

test2 <- test %>% filter(stabbr=="IN")

cp <- cpt.mean(test2$value, penalty="SIC", method="AMOC", class=FALSE)
str(cp)

cpf <- function(x) cpt.mean(x, penalty="SIC", method="AMOC", class=FALSE)[1]
test2$year[cpf(test2$value)]

library(strucchange)
breakpoints(test2$value)
mod <- breakpoints(value ~ year, data = test2, h = 0.1)
str(mod)
mod$breakpoints

# https://lindeloev.github.io/mcp/articles/packages.html
library(mcp)
# model = list(y~1, 1~1, 1~1)  # three intercept-only segments
model = list(value ~ 1, 1 ~ 1)
fit <- mcp(model, data = test2, par_x = "year")
summary(fit)
str(fit)
names(fit)
names(fit$model)
fit$mcmc_post[[1]]

test2 %>%
  ggplot(aes(year, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_vline(xintercept = 1979)

test2 %>%
  ggplot(aes(year, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_vline(aes(xintercept = year[cp[1]]))

test2 %>%
  ggplot(aes(year, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  geom_vline(xintercept = test2$year[mod$breakpoints])


test3 <- test %>%
  arrange(stabbr, name, year) %>%
  na.omit() %>%
  group_by(stabbr, name) %>%
  mutate(cp=cpf(value),
         cpy=year[cp]) %>%
  ungroup
count(test3, cpy)

test3 %>%
  ggplot(aes(year, value)) +
  geom_line(colour="blue") +
  geom_point(colour="blue") +
  facet_wrap(~stabbr, scales="free", ncol=4) +
  geom_vline(aes(xintercept = cpy)) 

 cpt.mean(test2$value, 
         test.stat="Normal", 
         method = "PELT", 
         penalty = "Manual", 
         pen.value = 10) 
