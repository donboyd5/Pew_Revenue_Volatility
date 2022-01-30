
# cspp
all_variables <- get_var_info()
all_variables %>%
  filter(str_detect(long_desc, pattern=coll("tax", ignore_case=TRUE))) %>%
  filter(str_detect(long_desc, pattern=coll("rate", ignore_case=TRUE))) %>%
  select(variable, years, short_desc)

tmp <- all_variables %>% filter(variable=="x_tax_rate_rich")
tmp$sources
glimpse(tmp)
# What is the state individual income tax rate for an individual who makes more than 1.5 million real dollars?"
# 1977-2012


tmp <- all_variables %>% filter(variable=="x_sales_taxes")
tmp$sources
glimpse(tmp)

# x_sales_taxes 1946-2014
# x_tax_rate_rich       1977-2012

# Full dataset
all_data <- get_cspp_data()

df <- all_data %>%
  select(st.abb, year, x_sales_taxes)
summary(df)  # 1900-2019 w/2586 NAs
summary(df %>% filter(!is.na(x_sales_taxes)))  # 1946-2019

df %>% filter(st.abb=="NY")


df <- all_data %>%
  select(st.abb, year, x_tax_rate_rich)
summary(df) 
summary(df %>% filter(!is.na(x_tax_rate_rich)))  # 1946-2019
df %>% filter(st.abb=="NY")

df %>%
  filter(year %in% 1977:2012) %>%
  distinct %>%
  pivot_wider(names_from = year, values_from = x_tax_rate_rich)


# https://users.nber.org/~taxsim/state-rates/  1977-2018
# https://users.nber.org/~taxsim/state-rates/maxrate.dat  # 2nd column is top state rate
# https://users.nber.org/~taxsim/state-rates/maxrate.html  # looks like html version of same
# tpc https://www.taxpolicycenter.org/statistics/state-individual-income-tax-rates 2000-2021
