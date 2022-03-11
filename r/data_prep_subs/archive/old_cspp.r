
# explore selected data ---------------------------------------------------
# not sure I'm going to use this
# http://ippsr.msu.edu/public-policy/correlates-state-policy
# x_sales_taxes
# cspp_june_2021.csv
fn <- "cspp_june_2021.csv"

df <- read_csv(file.path(scratchdir, fn))
df2 <- df %>%
  select(year:state_icpsr, x_sales_taxes)
df2 %>% tail(20)
df2 %>%
  filter(!is.na(x_sales_taxes)) %>%
  count(year) # ~1946-2014, 2019 (???)
# Caughey, Devin, and Christopher Warshaw. 2015. “The Dynamics of State
# Policy Liberalism, 1936–2014.” American Journal of Political Science,
# September. doi: 10.1111/ajps.12219.
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZXZMJB
# For 2019: “State Sales Tax Rates” (provided by the Sales Tax Institute)
# https://www.salestaxinstitute.com/resources/rates
