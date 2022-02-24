## Pew data on enacted revenue changes

# -   Pew constructed estimates of the percent change in major taxes, by year, state, and major tax type for work it did in \_\_\_
# -   In a 10/8/2015 email, on my request Brenna Erford [BErford\@pewtrusts.org](mailto:BErford@pewtrusts.org){.email} provided the file "Boyd Data Request 100815.xlsx" to me ([donald.boyd\@rockinst.suny.edu](mailto:donald.boyd@rockinst.suny.edu){.email}), which had data underlying the project
# -   Pew's short description of the data, in the "Cover" tab of the workbook, was:
# 
# Volatility scores for each state's total tax revenue and specific tax sources were calculated using the U.S. Census Bureau's State Government Tax Collections historical data series for 1994 to 2014, accessed April 21, 2015. Data were adjusted to control for the effects of tax policy change using the National Conference of State Legislatures (NCSL) State Tax Action reports for 1994 to 2013, accessed in January 2015.
# Note that in these data, adjusted = raw - polichy They include separate tabs for total and for major. The majors are:
# `Major Revenue Source` n <chr> <int> 1 Amusements 9 2 Corporation 9 3 Corporation net income 90 4 Documentary and stock transfer 6 5 General sales and gross receipts 135 6 Individual income 123 7 Motor fuels 99 8 Motor vehicle 12 9 Occupation and business, NEC 6 10 Property taxes 27 11 Public utilities 9 12 Severance 27 13 Tobacco products 3 14 Total 153
# The numbers are percent changes only. There are no levels. There are no US totals for the individual majors.
# Pew_1995_2014_2015-10-08(Boyd Data Request 100815).xlsx has the policy adjustments, no US totals for major taxes
# 
# Pew_2000_2019_2020-10-13(RevenueVolatilityData).xlsx, and Pew_1998_2017_2018-08-29(RevenueVolatilityData).xlsx
# 
# have the annual volatility score (I think this is policy adjusted growth) by state and major tax. Does not have policy adjustments separately.

# data file Boyd Data Request 100815.xlsx
# fnames <- c("", "", "")


xlfn <- here::here("raw_data", "pew", "Boyd Data Request 100815.xlsx")
sheets <- excel_sheets(xlfn)
sheets

get_pewsheet <- function(sheet){
  print(sheet)
  range <- ifelse(str_detect(sheet, "Major"), "A5:V190", "A5:V56")
  read_excel(xlfn, sheet=sheet, range=range) %>%
    mutate(sheet=sheet)
}

pewdf <- map_dfr(sheets[-1], get_pewsheet)
glimpse(pewdf)
count(pewdf, sheet)
count(pewdf, `Major Revenue Source`)

#    `Major Revenue Source`               n
#    <chr>                            <int>
#  1 Amusements                           9
#  2 Corporation                          9
#  3 Corporation net income              90
#  4 Documentary and stock transfer       6
#  5 General sales and gross receipts   135
#  6 Individual income                  123
#  7 Motor fuels                         99
#  8 Motor vehicle                       12
#  9 Occupation and business, NEC         6
# 10 Property taxes                      27
# 11 Public utilities                     9
# 12 Severance                           27
# 13 Tobacco products                     3
# 14 Total                              153

type_map <- read_delim("revsource; taxtype
Amusements; amusetax
Corporation; corptax
Corporation net income; cit
Documentary and stock transfer; doctax
General sales and gross receipts; gst
Individual income; iit
Motor fuels; mft
Motor vehicle; mvl
Occupation and business, NEC; occtax
Property taxes; proptax
Public utilities; utiltax
Severance; sevtax
Tobacco products; cigtax
Total; tottax", 
delim=";", trim_ws = TRUE)

type_map

pewtax_base <- pewdf %>%
  select(stname=1, revsource=2, everything()) %>%
  left_join(bdata::stcodes %>% select(stabbr, stname), by="stname") %>%
  left_join(type_map, by="revsource") %>%
  mutate(vartype=word(sheet, 3)) %>%
  pivot_longer(-c(stabbr, stname, revsource, sheet, vartype, taxtype), names_to = "year", values_to = "pch") %>%
  mutate(year=as.integer(year)) %>%
  select(stabbr, stname, sheet, vartype, revsource, taxtype, year, pch) %>%
  arrange(stabbr, vartype, taxtype, year)

glimpse(pewtax_base)

levels <- pewtax_base %>%
  filter(vartype %in% c("raw", "adjusted")) %>%
  group_by(stabbr, vartype, taxtype) %>%
  arrange(year) %>%
  do(add_row(.,
             stabbr=.$stabbr[1], 
             stname=.$stname[1],
             sheet=.$sheet[1],
             vartype=.$vartype[1], 
             revsource=.$revsource[1],
             taxtype=.$taxtype[1],
             year=1994, 
             pch=0,
             .before=0)) %>%
  mutate(level=cumprod(1 + pch),
         pch=ifelse(year==1994, NA_real_, pch)) %>%
  ungroup

levels %>% 
  filter(stabbr=="NY", taxtype=="tottax")

pewtax <- pewtax_base %>%
  full_join(levels %>% select(-pch), 
            by = c("stabbr", "stname", "sheet", "vartype", "revsource", "taxtype", "year")) %>%
  arrange(stabbr, vartype, taxtype, year)

# caution: US data do not have major taxes, just the total
count(pewtax, year)
count(pewtax, stabbr, stname)

pewtax %>% filter(stabbr=="US")
saveRDS(pewtax, here::here("data", "details", "pewtax.rds"))
