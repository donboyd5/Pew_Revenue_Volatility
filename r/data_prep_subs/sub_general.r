
# GENERAL general.rdata ----

#.. data frame indicating start of each state's fiscal year ----
# -   46 states have a July 1 fiscal year
# -   NY has April 1
# -   TX has Sept 1
# -   AL, MI have Oct 1
# -   See https://www.ncsl.org/research/fiscal-policy/fy-2021-state-budget-status.aspx

sfy_startmonths <- tibble(stabbr=state.abb) %>%
  mutate(startmonth=case_when(stabbr=="NY" ~ 4,
                              stabbr=="TX" ~ 9,
                              stabbr %in% c("AL", "MI") ~ 10,
                              TRUE ~ 7))

# as of now this is the only object to put in general.RData
save(sfy_startmonths, file = here::here("data", "general.RData"))
