
ne.cen <- c(9, 23, 25, 33, 44, 50, 34, 36, 42)
mw.cen <- c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46)
south.cen <- c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48)
west.cen <- c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53)

stcodes2 <- stcodes %>%
  mutate(ifips=as.integer(stfips),
         cenrgn=case_when(ifips %in% ne.cen ~ "ne.cen",
                          ifips %in% mw.cen ~ "mw.cen",
                          ifips %in% south.cen ~ "south.cen",
                          ifips %in% west.cen ~ "west.cen",
                          ifips == 0 ~ "US"),
         cenrgn.name=case_when(cenrgn=="ne.cen" ~ "Northeast",
                               cenrgn=="mw.cen" ~ "Midwest",
                               cenrgn=="south.cen" ~ "South",
                               cenrgn=="west.cen" ~ "West"))
saveRDS(stcodes2, here::here("data", "stcodes2.rds"))

stcodes2 %>%
  select(cenrgn, stname) %>%
  arrange(cenrgn, stname)

# https://www2.census.gov/geo/docs/maps-data/maps/reg_div.txt
# census regions
# Census Bureau Regions and Divisions with State FIPS Codes
# 
# REGION I: NORTHEAST
# 
# Division I: New England 
# Connecticut     (09)
# Maine           (23)
# Massachusetts   (25)
# New Hampshire   (33)
# Rhode Island    (44)
# Vermont         (50)
# 
# Division 2: Middle Atlantic
# New Jersey      (34)
# New York        (36)
# Pennsylvania    (42)
# 
# ne.cen <- c(9, 23, 25, 33, 44, 50, 34, 36, 42)
# 
# REGION 2: MIDWEST*
#   
#   Division 3:  East North Central
# Illinois        (17)
# Indiana         (18)
# Michigan        (26)
# Ohio            (39)
# Wisconsin       (55)
# 
# 
# Division 4:  West North Central
# Iowa            (19)
# Kansas          (20) 
# Minnesota       (27)
# Missouri        (29)
# Nebraska        (31)
# North Dakota    (38)
# South Dakota    (46)
# c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46)
# 
# REGION 3: SOUTH
# 
# Division 5: South Atlantic
# Delaware        (10)
# District of Columbia (11)
# Florida         (12)
# Georgia         (13)
# Maryland        (24)
# North Carolina  (37)
# South Carolina  (45)
# Virginia        (51)
# West Virginia   (54)
# 
# Division 6: East South Central
# Alabama         (01)
# Kentucky        (21)
# Mississippi     (28)
# Tennessee       (47)
# 
# Division 7:  West South Central
# Arkansas        (05)
# Louisiana       (22)
# Oklahoma        (40)
# Texas           (48)
# 
# south.cen <- c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48)
# 
# REGION 4: WEST
# 
# Division 8:  Mountain
# Arizona         (04)
# Colorado        (08)
# Idaho           (16)
# Montana         (30)
# Nevada          (32)
# New Mexico      (35)
# Utah            (49)
# Wyoming         (56)
# 
# 
# Division 9: Pacific
# Alaska          (02)
# California      (06)
# Hawaii          (15)
# Oregon          (41)
# Washington      (53)
#
# west.cen <- c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53)

