
state_choro <- function(data, colors, title){
  # return a state map ggplot object
  # data needs to have:
  #   stabbr
  #   sgroup
  #   sgroup_label
  # note that x is long, y is lat
  mdata <- usmap::us_map() %>% 
    rename(stabbr=abbr) %>%
    arrange(full, piece, order) %>%
    left_join(data, by="stabbr")
  
  p <- mdata %>%
    ggplot(mapping = aes(x, y,
                         group = group,
                         fill = sgroup)) +
    geom_polygon(color = "gray90", size = 0.1) +
    coord_equal() +
    theme_map()
  p
}


state_choro2 <- function(data, title="None"){
  # return a state map ggplot object
  # data needs to have:
  #   stabbr
  #   value
  #   cut_label
  # note that x is long, y is lat
  mdpoint <- median(data$value, na.rm=TRUE)
  print(mdpoint)
  
  mdata <- usmap::us_map() %>% 
    rename(stabbr=abbr) %>%
    arrange(full, piece, order) %>%
    left_join(data, by="stabbr")
  
  p <- mdata %>%
    ggplot(mapping = aes(x, y,
                         group = group,
                         fill = value)) +
    geom_polygon(color = "gray90", size = 0.1) +
    coord_equal() +
    theme_map() +
    # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = mdpoint)
    scale_fill_continuous(
      low = "white", high = "red", label = scales::comma
    )
  p
}



df <- measures %>%
  filter(taxtype=="tottax", stabbr != "US") %>% # , stabbr != "AK"
  mutate(value=hpsd)

quantile(df$hpsd, probs=c(0, .2, .4, .6, .8, 1))

state_choro2(df)


tmp <- usmap::us_map() %>% 
  rename(stabbr=abbr,
         long=x, lat=y) %>%
  arrange(full, piece, order)

p <- ggplot(data = tmp,
            mapping = aes(x = long, y = lat,
                          group = group, fill = stabbr))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_equal() +
  guides(fill = FALSE)

stcodes %>%
  arrange(beargn.name, stname)


pit_colors <- c("#2E74C0", "#CB454A") 


measures2 <- measures %>%
  left_join(stcodes2 %>%
              select(stabbr, stname, cenrgn.name),
            by="stabbr")

df <- measures2 %>%
  filter(taxtype=="tottax", stabbr != "US") %>%
  # mutate(value=ifelse(stabbr=="AK", NA_real_, hpsd)) %>%
  mutate(value=hpsd)
mdn <- median(df$hpsd, na.rm=TRUE)


# php <- df %>%
#   ggplot(mapping = aes(x = value,
#                        y = reorder(stname, hpsd))) +
#   geom_vline(xintercept = mdn, color = "gray30") +
#   geom_point(size = 1, colour="blue") +
#   scale_x_continuous(breaks = seq(0, 30, 2)) +
#   scale_x_break(breaks=c(10, 25)) +
#   facet_wrap(~ cenrgn.name, ncol=1, scales="free_y") +
#   guides(color="none") + 
#   labs(x = "Volatility (deviation from trend)") +
#   labs(y = NULL) +
#   theme(axis.text=element_text(size=8)) +
#   theme_bw()
# php


p <- ggplot(economics, aes(x=date, y = unemploy, colour = uempmed)) +
  geom_line()

p + scale_wrap(n=4)


df <- measures2 %>%
  filter(taxtype=="tottax", stabbr != "US")
mdns <- df %>%
  summarise(across(c(pchsd, hpsd, sre, srese, sregsp, sregspse), ~ median(.x, na.rm=TRUE)))
mdns$hpsd

php <- df %>%
  ggplot(mapping = aes(x = hpsd,
                       y = reorder(stname, hpsd))) +
  geom_point(size = 1, colour="blue") +
  scale_x_break(breaks=c(10, 26.5), scales = 0.1, ticklabels = c(26.5, 27)) +
  labs(x = "Volatility measure") +
  ylab(NULL) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0)) +
  ggtitle("State tax volatility, 1990-2018, deviation from trend")
php

psre <- df %>%
  ggplot(mapping = aes(x = sre,
                       y = reorder(stname, sre))) +
  geom_point(size = 1, colour="blue") +
  scale_x_break(breaks=c(10, 26.5), scales = 0.1, ticklabels = c(26.5, 27)) +
  labs(x = "Volatility measure") +
  ylab(NULL) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0)) +
  ggtitle("State tax volatility, 1990-2018, short-run elasticity")
psre

# geom_vline(xintercept = mdn, color = "gray30") +

# seg <- data.frame(x1 = mdn, x2 = mdn, y1 = min(df$value), y2 = max(df$value))
  # geom_vline(xintercept=mdn, show.legend = NULL)
  # geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = seg)
  
  # geom_vline(xintercept=mdn, show.legend = FALSE)


# geom_vline(name=NULL, xintercept = mdn, color = "gray30") +
# p1 <- ggplot(d, aes(y, x)) + geom_col(orientation="y")
# d2 <- data.frame(x = c(2, 18), y = c(7, 26), label = c("hello", "world"))
# p2 <- p1 + scale_x_break(c(7, 17)) + 
#   geom_text(aes(y, x, label=label), data=d2, hjust=1, colour = 'firebrick')  + 
#   xlab(NULL) + ylab(NULL) + theme_minimal()







party_colors <- c("#2E74C0", "#CB454A") 

p0 <- ggplot(data = subset(election, st %nin% "DC"),
             mapping = aes(x = r_points,
                           y = reorder(state, r_points),
                           color = party))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") +
  geom_point(size = 2)

p2 <- p1 + scale_color_manual(values = party_colors)

p3 <- p2 + scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30, 40),
                              labels = c("30\n (Clinton)", "20", "10", "0",
                                         "10", "20", "30", "40\n(Trump)"))

p3 + facet_wrap(~ census, ncol=1, scales="free_y") +
  guides(color=FALSE) + labs(x = "Point Margin", y = "") +
  theme(axis.text=element_text(size=8))






#.. Libraries ----
library(maps)
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
library(usmap)
library(gridExtra)

#.. Functions ----
theme_map <- function(base_size=9, base_family="") {
  # see:
  # https://socviz.co/maps.html
  # https://github.com/kjhealy/socviz
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

get_mdata <- function(data){
  # df must have a column named stabbr
  mdata <- left_join(usmap::us_map() %>% arrange(full, piece, order),
                     data %>% rename(abbr=stabbr),
                     by="abbr")
  return(mdata)
}

get_stateplot <- function(data, fillvar){
  # data is a data frame
  # fillvar is a character value with the name of the variable that will determine the fill color
  mdata <- get_mdata(data)
  # p <- ggplot(data = mdata, aes(x = long, y = lat, group = group)) +
  p <- ggplot(data = mdata, aes(x = x, y = y, group = group)) +
    geom_polygon(aes(fill=mdata[[fillvar]]), color = "gray90", size = 0.1, na.rm=TRUE) +
    coord_equal() +
    theme_map() +
    theme(legend.position = "right")
  return(p)
}

#.. Data ----
agepop <- wpop %>%
  filter(stabbr %in% c("DC", state.abb),
         sex=="Total", year %in% c(2020, 2040),
         popgroup %in% c(globals$wc_older, "poptot")) %>%
  mutate(older=ifelse(popgroup %in% globals$wc_older, "older", "total")) %>%
  group_by(stabbr, year, older) %>%
  summarise(pop=sum(value)) %>%
  ungroup %>%
  spread(older, pop) %>%
  mutate(pctold=older / total * 100) %>%
  select(stabbr, year, pctold)

# glimpse(agepop)
# agepop %>%
#   group_by(year) %>%
#   do(qtiledf(.$pctold, probs=0:10/10))

#.. Draw map ----
# group the age data
# INCLUDING DC IS CRITICAL IF IT IS IN THE MAPDATA (else leave it out of map data) -- else we will get an NA facet
# cuts <- c(0, 14, 17, 19, 21, 24, 30)
cuts <- c(0, 14, 16, 18, 20, 24, 30)
agepop$agroup <- cut(agepop$pctold, breaks=cuts)
# count(agepop, year, agroup) %>% spread(year, n)

# labs <- c("<= 14%", levels(agepop$agroup)[2:5], "> 24%")

levs <- levels(agepop$agroup)
labs <- c("<= 14%",
          " > 14% to 16%",
          " > 16% to 18%",
          " > 18% to 20%",
          " > 20% to 24%",
          " > 24%")
# cbind(levs, labs)

agepop <- agepop %>%
  mutate(agroup=factor(agroup, labels=labs))
# count(agepop, agroup)

capt <- str_wrap(paste0("Source: Author's analysis of projections from ", wc_src), 90)

colors <- c('#31a354','#f0f9e8','#ffffb2','#fee391','#feb24c','#f03b20')
p <- get_stateplot(agepop, "agroup") +
  scale_fill_manual(values=colors, drop=FALSE)+
  theme(legend.position = "right") +
  guides(fill=guide_legend(title="% age 65+")) +
  geom_polygon(color = "black", fill=NA) +
  facet_wrap(~year, ncol=1) +
  ggtitle("Percent of population age 65+") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.background = element_rect(colour="lightgrey", fill=NA),
        strip.text = element_text(size = 10, face="bold")) +
  labs(caption=capt) +
  theme(plot.caption = element_text(hjust=0, size=8))
p
ggsave("./results/states/pctold_map.png", p, width=8, height=6.5, units="in")
ggsave(plot=p, filename=here::here("figures_in_report", "pctold_map.png"), width=8, height=6.5, units="in")



# get 48-state map data
usstatemap<-map_data('state')
usstatemap$stabbr<-as.character(factor(usstatemap$region,levels=tolower(stname), labels=stabbr))
# head(usstatemap)
# table(usstatemap$stabbr)

# construct data frame with state centers and stabbr to annotate the map
stabbrmaplabels<-data.frame(state.center, state.abb);
stabbrmaplabels<-subset(stabbrmaplabels, !state.abb %in% c("AK", "HI"))
names(stabbrmaplabels)<-rencol(stabbrmaplabels,"state.abb","stabbr")
stabbrmaplabels$group<-1 # we need a dummy group variable on the file so ggplot does not get confused, since we are using group in these maps
# stabbrmaplabels

# now get US state map with AK and HI - not sure if we'll use this
load(paste(rdat,"maps\\","usakhimoved.RData",sep=""))
# gpclibPermit() # Necessary for ggplot2 fortify to run
usmap50<-fortify(usmap, region="stabbr") # Reshape map for use by ggplot
names(usmap50)<-rencol(usmap50,"id","stabbr")
# head(usmap50)
# table(usmap50$stabbr)


library(ggplot2)
library(ggbreak) 
library(patchwork)

set.seed(2019-01-19)
d <- data.frame(x = 1:20,
                y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
)

p1 <- ggplot(d, aes(y, x)) + geom_col(orientation="y")
d2 <- data.frame(x = c(2, 18), y = c(7, 26), label = c("hello", "world"))
p2 <- p1 + scale_x_break(c(7, 17)) + 
  geom_text(aes(y, x, label=label), data=d2, hjust=1, colour = 'firebrick')  + 
  xlab(NULL) + ylab(NULL) + theme_minimal()

p1 + p2

