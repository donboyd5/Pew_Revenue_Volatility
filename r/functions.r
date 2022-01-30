
# general utility functions ----
ns <- function(df) {names(df) %>% sort}

ma <- function(x, period) {
  # create trailing moving average of x, with length period
  zoo::rollapply(x, period, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
}


stname <- function(stabbr) {
  stabbrs <- c(state.abb, "DC", "US")
  stnames <- c(state.name, "District of Columbia", "United States")
  stnames[match(stabbr, stabbrs)]
}

stabbr <- function(stname) {
  stabbrs <- c(state.abb, "DC", "US")
  stnames <- c(state.name, "District of Columbia", "United States") %>% str_to_upper()
  stabbrs[match(str_to_upper(stname), stnames)]
}


# date-related functions ----
qtodate <- function(yq){
  year <- str_sub(yq, 1, 4)
  q <- str_sub(yq, -1) %>% as.integer
  mo <- (q - 1)*3 +1
  paste0(year, "-", mo, "-", 1) %>% as.Date()
  # qtodate(c("2019Q1", "2019Q2", "2019Q3", "2019Q4"))
}

# don't need following function - yq in lubridate does what we need
# yq_date <- function(y, q, fmt="%YQ%q", frac=0){
#   # frac=0 gives beginning of month, 1 gives end of month
#   as.Date(zoo::as.yearqtr(paste0(y, q), format = fmt), frac=frac)
# }


# statistical functions ----

p25 <- function(x) {as.numeric(quantile(x, .25, na.rm=TRUE))} # use braces so function appears in RStudio outline
p50 <- function(x) {as.numeric(quantile(x, .50, na.rm=TRUE))}
p75 <- function(x) {as.numeric(quantile(x, .75, na.rm=TRUE))}
pany <- function(x, p) {as.numeric(quantile(x, p, na.rm=TRUE))}

pchya <- function(value, year){
  value / value[match(year - 1, year)] -1
}

diffya <- function(value, year){
  value - value[match(year - 1, year)]
}

rollsd <- function(x, nobs) {
  # note that this is sample standard deviation
  zoo::rollapply(x, nobs, function(x) sd(x, na.rm=TRUE), fill=NA, align="right")
}

rollsd_p <- function(x, nobs) {
  # population standard deviation
  sdp <- function(x){
    n <- sum(!is.na(x))
    sd(x, na.rm=TRUE) * sqrt((n - 1) / n)
  }
  zoo::rollapply(x, nobs, function(x) sdp(x),
                 fill=NA, align="right")
}

rollsd_p2 <- function(x, nobs) {
  # population standard deviation
  sdp <- function(x){
    n <- sum(!is.na(x))
    sd(x, na.rm=TRUE) * sqrt((n - 1) / n)
  }
  zoo::rollapply(x, nobs, function(x) sdp(x),
                 fill=NA, align="center")
}

rollmean <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")}

rollmin <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) min(x, na.rm=TRUE), fill=NA, align="right")}

se <- function(model) {sqrt(diag(vcov(model)))} # we don't need this now


# graphics related ----
recdata <- function(years){
  # return a recessions data frame including just the years noted
  recessions %>%
    filter(rec_year %in% !!years)
}

geom_band <- function(recdf, period="annual", fill="grey", alpha=.5, ...) {
  # for recession bands and other bands
  geom_rect(
    mapping=aes(xmin = peak_decimal,
                xmax = trough_decimal),
    data=recdf,
    ymin = -Inf, ymax = Inf,
    fill = fill,
    alpha = alpha, # larger alpha is darker rectangle
    ...,
    inherit.aes = FALSE, show.legend = FALSE)
}

#.. OLD recessions bands in next 2 functions ----
gband <- function(xmin, xmax, fill="grey", alpha=.5, ymin=-Inf, ymax=Inf) {
  # for recession bands and other similar time-series bands in ggplot
  annotate("rect",  # rectangle
           fill = fill,
           alpha = alpha, # larger alpha is darker rectangle
           xmin = xmin, xmax = xmax,
           ymin = ymin, ymax = ymax)
}

recband <- function(recdf, period="annual", fill="grey", alpha=.5) {
  # for recession bands
  annotate("rect",  # rectangle
           fill = fill,
           alpha = alpha, # larger alpha is darker rectangle
           xmin = recdf$peak_decimal, xmax = recdf$trough_decimal,
           ymin = -Inf, ymax = Inf)
}


rfy <- function(ryear, pt="peak"){
  if(pt=="peak") rec2$fypeak[match(ryear, rec2$year)] else
    rec2$fytrough[match(ryear, rec2$year)]
}

rcy <- function(ryear, pt="peak"){
  if(pt=="peak") rec2$cypeak[match(ryear, rec2$year)] else
    rec2$cytrough[match(ryear, rec2$year)]
}


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



# sp500 graph ----
# recover <- .0
# (sp2019 <- sp500a$value[sp500a$year==2019]) # 2913.36
# recent <- 2711 # March 13
# (pch2020 <- recent * (1+recover) / sp2019 * 100 - 100)
# p <- sp500a %>%
#   filter(year >= 1970, year<2020) %>%
#   ggplot(aes(year, pch)) +
#   geom_line(colour="blue", size=1) +
#   geom_point(colour="blue", size=1) +
#   geom_hline(yintercept = 0) +
#   geom_line(data=tibble(year=2019:2020, pch=c(6.09, pch2020)), colour="red", size=1, linetype="dashed") +
#   geom_point(data=tibble(year=2020, pch=pch2020), colour="red", size=1) +
#   scale_color_manual(values=c("blue", "darkgreen")) +
#   scale_y_continuous(name="Percent change", breaks=seq(-100, 200, 5)) +
#   scale_x_continuous(name="Calendar year", breaks = seq(1960, 2020, 5)) +
#   ggtitle("S&P 500 calendar year average percent change",
#           subtitle=paste0("Note: Red line shows 2020 S&P calendar average change of ",
#                           sprintf("%3.1f", pch2020),
#                           "% if market is flat for rest of year")) +
#   gband(rcy(1973, "peak"), rcy(1973, "trough")) +
#   gband(rcy(1980, "peak"), rcy(1980, "trough")) +
#   gband(rcy(1981, "peak"), rcy(1981, "trough")) +
#   gband(rcy(1990, "peak"), rcy(1990, "trough")) +
#   gband(rcy(2001, "peak"), rcy(2001, "trough")) +
#   gband(rcy(2007, "peak"), rcy(2007, "trough")) +
#   theme_bw() +
#   theme(legend.title = element_blank())
# p
# ggsave(here::here("results", "stocks.png"), plot=p, width=10, height=8, scale=1)
# ggsave(here::here("results", "stocks_ppt.png"), plot=p, width=12, height=6, scale=1) 

# define cy and fy peaks and troups
# recessions2 <- recessions %>%
#   mutate(fypeak=datefy(peak),
#          fytrough=datefy(trough),
#          cypeak=year(peak) -1 + (month(peak)) / 12,
#          cytrough=year(trough) -1 + (month(trough)) / 12)


# date related




