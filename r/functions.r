
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


rfy <- function(ryear, pt="peak"){
  if(pt=="peak") rec2$fypeak[match(ryear, rec2$year)] else
    rec2$fytrough[match(ryear, rec2$year)]
}

rcy <- function(ryear, pt="peak"){
  if(pt=="peak") rec2$cypeak[match(ryear, rec2$year)] else
    rec2$cytrough[match(ryear, rec2$year)]
}


