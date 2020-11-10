

ns <- function(df) {names(df) %>% sort}

ma <- function(x, period) {
  # create trailing moving average of x, with length period
  zoo::rollapply(x, period, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
}

se <- function(model) {sqrt(diag(vcov(model)))} # we don't need this now

yq_date <- function(y, q, fmt="%YQ%q", frac=0){
  # frac=0 gives beginning of month, 1 gives end of month
  as.Date(zoo::as.yearqtr(paste0(y, q), format = fmt), frac=frac)
}

p25 <- function(x) {as.numeric(quantile(x, .25, na.rm=TRUE))} # use braces so function appears in RStudio outline
p50 <- function(x) {as.numeric(quantile(x, .50, na.rm=TRUE))}
p75 <- function(x) {as.numeric(quantile(x, .75, na.rm=TRUE))}
pany <- function(x, p) {as.numeric(quantile(x, p, na.rm=TRUE))}

pchya <- function(value, year){
  value / value[match(year - 1, year)] * 100 - 100
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

