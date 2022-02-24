
prep_3panel <- function(df){
  # df must have year, value
  df %>%
    select(year, value) %>%
    mutate(trend=hptrend(value, smooth=6.25),
           pch=value / value[match(year - 1, year)] - 1,
           pchtrend=trend / trend[match(year - 1, year)] - 1,
           pdtrend=value / trend - 1)
}


p1f <- function(df, title, colors,
                xinterval=5, xlims=c(NA, NA), 
                ylims=c(NA, NA), 
                scale=1, scaled_units=NULL){
  
  subtitle <- "Amount"
  if(!is.null(scaled_units)) subtitle <- paste0(subtitle, " in ", scaled_units)
  
  p1 <- df %>%
    mutate(value=value / scale, trend=trend / scale) %>%
    pivot_longer(cols=c(value, trend), values_to = "measure") %>%
    mutate(name=factor(name, levels=c("value", "trend"), labels=c("actual", "trend"))) %>%  # to get right sort order
    ggplot(aes(year, measure, colour=name)) +
    geom_line() +
    scale_y_continuous(name="amount", limits=ylims, labels=scales::dollar_format()) + # scale=1e-6
    scale_x_continuous(name=NULL, breaks=seq(1950, 2050, xinterval), limits=xlims) +
    scale_colour_manual(values=colors) +
    ggtitle(title,
            subtitle=subtitle) +
    theme_report +
    nolegend_title
  p1
}
# p1f(df2, title="Gross domestic product", colors=c("blue", "red"))
# p1f(df2, title="Gross domestic product", colors=c("blue", "red"), scaled_units="$ thousands")
# p1f(df2, title="Gross domestic product", colors=c("blue", "red"), scale=1e6, scaled_units="$ billions")
# p1f(caiit, title="California income tax", colors=c("blue", "red"), scale=1e6, scaled_units="$ billions")


p2f <- function(df, title, colors,
                xinterval=5, xlims=c(NA, NA),
                yinterval=.1, ylims=c(NA, NA)){

  p2 <- df %>%
    pivot_longer(cols=c(pch, pchtrend), values_to = "measure") %>%
    mutate(name=factor(name, levels=c("pch", "pchtrend"), labels=c("actual", "trend"))) %>%  # to get right sort order
    ggplot(aes(year, measure, colour=name)) +
    geom_line() +
    geom_hline(yintercept = 0, colour="grey") +
    scale_y_continuous(name="% change", 
                       breaks=c(0, seq(-1, 1, yinterval)),
                       limits=ylims, 
                       labels=scales::percent_format(accuracy=1)) +
    scale_x_continuous(name=NULL, breaks=seq(1950, 2050, xinterval), limits=xlims) +
    scale_colour_manual(values=colors) +
    ggtitle(title,
            subtitle="Annual percent change") +
    theme_report +
    nolegend_title
  p2
}
# p2f(df2, title="Gross domestic product", colors=c("blue", "red"))
# p2f(caiit, title="California income tax", colors=c("blue", "red"))


p3f <- function(df, title, 
                xinterval=5, xlims=c(NA, NA),
                yinterval=.01, ylims=c(NA, NA)){
  p3 <- df %>%
    mutate(posneg=ifelse(pdtrend < 0, "neg", "pos")) %>%
    ggplot(aes(year, pdtrend)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", aes(fill = posneg)) + 
    scale_fill_manual(values=c("red", "blue")) +
    scale_x_continuous(name=NULL, breaks=seq(1950, 2050, xinterval), limits=xlims) +
    scale_y_continuous(name="% difference from trend",
                       breaks=c(0, seq(-1, 1, yinterval)), 
                       labels=scales::percent_format(accuracy=1),
                       limits=ylims) +
    ggtitle(title,
            subtitle="Percent difference from trend") +
    theme_report +
    nolegend
  p3
}
# p3f(caiit, title="California income tax")




get_min <- function(vec, nearest=5, offset=0){
  vmin <- min(vec, na.rm=TRUE)
  vmin <- floor((vmin - offset) / nearest) * nearest
  vmin
}

get_max <- function(vec, nearest=5, offset=0){
  vmax <- max(vec, na.rm=TRUE)
  vmax <- ceiling((vmax + offset) / nearest) * nearest
  vmax
}

# test get_xmin and get_xmax
# yrs <- c(1941, 1947, 1950, 1952, 1954, 1955, 1956)
# purrr::map_dbl(yrs, get_xmin)
# purrr::map_dbl(yrs, get_xmax)


panel3 <- function(df, title, 
                   colors=c("blue", "black"), 
                   xinterval=5, xlims=c(NULL, NULL), 
                   ylims=c(NA, NA),
                   scale=1, scaled_units=NULL){
  # return a stacked 3 panel plot:
  #    top: amount and trend
  #    middle:  percent change of both
  #    bottom:  percent difference from trend
  
  # df must have
  #  value, trend, pch, pdtrend (latter 2 as proportions, not percents)
  
  # determine x limits so that all three graphs have the same limits
  # CAUTION: I only check xlims[1] and decide based on that
  if(is.null(xlims[1])){
    xmin <- get_min(df$year, nearest=1, offset=1)
    xmax <- get_max(df$year, nearest=1, offset=1)
    xlims <- c(xmin, xmax)
  }
  # print(xlims)
  
  p1 <- p1f(df, title=title, colors=colors, 
            xinterval=xinterval, xlims=xlims, 
            ylims=ylims, 
            scale=scale, scaled_units=scaled_units)  # levels
  
  p2 <- p2f(df, title=title, colors=colors, 
            xinterval=xinterval, xlims=xlims, 
            ylims=ylims)  # percent change
  
  p3 <- p3f(df, title=title, xinterval=xinterval, xlims=xlims, yinterval=.05)  # % diff from trend
  
  p123 <- p1 / p2 / p3
  p123
}



revecon4 <- function(revdf, econdf, revtitle, econtitle,
                     colors=c("blue", "black"), 
                     xinterval=5, xlims=c(NULL, NULL)){
  # return a 6 panel plot:
  # 2 rows, 3 columns
  #    col1:  percent change actual and trend, rev over econ
  #    col2:  percent difference from trend, rev over econ
  
  # df must have
  #  value, trend, pch, pdtrend (latter 2 as proportions, not percents)
  
  # determine x limits so that all three graphs have the same limits
  # CAUTION: I only check xlims[1] and decide based on that
  if(is.null(xlims[1])){
    xmin <- get_min(c(revdf$year, econdf$year), nearest=1, offset=1)
    xmax <- get_max(c(revdf$year, econdf$year), nearest=1, offset=1)
    xlims <- c(xmin, xmax)
  }
  # print(xlims)
  
  # get one set of ylims for the percent change graphs, and one set for the percent difference graphs
  ymin <- get_min(c(revdf$pch, econdf$pch), nearest=.005, offset=.001)
  ymax <- get_max(c(revdf$pch, econdf$pch), nearest=.005, offset=.001)
  ylims <- c(ymin, ymax)
  yinterval = (ymax - ymin) / 10
  p2rev <- p2f(revdf, title=revtitle, colors=colors, xinterval, xlims, yinterval, ylims)  # percent change
  p2econ <- p2f(econdf, title=econtitle, colors=colors, xinterval, xlims, yinterval, ylims)  # percent change
  
  ymin <- get_min(c(revdf$pdtrend, econdf$pdtrend), nearest=.005, offset=.001)
  ymax <- get_max(c(revdf$pdtrend, econdf$pdtrend), nearest=.005, offset=.001)
  ylims <- c(ymin, ymax)
  yinterval = (ymax - ymin) / 10
  p3rev <- p3f(revdf, title=revtitle, xinterval, xlims, yinterval, ylims)  # % diff from trend
  p3econ <- p3f(econdf, title=econtitle, xinterval, xlims, yinterval, ylims)  # % diff from trend
  
  pleft <- p2rev / p2econ
  pright <- p3rev / p3econ
  pleft | pright
  # pright
}





