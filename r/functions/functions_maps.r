#.. function to map vol measures ----
# Sequential color palettes available in colorbrewer
# Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd,
# Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd


f_volmap <- function(tax="tottax", measure="sre", 
                     gtitle, data=volrecs, facetvar="recyear") {
  
  # titles
  if(measure=="sre"){
    mname="short-run elasticity"
  } else if(measure=="pchiqr"){
    mname="interquartile range of percent change"
  } else if(measure=="dpchtrendiqr"){
    mname="interquartile range of difference in percent change from trend"
  }else if(measure=="pdtrendiqr"){
    mname="interquartile range of percent deviation from trend"
  }
  gsubtitle <- paste0("Volatility measure: ", mname)
  
  # build the data
  voldata <- data %>%
    filter(name==tax, realnom=="nominal") %>%
    select(facet=all_of(facetvar), stabbr, value=all_of(measure))
  
  # cut the measure into quartiles, over all recession years
  
  f_breaks <- function(value){
    # function to define quartile cuts
    qtiles <- quantile(value, p=c(0, .25, .5, .75, 1), na.rm=TRUE)
    # breaks replace min and max with infinite
    c(-Inf, qtiles[-c(1, length(qtiles))], Inf)
  }
  breakpoints <- f_breaks(voldata$value)
  
  # create decent labels for the groups defined by these breakpoints
  f_cutlabs <- function(breakpoints){
    if(measure %in% c("pdtrendiqr", "pchiqr", "dpchtrendiqr")){
      fmt_breaks <- percent(breakpoints[2:4], accuracy=.1)
    } else if(measure %in% c("sre")){
      fmt_breaks <- number(breakpoints[2:4], accuracy=.01)
    }
    lab1 <- paste0("Q1: <= ", fmt_breaks[1])
    lab2 <- paste0("Q2: > ", fmt_breaks[1], " - ", fmt_breaks[2])
    lab3 <- paste0("Q3: > ", fmt_breaks[2], " - ", fmt_breaks[3])
    lab4 <- paste0("Q4: > ", fmt_breaks[3])
    c(lab1, lab2, lab3, lab4)
  }
  vlabs <- f_cutlabs(breakpoints)
  
  # create input data frame with all states plus DC for all periods
  # then cut into groups and label
  mbase <- expand_grid(stabbr=c(state.abb, "DC"),
                       facet=unique(voldata$facet)) %>%
    left_join(voldata, by = c("stabbr", "facet")) %>%
    mutate(vcut=cut(value, breakpoints),
           vlab=factor(as.integer(vcut), levels=1:4, labels = vlabs))
  
  # now construct the mapdata and create the map
  # we need an input data frame that has all states plus DC for all years
  mdata <- states51 %>%
    left_join(mbase, by="stabbr")
  
  m <- mdata %>%
    ggplot(aes(x = long, y = lat, group = group, fill = vlab)) +
    geom_polygon(color = "black", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_brewer(palette = "BuPu", na.translate = FALSE) + # , na.value="grey90") +
    # scale_fill_manual(breaks=as.integer(1:4), values=c("red", "green", "yellow", "blue")) +
    theme_map() + 
    theme(legend.position = "right") +
    labs(fill="Quartile: ") +
    # legend_notitle +
    facet_wrap(~facet, ncol = 2) +
    ggtitle(gtitle, subtitle=gsubtitle)
  
  # return the base map data and the map
  mlist <- list(mbase=mbase, m=m)
}
