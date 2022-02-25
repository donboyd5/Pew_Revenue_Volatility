
#.. constants ----
# vorder, taxnames
stdc <- c(state.abb, "DC")

(decades <- paste0(seq(1960, 2010, 10), "-", seq(1969, 2019, 10)))
fullperiod <- "1970-2020"
last20 <- "2000-2020"

#.. colors ----
# https://colorbrewer2.org/
darkred <- "#a50f15"
bluered <- c("blue", darkred)  # #a50f15 darkred
bluegreen <- c("blue", "darkgreen")


# examine colors with the following functions
# RColorBrewer colors:
# brewer.pal.info
# brewer.pal.info[brewer.pal.info$category == "seq",]
# display.brewer.pal(7,"Accent")
# display.brewer.pal(3,"Dark2")
# display.brewer.pal(4,"Dark2")
# display.brewer.pal(4,"Set1")
# display.brewer.pal(4,"Set2")
# display.brewer.pal(4,"Set3")

# scale_color_viridis()


#.. graph theme items ----

theme_report <- theme_bw() + 
  theme(
    plot.caption = element_text(hjust = 0)
  )

#.. miscellaneous constants ---
mlabels <- tribble(
  ~measure, ~mlabel,
  "pchsd", "Standard deviation of annual % change",
  "hpsd", "Standard deviation of % difference from trend",
  "sre", "Short-run elasticity vs. state GDP",
  "esum", "Average cumulative % difference from trend, per episode above or below trend",
  "srese", "Standard error short-run elasticity",
  "elength", "Average length of deviation-from-trend episodes"
)

taxnames <- tribble(
  ~name, ~namef,
  "tottax", "Total taxes",
  "iit", "Personal income tax",
  "gst", "General sales tax",
  "selsalestax", "Selective sales taxes",
  "cit", "Corporate income tax",
  "sevtax", "Severance taxes",
  "othertax", "Other taxes"
)