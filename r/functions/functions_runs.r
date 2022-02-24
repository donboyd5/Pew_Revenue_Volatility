
#**************************************************************************
#                                                                         
# functions for examining runs of data ------------------------------------
#
#**************************************************************************

allpos <- function(x) {
  # TRUE if ALL elements in a vector are (1) not NA, and (2) strictly positive
  all(!is.na(x) & x > 0)
  # check allpos:
  # allpos(c(1, 2, 3)) # TRUE
  # allpos(c(-1, 2, 3)) # FALSE
  # allpos(c(0, 2, 3)) # FALSE
  # allpos(c(NA, 2, 3)) # FALSE
}

nopos <- function(x){
  # TRUE if there are NO positive values in a vector
  !any(x > 0, na.rm=TRUE)
  # check nopos:
  # nopos(c(0, 0, 0)) # TRUE - no positive values
  # nopos(c(0, 0, 1)) # FALSE - there is a positive
  # nopos(c(NA, 0, 0)) # TRUE - no positives
  # nopos(c(NA, 0, 1)) # FALSE - there is a positiver value
}


# this next function detects consecutive zero and/or NA values in a vector
# (for example, in tax X in state Y)
# based on previous examination of the data, I have
# concluded these represent gaps in the data, and that in such a situation
# we will want to keep only the more-recent run of the data
# identifying these consec values and later converting them to NA makes it
# easy to drop them
consec_zna <- function(x){
  # identify consecutive zero or NA values in a vector
  # TRUE for a vector element if this element is zero or NA, AND: 
  #   previous (i.e., lag) element, or
  #   next (i.e., lead) element, or
  #   both (lag and lead) are either zero or NA
  # in other words it returns a vector that is TRUE in the places where
  # there are any consecutive zero--NA values
  zna <- function (x1) is.na(x1) | x1==0 # is each vector element zero or NA?
  
  # define lag and lead; note that:
  #   lag of first element doesn't exist, so can't be zero or NA
  #   same for lead of last element
  lastx <- length(x)
  xlag <- c(Inf, lag(x)[-1]) 
  xlead <- c(lead(x)[-lastx], Inf) # lead of last element doesn't exist, ...
  
  z <- zna(x)
  zlag <- zna(xlag)
  zlead <- zna(xlead)
  consec_zna <- z & (zlag | zlead)
  consec_zna
  # uncomment this next line to show the details
  # tibble(x, z, zlag, zlead, consec_zna)
  
  # tests:
  # x <- c(1, 2, 3, 4)  # all FALSE
  # x <- c(0, 1, 2, 3)  # all FALSE
  # x <- c(0, 0, 1, 2)  # TRUE TRUE FALSE FALSE
  # x <- c(NA, NA, 1, 2) # TRUE TRUE FALSE FALSE
  # x <- c(NA, 0, 1, 2) # TRUE TRUE FALSE FALSE
  # x <- c(0, 1, NA, NA, 1, 2) # FALSE FALSE  TRUE  TRUE FALSE FALSE
  # x <- c(0, 1, 0, NA, 1, 2) # FALSE FALSE  TRUE  TRUE FALSE FALSE
  # consec_zna(x)
}


last_na <- function(x) {
  # identify the rightmost NA in a vector
  #   e.g., for c(1, NA, 2, NA, 5) the 4th element is the last NA
  #   so the function would return 4
  inax <- which(is.na(x)) # get indexes of the NA values of x
  # get the maximum index; inax will be empty if there are no NA values,
  # which would trigger a warning, so suppress warnings
  lna <- suppressWarnings(max(inax)) 
  lna
  
  # tests:
  # x <- c(1, 2, 3, 4, 5)
  # x <- c(1, 2, NA, NA, 5)
  # x <- c(1, NA, 2, NA, 5)
  # x <- c(1, 2, NA, NA, NA)
  # last_na(x)
}


first_notna <- function(x){
  # identify the first non-NA value in a vector
  #   e.g., for c(1, NA, 2, NA, 5) the 4th element is the last NA
  #   so the function would return 4
  inotnax <- which(!is.na(x)) # get indexes of the NA values of x
  # get the minimum index; inotnax will be empty if there are no not-NA values,
  # which would trigger a warning, so suppress warnings
  fnotna <- suppressWarnings(min(inotnax)) 
  fnotna
  
  # tests:
  # x <- c(1, 2, 3, 4, 5)
  # x <- c(NA, 2, NA, NA, 5)
  # x <- c(1, NA, 2, NA, 5)
  # x <- c(1, 2, NA, NA, NA)
  # first_notna(x)
  
}


# the next two functions examine the tail end of a vector (for example the
# last 20 years of a tax X in state Y) to count the length of an uninterrupted
# run of data at the tail end
#   recent_gez counts the number of consecutive values that are greater
#     or equal to zero at the tail end of a state-tax vector

recent_gez <- function(x){
  # number of consecutive >= zero values at end of vector
  gez <- !is.na(x) & x >= 0 # which elements are >= zero?
  revgez <- rev(gez)  # reverse the vector so we can start from the back
  recent_gez <- cumsum(revgez) == 1:length(revgez) # count consecutive >= zero from back
  sum(recent_gez)
  # tests:
  # x <- c(1, 1, 1, 2, 2, 2, -1, 1, 2, 3)
  # x <- c(1, 2, 3, 4, -1, NA, 1, 2, 3)
  # x <- 1:6
  # recent_gez(x)
}

recent_notna <- function(x){
  # number of consecutive non-NA values at end of vector
  notna <- !is.na(x) # which elements are not NA?
  revnotna <- rev(notna)  # reverse the vector so we can start from the back
  recent_notna <- cumsum(revnotna) == 1:length(revnotna) # count consecutive not NA from back
  sum(recent_notna)
  # tests:
  # x <- c(1, 1, 1, 2, 2, 2, -1, 1, 2, 3)
  # x <- c(1, 2, 3, 4, -1, NA, 1, 2, 3)
  # x <- 1:6
  # recent_notna(x)
}

