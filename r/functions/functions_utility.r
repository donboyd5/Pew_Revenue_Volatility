
# general utility functions ----

savefig <- function(basename, .p=p, .pdata=pdata, width=10, height=6, scale=1) {
  
  ggsave(filename = here::here("results", 
                               paste0(basename, ".png")),
         plot = .p, 
         width=width, height=height,
         scale=scale)
  
  write_csv(.pdata, file= here::here("results", paste0(basename, "_data.csv")))
}

savetab <- function(basename, .tab=tab, .tabdata=tabdata, zoom=2, expand=5) {
  # zoom and expand are passed to the underlying webshot::webshot() function
  # for PNG saving zoom defaults to a scale level of 2 and
  # expand (adds whitespace pixels around cropped table image) has a default value of 5)
  
  # expand: A numeric vector specifying how many pixels to expand the clipping
  # rectangle by. If one number, the rectangle will be expanded by that many
  # pixels on all sides. If four numbers, they specify the top, right, bottom,
  # and left, in that order. When taking screenshots of multiple URLs, this
  # parameter can also be a list with same length as url with each element of
  # the list containing a single number or four numbers to use for the
  # corresponding URL.
  
  # zoom: A number specifying the zoom factor. A zoom factor of 2 will result in
  # twice as many pixels vertically and horizontally. Note that using 2 is not
  # exactly the same as taking a screenshot on a HiDPI (Retina) device: it is
  # like increasing the zoom to 200 doubling the height and width of the browser
  # window. This differs from using a HiDPI device because some web pages load
  # different, higher-resolution images when they know they will be displayed on
  # a HiDPI device (but using zoom will not report that there is a HiDPI
  # device).
  
  gtsave(.tab, here::here("results", paste0(basename, ".png")),
         zoom = zoom, expand = expand)
  
  write_csv(.tabdata, file= here::here("results", paste0(basename, "_data.csv")))
}
