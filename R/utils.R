projection <- function(x, min = 0, max = 1) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  
  # handle the case of all `x` are the same
  if (x_max == x_min || is.null(x) || is.na(x)) {
    # return the middle of expectation
    return((max + min) / 2)
  }

  ((x - x_min)/(x_max - x_min)) * (max - min) + min
}
