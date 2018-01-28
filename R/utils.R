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

#' @importFrom grDevices rgb
#' @importFrom grDevices colorRamp
get_color_variant <- function(color, level = 1) {
  return(
    grDevices::rgb(
      grDevices::colorRamp(c(color, "white"))(1 - level),
      maxColorValue = 255
    )
  )
}

# Define material design palette base colors
# Use 900 of each color
# Reference: https://material.io/guidelines/style/color.html#color-color-palette
MATERIAL_DESIGN_PALETTE <- c(
  #   blue        red      green     yellow      brown
  "#0D47A1", "#B71C1C", "#1B5E20", "#F57F17", "#3E2723",
  # indigo       pink       teal      amber     orange
  "#1A237E", "#880E4F", "#004D40", "#FF6F00", "#E65100",
  # purple   d-orange    l-green       lime       grey
  "#4A148C", "#BF360C", "#33691E", "#827717", "#212121",
  # d-purple     cyan   blue-grey light-blue
  "#311B92", "#006064", "#263238", "#01579B"
)

get_colors <- function(categories) {
  if (length(categories) < 1) {
    return(data.frame())
  }

  # Get base colors from ggsci's material design color palette
  colors <- MATERIAL_DESIGN_PALETTE[1:min(length(categories), length(MATERIAL_DESIGN_PALETTE))]

  color1 <- sapply(colors, get_color_variant, 1)
  color2 <- sapply(colors, get_color_variant, 0.5)
  color3 <- sapply(colors, get_color_variant, 0.3)

  data.frame(
    category = categories,
    color = color1,
    fillcolor = paste0(color2, ":", color3),
    stringsAsFactors = FALSE
  )
}

# Function for generating random time vector
generate_random_datetimes <- function(size, from = "2017-01-01", to = "2017-12-31") {
  as.POSIXct(
    round(stats::runif(
      size,
      min = as.numeric(as.POSIXct(from)),
      max = as.numeric(as.POSIXct(to))
    )),
    origin = "1970-01-01"
  )
}

#' @importFrom lubridate    as.duration
format_duration <- function(duration) {
  gsub(
    # "966.854166666667s (~16.11 minutes)"  => "16.11 minutes"
    "^.*\\(~?(.+)\\)$",
    "\\1",
    as.character(lubridate::as.duration(duration))
  )
}
