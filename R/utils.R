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

# define material design palette base colors
MATERIAL_DESIGN_PALETTE <- c(
  "blue",         "red",          "green",        "yellow",   "brown",
  "indigo",       "pink",         "teal",         "amber",    "orange",
  "purple",       "deep-orange",  "light-green",  "lime",     "grey",
  "deep-purple",  "cyan",         "blue-grey",    "light-blue"
)

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

#' @importFrom ggsci  pal_material
get_colors <- function(types) {
  if (length(types) < 1) {
    return(data.frame())
  }

  # Get base colors from ggsci's material design color palette
  colors <- sapply(
    sapply(
      MATERIAL_DESIGN_PALETTE[1:min(length(types), length(MATERIAL_DESIGN_PALETTE))],
      ggsci::pal_material,
      reverse = TRUE
    ),
    function(x) { x(1) }
  )

  color1 <- sapply(colors, get_color_variant, 1)
  color2 <- sapply(colors, get_color_variant, 0.5)
  color3 <- sapply(colors, get_color_variant, 0.3)

  data.frame(
    type = types,
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
