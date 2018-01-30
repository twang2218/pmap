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

# Define material design palette (19 colors)
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

# ggsci igv color palette (26 colours)
# Not all of IGV colours have been chosen as the last colours doesn't seems good.
# Reference: https://ggsci.net/reference/pal_igv.html
IGV_PALETTE <- c(
  "#5050FF", "#CE3D32", "#749B58", "#F0E685", "#466983",
  "#BA6338", "#5DB1DD", "#802268", "#6BD76B", "#D595A7",
  "#924822", "#837B8D", "#C75127", "#D58F5C", "#7A65A5",
  "#E4AF69", "#3B1B53", "#CDDEB7", "#612A79", "#AE1F63",
  "#E7C76F", "#5A655E", "#CC9900", "#99CC00", "#A9A9A9",
  "#33CC00"
)

# ggsci npg color palette (10 colours)
# Reference: https://ggsci.net/reference/pal_npg.html
NPG_PALETTE <- c(
  "#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F",
  "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85"
)

# ggsci locuszoom color palette (7 colours)
# Reference: https://ggsci.net/reference/pal_locuszoom.html
LOCUSZOOM_PALETTE <- c(
  "#D43F3A", "#EEA236", "#5CB85C", "#46B8DA", "#357EBD",
  "#9632B8", "#B8B8B8"
)

# ggsci jama color palette (7 colours)
# Reference: https://ggsci.net/reference/pal_jama.html
JAMA_PALETTE <- c(
  "#374E55", "#DF8F44", "#00A1D5", "#B24745", "#79AF97",
  "#6A6599", "#80796B"
)

# ggsci futurama color palette (11 colours)
# Remove "#FF6F00" as it's already in `MATERIAL_DESIGN_PALETTE`
# Reference: https://ggsci.net/reference/pal_futurama.html
FUTURAMA_PALETTE <- c(
  "#C71000", "#008EA0", "#8A4198", "#5A9599", "#FF6348",
  "#84D7E1", "#FF95A8", "#3D3B25", "#ADE2D0", "#1A5354",
  "#3F4041"
)

# 80 colours
PALETTE <- c(
  MATERIAL_DESIGN_PALETTE,
  IGV_PALETTE,
  NPG_PALETTE,
  LOCUSZOOM_PALETTE,
  JAMA_PALETTE,
  FUTURAMA_PALETTE)

get_colors <- function(categories) {
  if (length(categories) < 1) {
    return(data.frame())
  }

  # Get base colors from predefined palette
  colors <- PALETTE[1:min(length(categories), length(PALETTE))]

  # For more colors, we simply assign them to a single color so the function won't fail
  if (length(colors) < length(categories)) {
    # #428BCA is from Bootstrap's color palette
    colors <- c(colors, rep("#428BCA", length(categories) - length(colors)))
  }

  data.frame(
    category = categories,
    color = sapply(colors, get_color_variant, 1),
    fillcolor = sapply(colors, get_color_variant, 0.5),
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

# Get file extension in lower case
#' @importFrom stringr    str_to_lower
#' @importFrom tools      file_ext
get_file_type <- function(file_name) {
  stringr::str_to_lower(tools::file_ext(file_name))
}

# Export graph as a dot file
#' @importFrom DiagrammeR   generate_dot
export_dot <- function(p, file_name) {
  sink(file_name)
  cat(gsub("'", "\"", DiagrammeR::generate_dot(p)))
  sink()
}

# Using external `dot` command to generate the result
external_dot <- function(dotfile, file_name) {
  t <- try(system2("dot", c("-V"), stdout = TRUE, stderr = TRUE), silent = TRUE)
  if (inherits(t, "try-error")) {
    warning("Cannot find `dot` command. GraphViz should be installed to use `dot` command.")
  } else {
    system2(
      "dot",
      c(
        paste0("-T", get_file_type(file_name)),
        paste0("\"", dotfile, "\""),
        "-o",
        paste0("\"", file_name, "\"")
      ),
      stdout = TRUE,
      stderr = TRUE
    )
  }
}

huge_graph_warning <- function(p, nodes_limit = 80, edges_limit = 300) {
  if (nrow(p$nodes_df) > nodes_limit || nrow(p$edges_df) > edges_limit) {
    warning(
      paste0(
        "The process map is big (nodes: ",
        nrow(p$nodes_df),
        ", edges: ",
        nrow(p$edges_df),
        "). It may take too long to render."
      ),
      immediate. = TRUE
    )
  }
}
