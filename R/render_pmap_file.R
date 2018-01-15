#' @title Render the process map as a file
#' @description You can save the process map to a file
#' @param p the process map created by `create_pmap()`
#' @param file_name the file name to be stored
#' @param format the file format, it can be `png`(default), `pdf`, `svg` and `ps`.
#' @param width width of the image (optional)
#' @param height height of the image (optional)
#' @details
#' The function depends on V8 engine, so please install `v8` engine support on your platform before use the function.
#'  * For Ubuntu/Debian user, please install `libv8-dev` package;
#'  * For Fedora/RHEL user, please install `v8-devel` package;
#'  * For macOS user, please `brew install v8@3.15`;
#'
#' Example:
#'
#' ```R
#' library(dplyr)
#' library(pmap)
#' eventlog <- generate_eventlog()
#' p <- create_pmap(eventlog)
#' render_pmap_file(p, file_name = "test.svg", format = "svg")
#' ```
#' @importFrom DiagrammeR   export_graph
#' @export
render_pmap_file <- function(p, file_name, format = c("png", "pdf", "svg", "ps"), width = NULL, height = NULL) {
  DiagrammeR::export_graph(p, file_name = file_name, file_type = format, width = width, height = height)
}
