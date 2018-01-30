#' @title Render the process map as a file
#' @description You can save the process map to a file
#' @usage render_pmap_file(p, 
#'    file_name, format = NULL, 
#'    width = NULL, height = NULL,
#'    use_external_dot = FALSE)
#' @param p the process map created by `create_pmap()`
#' @param file_name the file name to be stored
#' @param format the file format, it can be `NULL`(default), `png`, `pdf`, `svg` and `ps`. If it's `NULL`, the format will be guessed from the `file_name` extension.
#' @param width width of the image (optional)
#' @param height height of the image (optional)
#' @param use_external_dot Whether to use external `dot` command to generate the file
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
#' render_pmap_file(p, file_name = "test.svg")
#' ```
#' @importFrom DiagrammeR   export_graph
#' @importFrom tools        file_ext
#' @importFrom stringr      str_to_lower
#' @export
render_pmap_file <- function(p, file_name, format = NULL, width = NULL, height = NULL, use_external_dot = FALSE) {
  supported_format <- c("pdf", "svg", "png", "ps", "dot")
  if (is.null(format)) format <- get_file_type(file_name)

  # Check whether format is supported or not
  if (!format %in% supported_format) {
    stop(paste0("'", format, "' is not supported. Supported format are: ", paste(supported_format, collapse = ", ")))
  }

  if (format == "dot") {
    # Generate dot file
    export_dot(p, file_name)
  } else {
    # render for the rest formats
    if (use_external_dot) {
      # Warning message for big process map
      huge_graph_warning(p, nodes_limit = 100, edges_limit = 500)

      # Use external `dot` command for the graph generation
      dotfile <- tempfile(fileext = ".dot")
      export_dot(p, dotfile)
      external_dot(dotfile, file_name)
      # file.remove(dotfile)
    } else {
      # Warning message for big process map
      huge_graph_warning(p, nodes_limit = 80, edges_limit = 300)

      # Use viz.js and V8 for graph file generation
      DiagrammeR::export_graph(p, file_name = file_name, file_type = format, width = width, height = height)
    }
  }
}
