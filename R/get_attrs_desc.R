#' @title Get an attribute key-pair string from an object
#' @usage get_attrs_desc(object)
#' @param object Given object, can be `list`, `matrix`, or `data.frame`
#' @description Get an attribute list string from an object
#' @examples
#' print(df)
#' #   id   name is_manager
#' # 1  1   Jane      FALSE
#' # 2  2   John      FALSE
#' # 3  3   Eric      FALSE
#' # 4  4 Selena       TRUE
#' get_attrs_desc(df)
#' # [1] "id: 1\nname: Jane\nis_manager: FALSE"
#' # [2] "id: 2\nname: John\nis_manager: FALSE"
#' # [3] "id: 3\nname: Eric\nis_manager: FALSE"
#' # [4] "id: 4\nname: Selena\nis_manager: TRUE"
#' @export
get_attrs_desc <- function(object) {
  cls <- class(object)

  # Get attribute names
  #   `names()` can only support `list` and `data.frame`;
  #   and `colnames()` cannot support `list`.
  attrs.name <- NULL
  if (inherits(object, "list")) {
    attrs.name <- names(object)
  } else {
    attrs.name <- colnames(object)
  }

  if (any(is.null(attrs.name)) || any(is.na(attrs.name)) || length(attrs.name) == 0) {
    return("")
  }

  # Get attributes key-pair
  #   `matrix` do not support `object[[name]]`, so it has to be treated specially.
  attrs <- NULL
  if (inherits(object, "matrix")) {
    attrs <- sapply(
      attrs.name,
      function(name) {
        paste0(name, ": ", object[, name])
      }
    )
  } else {
    attrs <- sapply(
      attrs.name,
      function(name) {
        paste0(name, ": ", object[[name]])
      }
    )
  }

  # Combine attributes key-pairs to a single string contains the list
  #   `paste()` is only works for columns oriented direction, so `apply()`
  #   is used for `matrix` and `data.frame`
  if (inherits(attrs, c("matrix", "data.frame"))) {
    apply(attrs, 1, paste, collapse = "\n")
  } else {
    paste(attrs, collapse = "\n")
  }
}
