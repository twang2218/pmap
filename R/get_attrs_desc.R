
#' @title Get an attribute key-pair string from an object
#' @param object Given object, can be `list`, `matrix`, or `data.frame`
#' @description Get an attribute list string from an object
#' @usage get_attrs_desc(object)
#' @export
get_attrs_desc <- function(object) {
  cls <- class(object)

  # Get attribute names
  #   `names()` can only support `list` and `data.frame`;
  #   and `colnames()` cannot support `list`.
  attrs.name <- NULL
  if (cls == "list") {
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
  if (cls == "matrix") {
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
  if (cls == "list") {
    paste(attrs, collapse = "\n")
  } else {
    apply(attrs, 1, paste, collapse = "\n")
  }
}
