ensure_str <- function(x) {
  # Taking the Python string representation, but bupaR does not like ' in names
  # TODO fix in bupaR
  if (is.null(x)) {
    NA_character_
  } else {
    stringr::str_remove_all(reticulate::py_str(as_py_value(x)), stringr::fixed("'"))
  }
}

as_py_value <- function(x) {

  if (inherits(x, "python.builtin.object"))
    x
  else
    r_to_py(x)
}
