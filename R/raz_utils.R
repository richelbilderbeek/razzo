#' @title Create a temporary folder
#' @description The function removes all the pre-existing temporary folders
#'   and creates a new temporary folder.
#' Mainly used to run tests.
#' @inheritParams default_params_doc
#' @param ... options for \code{tempdir}
#' @return the address of the temporary folder
#' @author Giovanni Laudanno
raz_make_tempdir <- function(...) {
  tempdir(...)
}
