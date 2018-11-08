#' @title Create a temporary folder
#' @description The function removes all the pre-existing temporary folders
#'   and creates a new temporary folder.
#' Mainly used to run tests.
#' @inheritParams default_params_doc
#' @param ... options for \code{tempdir}
#' @return the address of the temporary folder
#' @author Giovanni Laudanno
raz_make_tempdir <- function(...) {

  folder_name <- tempdir(...)
  suppressWarnings(dir.create(folder_name))

  x <- unlink(
    file.path(temp_dir <- dirname(folder_name),
              list.files(temp_dir, pattern = "Rtmp")
    ), recursive = TRUE, force = TRUE
  ); list.files(temp_dir, pattern = "Rtmp")

  rm(folder_name)
  folder_name <- tempdir(...)
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
  testit::assert(dir.exists(folder_name))
  return(folder_name)
}

#' @title Convert a tree into branching times
#' @description Convert a tree into branching times. Differently from the ape's
#'  function, it will keep the multiple events. Since the units are million
#'  years, a precision of 8 means that the approximation goes up to the 8-th
#'  digits. With such approximation we consider events happening within an
#'  interval of 4 days (1 million years / 10^8 = 1 year / 100) as simultaneous.
#' @inheritParams default_params_doc
#' @return the branching times
#' @author Giovanni Laudanno
raz_tree2brts <- function(tree, precision = 8) {

  brts0 <- ape::branching.times(tree)
  brts <- DDD::roundn(brts0, digits = precision)

  brts
}
