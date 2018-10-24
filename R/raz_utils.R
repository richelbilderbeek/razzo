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
