raz_tempdir <- function(...) {

  folder_name <- tempdir(...)
  dir.create(folder_name)

  x <- unlink(
    file.path(temp_dir <- dirname(folder_name),
              list.files(temp_dir, pattern = "Rtmp")
    ), recursive = TRUE, force = TRUE
  ); list.files(temp_dir, pattern = "Rtmp")

  rm(folder_name)
  folder_name <- tempdir(...)
  dir.create(folder_name)
  return(folder_name)
}
