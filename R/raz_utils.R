#' @title Create a temporary folder
#' @description The function removes all the pre-existing temporary folders and creates a new temporary folder.
#' Mainly used to run tests.
#' @inheritParams default_params_doc
#' @return the address of the temporary folder
#' @author Giovanni Laudanno
raz_tempdir <- function(...) {

  folder_name <- tempdir(...)
  suppressWarnings(dir.create(folder_name))

  x <- unlink(
    file.path(temp_dir <- dirname(folder_name),
              list.files(temp_dir, pattern = "Rtmp")
    ), recursive = TRUE, force = TRUE
  ); list.files(temp_dir, pattern = "Rtmp")

  rm(folder_name)
  folder_name <- tempdir(...) #folder_name <- tempdir()
  if (!dir.exists(folder_name)) {dir.create(folder_name)}
  testit::assert(dir.exists(folder_name))
  return(folder_name)
}

#' @title Create standard parameters for tests
#' @description Create standard parameters for tests
#' @inheritParams default_params_doc
#' @return the parameters
#' @author Giovanni Laudanno
raz_standard_parameters <- function() {
  parameters <- c(lambda = 0.2,
                  mu = 0.15,
                  nu = 1,
                  q = 0.1,
                  seed = 1,
                  soc = 2,
                  age = 10,
                  cond = 1,
                  sequence_length = 10^3
                  )
  return(parameters)
}

#' @title Create standard parameters interval for tests
#' @description Create standard parameters interval to test the function "raz_create_parameters_files".
#' @inheritParams default_params_doc
#' @return the whole parameters interval
#' @author Giovanni Laudanno
raz_standard_parameters_interval <- function() {
  lambda.interval <<- c(0.2, 0.2)
  mu.interval     <<- c(0.15, 0.15)
  nu.interval     <<- seq(from = 1, to = 2.5, by = 0.5)
  q.interval      <<- seq(from = 0.10, to = 0.20, by = 0.05)
  seed.interval   <<- 1:3
  soc  <<- 2
  age  <<- 10
  cond <<- 1
  sequence_length <<- 10^3
}

# raz_source <- function() {
#   razzo_scripts <- file.path(getwd(),
#                              list.files(file.path(getwd(), "R"))
#   )
#   # source(razzo_scripts[2])
#   # sapply(razzo_scripts[1], source, .GlobalEnv)
# }
