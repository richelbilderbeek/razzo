#' @title Create a temporary folder
#' @description The function removes all the pre-existing temporary folders
#'   and creates a new temporary folder.
#' Mainly used to run tests.
#' @inheritParams default_params_doc
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
  folder_name <- tempdir(...) #folder_name <- tempdir()
  if (!dir.exists(folder_name)) {dir.create(folder_name)}
  testit::assert(dir.exists(folder_name))
  return(folder_name)
}

#' @title Create standard parameters interval for tests
#' @description Create standard parameters interval to test the function "raz_create_parameters_files".
#' It saves the dataset in the data folder of the package.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Giovanni Laudanno
raz_save_standard_test_parameters <- function() {
  lambda.interval <- c(0.2, 0.2)
  mu.interval   <- c(0.15, 0.15)
  nu.interval   <- seq(from = 1, to = 2.5, by = 0.5)
  q.interval    <- seq(from = 0.10, to = 0.20, by = 0.05)
  seed.interval <- 1:3
  soc  <- 2
  age  <- 10
  cond <- 1
  sequence_length <- 10^3
  mbd_mutation_rate <- 10^3
  sample_interval <- 10^3
  chain_length <- 10^6
  sub_chain_length <- 10^3

  datafile_name <- file.path(getwd(), 'data', 'standard_test_parameters.RData')
  if (file.exists(datafile_name)) {file.remove(datafile_name)}

  save(
    lambda.interval,
    mu.interval,
    nu.interval,
    q.interval,
    seed.interval,
    soc,
    age,
    cond,
    sequence_length,
    mbd_mutation_rate,
    sample_interval,
    chain_length,
    sub_chain_length,
    file = datafile_name
  )

}

#' @title Load standard parameters interval for tests
#' @description Load standard parameters interval to test the function "raz_create_parameters_files".
#' It loads the dataset in the data folder of the package.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Giovanni Laudanno
raz_load_standard_test_parameters <- function() {
  data(standard_test_parameters)
}

#' @title Get standard parameters names for tests
#' @description Get standard parameters names for tests
#' @inheritParams default_params_doc
#' @return nothing
#' @author Giovanni Laudanno
raz_get_standard_test_filenames <- function() {
    raz_load_standard_test_parameters()
    filenames <- razzo::raz_create_parameters_files(folder_name = folder_name,
                                                  lambda.interval = lambda.interval,
                                                  mu.interval = mu.interval,
                                                  nu.interval = nu.interval,
                                                  q.interval = q.interval,
                                                  seed.interval = seed.interval,
                                                  soc = soc,
                                                  age = age,
                                                  cond = cond,
                                                  sequence_length = sequence_length,
                                                  mbd_mutation_rate = mbd_mutation_rate,
                                                  sample_interval = sample_interval,
                                                  chain_length = chain_length,
                                                  sub_chain_length = sub_chain_length)

    filenames
}




# raz_source <- function() {
#   razzo_scripts <- file.path(getwd(),
#                              list.files(file.path(getwd(), "R"))
#   )
#   # source(razzo_scripts[2])
#   # sapply(razzo_scripts[1], source, .GlobalEnv)
# }
