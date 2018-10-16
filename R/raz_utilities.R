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

raz_source <- function() {
  razzo_scripts <- file.path(getwd(),
                             list.files(file.path(getwd(), "R"))
  )
  # source(razzo_scripts[2])
  # sapply(razzo_scripts[1], source, .GlobalEnv)
}
