#' Collect the times simulations took to run
#' @inheritParams default_params_doc
#' @return a data frame with columns named 'filename', 'state' and 'cpu_time'
#' @author Richel J.C. Bilderbeek
#' @export
collect_run_times <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  razzo::check_project_folder_name(project_folder_name)

  log_filenames <- list.files(
    path = project_folder_name,
    pattern = "^run_r_cmd_.*\\.log$",
    full.names = TRUE,
    recursive = FALSE
  )
  log_filenames
  if (length(log_filenames) == 0) {
    return(
      data.frame(filename = NA, state = NA, cpu_time = NA)
    )
  }

  df <- data.frame(filename = log_filenames, state = NA, cpu_time = NA)

  for (i in seq_along(log_filenames)) {
    log_filename <- log_filenames[i]
    testit::assert(file.exists(log_filename))
    log_filename_text <- readLines(log_filename)
    tryCatch({
        state <- as.character(
          stats::na.omit(
            stringr::str_match(
              string = log_filename_text,
              pattern = "State.*: (CANCELLED|COMPLETED)"
            )[, 2]
          )
        )
        df$state[i] <- state
        cpu_time <- as.character(
          stats::na.omit(
            stringr::str_match(
              string = log_filename_text,
              pattern = "Used CPU time .*:.*((.-)?..:..:..) "
            )[, 2]
          )
        )
        cpu_time
        df$cpu_time[i] <- cpu_time
      }, error = function(e) {} # nolint ignore if something goes wrong
    )
  }
  df
}
