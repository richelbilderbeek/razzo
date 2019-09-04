# Collect the run-times of all razzo_project experiments in ~/data
#
# Usage:
#
# ./issue_262.R
#


# Convert time in HH:MM:SS
time_str_to_n_sec <- function(str) {
  x <- stringr::str_match(str, "((.)-)?(..):(..):(..)")
  n_secs <- as.numeric(x[1, 6])
  n_mins <- as.numeric(x[1, 5])
  n_hours <- as.numeric(x[1, 4])
  n_days <- as.numeric(x[1, 3])
  if (is.na(n_days)) n_days <- 0

  n_hours <- n_hours + (n_days * 24)
  n_mins <- n_mins + (n_hours * 60)
  n_secs <- n_secs + (n_mins * 60)
  n_secs
}

time_str_to_n_sec(str = "00:00:01")
time_str_to_n_sec(str = "4-00:00:01")


run_times_filenames <- list.files(
  path = "~/data",
  pattern = "run_times.csv",
  full.names = TRUE,
  recursive = TRUE
)
run_times_filenames

run_times_filename <- run_times_filenames[1]
cpu_times_str <- as.character(read.csv(run_times_filename)$cpu_time)

(cpu_times)
