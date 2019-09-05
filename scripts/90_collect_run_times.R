# Collect the run-times of all razzo_project experiments in ~/data
#
# Usage:
#
# ./90_collect_run_times
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

library(testthat)
expect_equal(time_str_to_n_sec("00:00:11"), 11)
expect_equal(time_str_to_n_sec("00:33:22"), (33 * 60) + 22)
expect_equal(time_str_to_n_sec("12:55:44"), (12 * 60 * 60) + (55 * 60) + 44)
expect_equal(
  time_str_to_n_sec(str = "4-33:22:11"),
  (4 * 24 * 60 * 60) + (33 * 60 * 60) + (22 * 60) + 11
)

# Convert time in HH:MM:SS
time_strs_to_n_secs <- function(strs) {
  n_secs <- rep(NA, length(strs))
  for (i in seq_along(strs)) {
    n_secs[i] <- time_str_to_n_sec(strs[i])
  }
  n_secs
}

expect_silent(time_strs_to_n_secs(c("00:00:01", "01:02:03", "1-02:03:04")))


all_parameter_filenames <- list.files(
  path = "~/data",
  pattern = "parameters.RDa",
  full.names = TRUE,
  recursive = TRUE
)
parameter_filenames <- as.character(
  na.omit(
    stringr::str_match(
      string = all_parameter_filenames,
      pattern = ".*razzo_project.*/1/parameters.RDa"
    )[,1]
  )
)
parameter_filenames

run_times_filenames <- list.files(
  path = "~/data",
  pattern = "run_times.csv",
  full.names = TRUE,
  recursive = TRUE
)
run_times_filenames

df <- data.frame()

for (i in seq_along(run_times_filenames)) {
  run_times_filename <- run_times_filenames[i]
  cpu_times_str <- as.character(read.csv(run_times_filename)$cpu_time)
  cpu_times_n_secs <- time_strs_to_n_secs(cpu_times_str)
  cpu_times_n_secs
  run_date <- stringr::str_match(
    string = run_times_filename,
    "razzo_project_(........)"
  )[1, 2]
  run_date

  this_df <- data.frame(date = run_date,  n_sec = cpu_times_n_secs)
  this_df$i <- seq(1, nrow(this_df))
  df <- rbind(df, this_df)
}

df$date <- as.factor(df$date)

names(df)

library(ggplot2)
library(plyr)

df$n_hour <- df$n_sec / (60 * 60)

ggplot(
  na.omit(df),
  aes(x = n_hour, fill = date)
  ) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  geom_vline(
    data = ddply(na.omit(df), .(date), summarize, mean = mean(n_hour)),
    aes(xintercept = mean, col = date)
  ) + labs(
    title = "Simulation run-times"
  ) + ggsave("~/fig_run_times.png", width = 7, height = 7)


# As table
df_means <- ddply(na.omit(df), .(date), summarize, mean_runtime_hours = mean(n_hour))
df_means

df_means$crown_age <- NA
df_means$n_candidates <- NA
df_means$mcmc_chain_length <- NA
df_means$n_replicates <- NA
df_means$mean_n_taxa <- NA
df_means$mean_ess <- NA


for (i in seq_along(parameter_filenames)) {
  df_means$crown_age[i] <- readRDS(parameter_filenames[i])$mbd_params$crown_age
  df_means$n_candidates[i] <- length(readRDS(parameter_filenames[i])$pir_params$experiments)
  df_means$mcmc_chain_length[i] <- readRDS(parameter_filenames[i])$pir_params$experiments[[1]]$inference_model$mcmc$chain_length
  n_replicates <- length(list.dirs(dirname(dirname(parameter_filenames[i]))[1])) - 1
  df_means$n_replicates[i] <- n_replicates

  # Mean number of taxa
  n_taxa_filename <- file.path(dirname(dirname(dirname(dirname(parameter_filenames[i])))), "results", "n_taxa.csv")
  testit::assert(file.exists(n_taxa_filename))
  df_n_taxa <- read.csv(n_taxa_filename)
  mean_n_taxa <- mean(df_n_taxa$n_taxa)
  df_means$mean_n_taxa[i] <- mean_n_taxa

  # Mean ESS
  esses_filename <- file.path(dirname(dirname(dirname(dirname(parameter_filenames[i])))), "results", "esses.csv")
  #if (!file.exists(esses_filename)) next
  testit::assert(file.exists(esses_filename))
  df_esses <- read.csv(esses_filename)
  mean_ess <- mean(df_esses$ess_likelihood)
  df_means$mean_ess[i] <- mean_ess
}

df_means

knitr::kable(df_means)
