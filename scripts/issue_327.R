# For https://github.com/richelbilderbeek/razzo/issues/327
#
# Check ESSes versus MBD params
#
# Usage:
#
#   Rscript issue_317.R
#
data_folder <- "/media/richel/D2B40C93B40C7BEB/"
scripts_folder <- "/home/richel/GitHubs/razzo_project"

# ONLY USE RUNS THAT HAVE A MAX ESS OF 1000,
# that is, that sample only 901 times
# TRUE makes sense for the created CSV
# FALSE makes sense for the created PNG
do_low_n_esses_only <- FALSE

library(testthat)
library(peregrine)

if (!dir.exists(data_folder)) {
  stop("Data folder '", data_folder,"' not found")
}
if (!dir.exists(scripts_folder)) {
  stop("Scripts folder '", scripts_folder,"' not found")
}

expect_true(dir.exists(data_folder))
expect_true(dir.exists(scripts_folder))

mbd_params_filenames <- list.files(
  path = data_folder,
  pattern = "mbd_params.csv",
  full.names = TRUE,
  recursive = TRUE
)
expect_true(length(mbd_params_filenames) > 0)

esses_filenames <- list.files(
  path = data_folder,
  pattern = "esses.csv",
  full.names = TRUE,
  recursive = TRUE
)
expect_true(length(esses_filenames) > 0)

# Should be an equal amount of files ...
expect_equal(length(mbd_params_filenames), length(esses_filenames))
# ... with razzo_project_[date]'s in the same order
expect_equal(
  dirname(dirname(mbd_params_filenames)),
  dirname(dirname(esses_filenames))
)

df <- NULL

for (i in seq_along(mbd_params_filenames)) {
  message(i)
  mbd_params_filename <- mbd_params_filenames[i]
  esses_filename <- esses_filenames[i]

  # One mbd param per folder
  mbd_params <- utils::read.csv(mbd_params_filename, stringsAsFactors = FALSE)

  # Four ESSes per folder and mbd setting
  esses <- utils::read.csv(esses_filename, stringsAsFactors = FALSE)

  if (do_low_n_esses_only) {
    if (any(esses$ess_likelihood > 1000)) next
  }

  # Outer join on folder
  this_df <- merge(mbd_params, esses, by = "folder")
  expect_equal(nrow(this_df), nrow(esses))

  if (is.null(df)) df <- this_df else df <- rbind(df, this_df)
}

write.csv(x = df, file = "~/GitHubs/razzo_pilot_results/mbd_vs_ess.csv")
cat(
  knitr::kable(df, format = "markdown"),
  sep = "\n",
  file = "~/GitHubs/razzo_pilot_results/mbd_vs_ess.md"
)

library(ggplot2)

ggplot(df, aes(x = nu, y = ess_likelihood, color = as.factor(q))) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = mean(df[ df$q == 0.0,  ]$ess_likelihood), col = "red") +
  ggtitle("horizontal line is mean for q == 0.0") +
  ggsave("~/issue_327.png")

