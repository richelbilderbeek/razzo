# Is the number of taxa sufficient to publish?
library(dplyr)

all_n_taxa_filenames <- list.files(
  path = "/home/richel/data",
  pattern = "n_taxa.csv",
  recursive = TRUE,
  full.names = TRUE
)
all_n_taxa_filenames

n_taxa_filenames <- purrr::discard(
    stringr::str_match(
    string = all_n_taxa_filenames,
    pattern = ".*/razzo_project_.*"
  )[,1],
  is.na
)
n_taxa_filenames

# Extract the dates in ISO format YYYYMMDD
dates <- stringr::str_match(
  string = n_taxa_filenames,
  pattern = "[:digit:]{8}"
)[,1]
dates

# This is what to arrive at
# index: unique value to specify a simulation run index.
# Usually is the seed. Because I don't care about extracting
# the exact seed, I ust use an index
df <- data.frame(date = NA, index = NA, n_taxa = NA)
df <- data.frame()

# Collect all data frames
for (i in seq_along(n_taxa_filenames)) {
  n_taxa_filename <- n_taxa_filenames[i]
  this_df <- utils::read.csv(n_taxa_filename) %>% select(n_taxa)
  this_df$date <- dates[i]
  this_df$index <- seq(1, nrow(this_df))
  df <- rbind(df, this_df)
}
head(df)

df$data <- as.factor(df$date)
df$index <- as.factor(df$index)

library(ggplot2)
library(dplyr)
library(plyr)

# As a figure
ggplot(data = df, aes(x = n_taxa)) + geom_histogram(binwidth = 10) +
  ggplot2::facet_grid(date ~ .) +
  ggplot2::geom_vline(data = ddply(df, "date", summarize, median_n_taxa = median(n_taxa)), aes(xintercept = median_n_taxa), col = "red") +
  ggplot2::geom_vline(data = ddply(df, "date", summarize, mean_n_taxa = mean(n_taxa)), aes(xintercept = mean_n_taxa), col = "blue") +
  ggtitle("n_taxa per date, median = red, mean = blue") +
  ggsave("~/issue_260.png", width = 7, height = 7)


# As a table
knitr::kable(
  ddply(df, "date", summarize, median_n_taxa = median(n_taxa), mean_n_taxa = mean(n_taxa))
)
