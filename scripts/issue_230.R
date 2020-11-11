zip_filename <- file.path(tempdir(), "razzo_project.zip")
curl::curl_download(
  url = "http://richelbilderbeek.nl/razzo_project_20190808.zip",
  destfile = zip_filename,
  quiet = FALSE # This download will take some time
)

razzo_project_folder <- file.path(dirname(zip_filename), "razzo_project")
dir.create(razzo_project_folder, recursive = TRUE)

utils::unzip(zipfile = zip_filename, exdir = razzo_project_folder)

# Read the ESSes
# df <- read.csv(file.path(razzo_project_folder, "results", "esses.csv"))
df <- collect_results(razzo_project_folder)
n_ess_low <- sum(df$ess_likelihood < 200)
n_ess_ok <- sum(df$ess_likelihood >= 200)
# Fraction of ESS that is low
f_low <- n_ess_low / n_ess_ok
message(f_low) # 45% is low!
# Say 5% low ESS is fine
f_low_threshold <- 0.05
testit::assert(f_low < f_low_threshold)

# Investigate
library(magrittr); library(ggplot2)
folder <-
variables <- c(
  "n_taxa",
  "n_mutations",
  "mu",
  "nu",
  "q",
  "nltt_means",
  "nltt_sd"
)

# numeric
df2 <- df %>% dplyr::select(ess_likelihood, variables) %>%
  tidyr::gather(variable, value, -ess_likelihood) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(q = 0.82 * (max(value) - min(value)) + min(value)) %>%
  dplyr::mutate(corr2 = cor(ess_likelihood, value))


p1 <- ggplot(df2, aes(x = value, y = ess_likelihood, group = variable)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = 'lm', formula = y ~ x)  +
  facet_wrap(
    ~ variable,
    scales = "free",
    ncol = ceiling(sqrt(length(variables))),
    nrow = ceiling(sqrt(length(variables))),
    strip.position = "bottom"
  ) +
  theme(strip.placement = "outside", strip.background = element_blank()) +
  labs(y = "ess_likelihood") +
  geom_text(
    data = dplyr::distinct(df2, variable, corr2, q),
    aes(
      x = q, y = 750,
      label = glue::glue("R = {signif(corr2, 2)}")
    ),
    size = 5
  )
p1

### chars
variables <- c(
  "tree",
  "site_model",
  "tree_prior"
)

df3 <- df %>% dplyr::select(ess_likelihood, variables) %>%
  tidyr::gather(variable, value, -ess_likelihood) %>%
  dplyr::group_by(variable)

p2 <- ggplot2::ggplot(data = df3, aes(x = value, y = ess_likelihood)) +
  ggplot2::geom_boxplot(data = df3, aes(x = value, y = ess_likelihood)) +
  ggplot2::facet_wrap(~ variable, scales = "free")
p2
