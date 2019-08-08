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
df <- read.csv(file.path(razzo_project_folder, "results", "esses.csv"))
n_ess_low <- sum(df$ess_likelihood < 200)
n_ess_ok <- sum(df$ess_likelihood >= 200)
# Fraction of ESS that is low
f_low <- n_ess_low / n_ess_ok
print(f_low) # 45% is low!
# Say 5% low ESS is fine
f_low_threshold <- 0.05
testit::assert(f_low < f_low_threshold)
