zip_filename <- file.path(tempdir(), "razzo_project.zip")
curl::curl_download(
  url = "http://richelbilderbeek.nl/razzo_project_20190801.zip",
  destfile = zip_filename,
  quiet = FALSE # This download will take some time
)

razzo_project_folder <- file.path(dirname(zip_filename), "razzo_project")
dir.create(razzo_project_folder, recursive = TRUE)

utils::unzip(zipfile = zip_filename, exdir = razzo_project_folder)

# This takes ages, Issue #228, Issue 228
df <- collect_esses(
  project_folder_name = razzo_project_folder
)

