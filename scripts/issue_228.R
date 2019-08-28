zip_filename <- file.path(tempdir(), "razzo_project.zip")
curl::curl_download(
  url = "http://richelbilderbeek.nl/razzo_project_20190801.zip",
  destfile = zip_filename,
  quiet = FALSE # This download will take some time
)

project_folder_name <- file.path(dirname(zip_filename), "razzo_project")
dir.create(project_folder_name, recursive = TRUE)

utils::unzip(zipfile = zip_filename, exdir = project_folder_name)

df <- collect_esses(
  project_folder_name = project_folder_name
)
