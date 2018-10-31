## ----message=FALSE-------------------------------------------------------
if (1 == 2) {
  devtools::install_github("Giappo/mbd", quiet = TRUE)
  devtools::install_github("richelbilderbeek/beautier", quiet = TRUE)
  devtools::install_github("richelbilderbeek/tracerer", quiet = TRUE)
  devtools::install_github("richelbilderbeek/beastier", quiet = TRUE)
  devtools::install_github("richelbilderbeek/mauricer", quiet = TRUE)
  devtools::install_github("richelbilderbeek/babette", quiet = TRUE)
}

## ------------------------------------------------------------------------
library(razzo)

## ------------------------------------------------------------------------
super_folder_name <- tempdir()
project_folder_name <- file.path(super_folder_name, "razzo_project") 
dir.create(path = project_folder_name, recursive = TRUE)

