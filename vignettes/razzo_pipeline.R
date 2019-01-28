## ----message=FALSE, install_dependencies---------------------------------
if (!require(ape)) {install.packages("ape")}
if (!require(mbd)) {devtools::install_github("Giappo/mbd", quiet = TRUE)}
devtools::install_github("richelbilderbeek/becosys", quiet = TRUE)
devtools::install_github("ropensci/beautier", quiet = TRUE)
devtools::install_github("ropensci/tracerer", quiet = TRUE)
devtools::install_github("ropensci/beastier", quiet = TRUE)
devtools::install_github("ropensci/mauricer", quiet = TRUE)
devtools::install_github("ropensci/babette", quiet = TRUE)
devtools::install_github("richelbilderbeek/pirouette", quiet = TRUE)

## ----load_libraries------------------------------------------------------
library(mbd)
library(razzo)
library(ggplot2)

## ----create_working_folder-----------------------------------------------
super_folder_name <- tempdir()
project_folder_name <- file.path(super_folder_name, "razzo_project") 
dir.create(path = project_folder_name, recursive = TRUE)

