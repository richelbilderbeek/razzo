## ----setup, results = "hide"---------------------------------------------
if (!require(mbd)) {devtools::install_github("Giappo/mbd")}
if (!require(TESS)) {
  install.packages("TESS", repo = "https://lib.ugent.be/CRAN/")
}
if (!require(pirouette)) {
  devtools::install_github("richelbilderbeek/babette")
  devtools::install_github("richelbilderbeek/pirouette")
}

## ------------------------------------------------------------------------
library(razzo)

## ------------------------------------------------------------------------
folder_name <- tempdir()

## ------------------------------------------------------------------------
parameters_filenames <- raz_create_parameters_files(folder_name)
testit::assert(file.path(folder_name, "1", "parameters.csv") %in% parameters_filenames)

## ------------------------------------------------------------------------
# Create all true trees, true alignments and their twins
for (parameters_filename in parameters_filenames) {
  input_filenames <- raz_create_input_files(parameters_filename)
  # True MBD tree
  testit::assert(file.path(folder_name, "1", "mbd.tree") %in% input_filenames)
  # True MBD alignment
  testit::assert(file.path(folder_name, "1", "mbd.fasta") %in% input_filenames)
  # Twin BD tree
  testit::assert(file.path(folder_name, "1", "bd.tree") %in% input_filenames)
  # Twin BD alignment
  testit::assert(file.path(folder_name, "1", "bd.fasta") %in% input_filenames)
}

