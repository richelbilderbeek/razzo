## ----message=FALSE-------------------------------------------------------
devtools::install_github("Giappo/mbd", quiet = TRUE)
if (1 == 2) {
  devtools::install_github("richelbilderbeek/beautier", quiet = TRUE)
  devtools::install_github("richelbilderbeek/tracerer", quiet = TRUE)
  devtools::install_github("richelbilderbeek/beastier", quiet = TRUE)
  devtools::install_github("richelbilderbeek/mauricer", quiet = TRUE)
  devtools::install_github("richelbilderbeek/babette", quiet = TRUE)
}

## ------------------------------------------------------------------------
library(razzo)
library(ggplot2)

## ------------------------------------------------------------------------
super_folder_name <- tempdir()
project_folder_name <- file.path(super_folder_name, "razzo_project") 
dir.create(path = project_folder_name, recursive = TRUE)

## ------------------------------------------------------------------------
parameters_filenames <- raz_create_parameters_files(
  project_folder_name = project_folder_name
)

## ------------------------------------------------------------------------
parameters_filenames <- parameters_filenames[c(1, 2)]
knitr::kable(parameters_filenames)

## ------------------------------------------------------------------------
mbd_tree_filenames <- parameters_filenames
# Create all true trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  mbd_tree_filenames[i] <- raz_create_mbd_tree_file(parameters_filename)
}

## ------------------------------------------------------------------------
ape::plot.phylo(ape::read.tree(file = mbd_tree_filenames[2]))

## ------------------------------------------------------------------------
bd_tree_filenames <- parameters_filenames
# Create all BD twin trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  bd_tree_filenames[i] <- raz_create_bd_tree_file(parameters_filename)
}

## ------------------------------------------------------------------------
ape::plot.phylo(ape::read.tree(file = bd_tree_filenames[2]))

## ------------------------------------------------------------------------
mbd_alignment_filenames <- parameters_filenames
# Create all true trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  mbd_alignment_filenames[i] <- raz_create_mbd_alignment_file(parameters_filename)
}

## ------------------------------------------------------------------------
image(ape::read.FASTA(file = mbd_alignment_filenames[1]))

## ------------------------------------------------------------------------
bd_alignment_filenames <- parameters_filenames
# Create all true trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  bd_alignment_filenames[i] <- raz_create_bd_alignment_file(parameters_filename)
}

## ------------------------------------------------------------------------
image(ape::read.FASTA(file = bd_alignment_filenames[1]))

