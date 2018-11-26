## ----message=FALSE, install_dependencies---------------------------------
if (!require(ape)) {install.packages("ape")}
if (!require(mbd)) {devtools::install_github("Giappo/mbd", quiet = TRUE)}
devtools::install_github("richelbilderbeek/beautier", quiet = TRUE)
devtools::install_github("richelbilderbeek/tracerer", quiet = TRUE)
devtools::install_github("richelbilderbeek/beastier", quiet = TRUE)
devtools::install_github("richelbilderbeek/mauricer", quiet = TRUE)
devtools::install_github("richelbilderbeek/babette", quiet = TRUE)

## ----load_libraries------------------------------------------------------
library(mbd)
library(razzo)
library(ggplot2)

## ----create_working_folder-----------------------------------------------
super_folder_name <- tempdir()
project_folder_name <- file.path(super_folder_name, "razzo_project") 
dir.create(path = project_folder_name, recursive = TRUE)

## ----create_parameter_files----------------------------------------------
parameters_filenames <- create_parameters_files(
  project_folder_name = project_folder_name
)

## ------------------------------------------------------------------------
mbd_l_matrix_filenames <- mbd_tree_filenames <- parameters_filenames
# Create all true trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  mbd_tree_filenames[i] <- create_mbd_tree_files(
    parameters_filename
  )$mbd_tree_filename
  mbd_l_matrix_filenames[i] <- create_mbd_tree_files(
    parameters_filename
  )$mbd_l_matrix_filename
}

## ------------------------------------------------------------------------
ape::plot.phylo(ape::read.tree(file = mbd_tree_filenames[2]))

## ------------------------------------------------------------------------
bd_l_matrix_filenames <- bd_tree_filenames <- parameters_filenames
# Create all BD twin trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  bd_tree_filenames[i] <- create_bd_tree_files(
    parameters_filename
  )$bd_tree_filename
  bd_l_matrix_filenames[i] <- create_bd_tree_files(
    parameters_filename
  )$bd_l_matrix_filename
}

## ------------------------------------------------------------------------
ape::plot.phylo(ape::read.tree(file = bd_tree_filenames[2]))

## ------------------------------------------------------------------------
mbd_alignment_filenames <- parameters_filenames
# Create all true trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  mbd_alignment_filenames[i] <- create_mbd_alignment_file(parameters_filename)
}

## ------------------------------------------------------------------------
image(ape::read.FASTA(file = mbd_alignment_filenames[1]))

## ------------------------------------------------------------------------
bd_alignment_filenames <- parameters_filenames
# Create all true trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  bd_alignment_filenames[i] <- create_bd_alignment_file(parameters_filename)
}

## ------------------------------------------------------------------------
image(ape::read.FASTA(file = bd_alignment_filenames[1]))

