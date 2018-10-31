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

## ------------------------------------------------------------------------
parameters_filenames <- raz_create_parameters_files(
  project_folder_name = project_folder_name
)

## ------------------------------------------------------------------------
parameters_filenames <- parameters_filenames[ c(1, 2) ]
knitr::kable(parameters_filenames)

## ------------------------------------------------------------------------
mbd_tree_filenames <- parameters_filenames
# Create all true trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  mbd_tree_filenames[i] <- raz_create_mbd_tree_file(parameters_filename)
}

## ------------------------------------------------------------------------
ape::plot.phylo(ape::read.tree(file = mbd_tree_filenames[1]))

## ------------------------------------------------------------------------
bd_tree_filenames <- parameters_filenames
# Create all BD twin trees, true alignments and their twins
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  bd_tree_filenames[i] <- raz_create_bd_tree_file(parameters_filename)
}

## ------------------------------------------------------------------------
ape::plot.phylo(ape::read.tree(file = bd_tree_filenames[1]))

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
i <- 1
for (i in seq_along(parameters_filenames)) {
  parameters_filename <- parameters_filenames[i]
  bd_alignment_filenames[i] <- raz_create_bd_alignment_file(parameters_filename)
}

## ------------------------------------------------------------------------
image(ape::read.FASTA(file = bd_alignment_filenames[1]))

## ------------------------------------------------------------------------
if (1 == 2) {
  # Do the inference. It doesn't work on Windows.
  mbd_alignment_filenames <- list() # Search the folder
  for (i in seq_along(parameters_filenames)) {
    parameter_filename <- parameters_filenames[i] 
    mbd_alignment_filenames[[i]] <- raz_create_mbd_posterior_files(
      parameter_filename
    )
    # Posterior trees
    testit::assert("1a.trees" %in% output_filenames)
    # Trace of MCMC, to estimate the Effective Sample Sizes
    testit::assert("1a.log" %in% output_filenames)
    # Marginal likelihood
    testit::assert("1a_mar_lik.csv" %in% output_filenames)
  }
}

## ------------------------------------------------------------------------
if (1 == 2) {
  # Create the nLTT distribution
  trees_filenames <- c("1a.trees") # Search the folder
  for (trees_filename in trees_filenames)
  {
    if (1 == 2) {
      # TODO: Issue 5
      nltt_filename <- raz_create_nltt_file(trees_filename)
      testit::assert("1a_nltts.csv" %in% nltt_filename)
    }
  }
}
# All files are in place!

## ------------------------------------------------------------------------
# TODO: plot the nLTT

## ------------------------------------------------------------------------
if (1 == 2) {
  
  graphics::par(mfrow = c(1, 2))
  hist(unlist(MBD_df.nLTT), main = "MBD nLTT")
  hist(unlist(BD_df.nLTT), main = "BD nLTT")
  cat("Average nLTT for MBD", MBD_mean.nLTT, "\nAverage nLTT for BD ", BD_mean.nLTT)
}

