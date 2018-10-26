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
# TODO: Issue: row names were found from a short variable and have been discarded
if (1 == 2) {
  bd_tree_filenames <- parameters_filenames
  # Create all BD twin trees, true alignments and their twins
  for (i in seq_along(parameters_filenames)) {
    parameters_filename <- parameters_filenames[i]
    bd_tree_filenames[i] <- raz_create_bd_tree_file(parameters_filename)
  }
}

## ------------------------------------------------------------------------
if (1 == 2) {
  ape::plot.phylo(ape::read.tree(file = bd_tree_filenames[1]))
}

## ------------------------------------------------------------------------
if (1 == 2) {
  mbd_alignment_filenames <- parameters_filenames
  # Create all true trees, true alignments and their twins
  for (i in seq_along(parameters_filenames)) {
    parameters_filename <- parameters_filenames[i]
    mbd_alignment_filenames[i] <- raz_create_mbd_alignment_file(parameters_filename)
  }
}

## ------------------------------------------------------------------------
if (1 == 2) {
  image(ape::read.FASTA(file = mbd_alignment_filenames[1]))
}

## ------------------------------------------------------------------------
# TODO: Issue Error in pirouette::sim_alignment(phylogeny = bd_tree, sequence_length = NULL, : phylogeny must not contain extant species
if (1 == 2) {
  bd_alignment_filenames <- parameters_filenames
  # Create all true trees, true alignments and their twins
  for (i in seq_along(parameters_filenames)) {
    i <- 1
    parameters_filename <- parameters_filenames[i]
    bd_alignment_filenames[i] <- raz_create_bd_alignment_file(parameters_filename)
  }
}

## ------------------------------------------------------------------------
if (1 == 2) {
  image(ape::read.FASTA(file = bd_alignment_filenames[1]))
}

## ------------------------------------------------------------------------
if (1 == 2) {
  # Do the inference
  fasta_filenames <- c("1a.fasta") # Search the folder
  for (fasta_filename in fasta_filenames) 
  {
    output_filenames <- raz_create_posterior_files(fasta_filename)
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

