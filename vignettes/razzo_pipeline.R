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
if (1 == 2) {
  parameters_filenames <- raz_create_parameters_files(folder_name)
  testit::assert(file.path(folder_name, "1", "parameters.csv") %in% parameters_filenames)
}

## ------------------------------------------------------------------------
if (1 == 2) {
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
}

## ------------------------------------------------------------------------
# TODO: Issue #8: actually create an MBD tree and save it
if (1 == 2) {
  graphics::plot(file.path(folder_name, "1", "mbd.tree"))
}

## ------------------------------------------------------------------------
if (1 == 2) {
  graphics::plot(file.path(folder_name, "1", "bd.tree"))
}

## ------------------------------------------------------------------------
if (1 == 2) {
  # Do the inference
  fasta_filenames <- c("1a.fasta") # Search the folder
  for (fasta_filename in fasta_filenames) 
  {
    output_filenames <- raz_create_inference_files(fasta_filename)
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
  
  graphics::par(mfrow = c(1,2))
  hist(unlist(MBD_df.nLTT), main = "MBD nLTT")
  hist(unlist(BD_df.nLTT), main = "BD nLTT")
  cat("Average nLTT for MBD", MBD_mean.nLTT, "\nAverage nLTT for BD ", BD_mean.nLTT)
}

