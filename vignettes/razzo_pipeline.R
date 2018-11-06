## ----message=FALSE-------------------------------------------------------
if (1 == 2) {
  devtools::install_github("Giappo/mbd", 
    ref = "d1b85b44cbcc28c20376708d8b6dee87298fa43d", quiet = TRUE
  )
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

## ------------------------------------------------------------------------
mbd_alignment_filenames <- list()
for (i in seq_along(parameters_filenames)) {

  if (rappdirs::app_dir()$os != "win") {
    # Do the inference
    mbd_alignment_filenames[[i]] <- raz_create_mbd_posterior_files(
      parameters_filenames[i]
    )
  } else {
    # Use fakes, Nested Sampling does not work under Windows
    mbd_alignment_filenames[[i]] <- c(
      raz_get_path("mbd.trees"),
      raz_get_path("mbd.log"),
      raz_get_path("mbd_mar_log_lik.csv")
    )
  }
  # Posterior trees
  testit::assert(any(stringr::str_detect(mbd_alignment_filenames[[i]], ".*/mbd\\.trees")))
  # Trace of MCMC, to estimate the Effective Sample Sizes
  testit::assert(any(stringr::str_detect(mbd_alignment_filenames[[i]], ".*/mbd\\.log")))
  # Marginal likelihood
  testit::assert(any(stringr::str_detect(mbd_alignment_filenames[[i]], ".*/mbd_mar_log_lik\\.csv")))
}

## ------------------------------------------------------------------------
babette::plot_densitree(ape::read.tree(mbd_alignment_filenames[[1]][1]))

## ------------------------------------------------------------------------
knitr::kable(
  tracerer::calc_esses(
    # -1 to drop first column
    utils::read.csv(mbd_alignment_filenames[[1]][2])[-1], 
    sample_interval = raz_open_parameters_file(
      parameters_filename[1]
    )$sample_interval
  )
)

## ------------------------------------------------------------------------
bd_alignment_filenames <- list()
for (i in seq_along(parameters_filenames)) {

  if (rappdirs::app_dir()$os != "win") {
    # Do the inference
    bd_alignment_filenames[[i]] <- raz_create_bd_posterior_files(
      parameters_filenames[i]
    )
  } else {
    # Use fakes, Nested Sampling does not work under Windows
    bd_alignment_filenames[[i]] <- c(
      raz_get_path("bd.trees"),
      raz_get_path("bd.log"),
      raz_get_path("bd_mar_log_lik.csv")
    )
  }
  # Posterior trees
  testit::assert(any(stringr::str_detect(bd_alignment_filenames[[i]], ".*/bd\\.trees")))
  # Trace of MCMC, to estimate the Effective Sample Sizes
  testit::assert(any(stringr::str_detect(bd_alignment_filenames[[i]], ".*/bd\\.log")))
  # Marginal likelihood
  testit::assert(any(stringr::str_detect(bd_alignment_filenames[[i]], ".*/bd_mar_log_lik\\.csv")))
}

## ------------------------------------------------------------------------
babette::plot_densitree(ape::read.tree(bd_alignment_filenames[[1]][1]))

## ------------------------------------------------------------------------
knitr::kable(
  tracerer::calc_esses(
    # -1 to drop first column
    utils::read.csv(bd_alignment_filenames[[1]][2])[-1], 
    sample_interval = raz_open_parameters_file(
      parameters_filename[1]
    )$sample_interval
  )
)

## ------------------------------------------------------------------------
mbd_nltt_filenames <- rep(NA, length(parameters_filenames))
for (i in seq_along(parameters_filenames)) {
  mbd_nltt_filenames[i] <- raz_create_mbd_nltts_file(
    parameters_filenames[i]
  )
}

## ------------------------------------------------------------------------
ggplot(
  data = data.frame(nltt = utils::read.csv(mbd_nltt_filenames[1])$x),
  aes(x = nltt)
) + geom_histogram(binwidth = 0.01) + 
  ggplot2::scale_x_continuous(limits = c(0.0, 1.0))

## ------------------------------------------------------------------------
bd_nltt_filenames <- rep(NA, length(parameters_filenames))
for (i in seq_along(parameters_filenames)) {
  bd_nltt_filenames[i] <- raz_create_bd_nltts_file(
    parameters_filenames[i]
  )
}

## ------------------------------------------------------------------------
ggplot(
  data = data.frame(nltt = utils::read.csv(bd_nltt_filenames[1])$x),
  aes(x = nltt)
) + geom_histogram(binwidth = 0.01) + 
  ggplot2::scale_x_continuous(limits = c(0.0, 1.0))

