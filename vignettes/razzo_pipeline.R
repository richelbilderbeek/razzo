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

## ----create_mbd_posterior------------------------------------------------
mbd_alignment_filenames <- list()
for (i in seq_along(parameters_filenames)) {
  # Do the inference
  mbd_alignment_filenames[[i]] <- create_mbd_posterior_files(
    parameters_filenames[i]
  )
  # Posterior trees
  testit::assert(any(stringr::str_detect(mbd_alignment_filenames[[i]], ".*/mbd\\.trees")))
  # Trace of MCMC, to estimate the Effective Sample Sizes
  testit::assert(any(stringr::str_detect(mbd_alignment_filenames[[i]], ".*/mbd\\.log")))
}

## ----plot_mbd_densitree--------------------------------------------------
babette::plot_densitree(tracerer::parse_beast_trees(mbd_alignment_filenames[[1]][1]))

## ----show_mbd_esses------------------------------------------------------
knitr::kable(
  tracerer::calc_esses(
    tracerer::parse_beast_log(mbd_alignment_filenames[[1]][2]), 
    sample_interval = open_parameters_file(
      parameters_filename[1]
    )$sample_interval
  )
)

## ----create_bd_posterior_files-------------------------------------------
bd_alignment_filenames <- list()
for (i in seq_along(parameters_filenames)) {
  # Do the inference
  bd_alignment_filenames[[i]] <- create_bd_posterior_files(
    parameters_filenames[i]
  )
  # Posterior trees
  testit::assert(any(stringr::str_detect(bd_alignment_filenames[[i]], ".*/bd\\.trees")))
  # Trace of MCMC, to estimate the Effective Sample Sizes
  testit::assert(any(stringr::str_detect(bd_alignment_filenames[[i]], ".*/bd\\.log")))
}

## ----plot_bd_densitree---------------------------------------------------
babette::plot_densitree(tracerer::parse_beast_trees(bd_alignment_filenames[[1]][1]))

## ----show_bd_esses-------------------------------------------------------
knitr::kable(
  tracerer::calc_esses(
    tracerer::parse_beast_log(bd_alignment_filenames[[1]][2]), 
    sample_interval = open_parameters_file(
      parameters_filename[1]
    )$sample_interval
  )
)

## ----calc_mbd_nltts------------------------------------------------------
mbd_nltt_filenames <- rep(NA, length(parameters_filenames))
for (i in seq_along(parameters_filenames)) {
  mbd_nltt_filenames[i] <- create_mbd_nltts_file(
    parameters_filename = parameters_filenames[i]
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
  bd_nltt_filenames[i] <- create_bd_nltts_file(
    parameters_filenames[i]
  )
}

## ------------------------------------------------------------------------
ggplot(
  data = data.frame(nltt = utils::read.csv(bd_nltt_filenames[1])$x),
  aes(x = nltt)
) + geom_histogram(binwidth = 0.01) + 
  ggplot2::scale_x_continuous(limits = c(0.0, 1.0))

## ----create_mbd_marg_lik_files-------------------------------------------
if (rappdirs::app_dir()$os != "win") {
  mbd_marg_lik_filenames <- rep(NA, length(parameters_filenames))
  for (i in seq_along(parameters_filenames)) {
    mbd_marg_lik_filenames[i] <- create_mbd_marg_lik_file(
      parameters_filename = parameters_filenames[i]
    )
  }
  knitr::kable(utils::read.csv(mbd_marg_lik_filenames[1])[-1])
}

## ----create_bd_marg_lik_files--------------------------------------------
if (rappdirs::app_dir()$os != "win") {
  bd_marg_lik_filenames <- rep(NA, length(parameters_filenames))
  for (i in seq_along(parameters_filenames)) {
    bd_marg_lik_filenames[i] <- create_bd_marg_lik_file(
      parameters_filename = parameters_filenames[i]
    )
  }
  knitr::kable(utils::read.csv(bd_marg_lik_filenames[1])[-1])
}

## ----create_figure_1-----------------------------------------------------
create_fig_1(project_folder_name = project_folder_name)

## ------------------------------------------------------------------------
mbd_nltts <- utils::read.csv(mbd_nltt_filenames[1])$x
bd_nltts <- utils::read.csv(bd_nltt_filenames[1])$x

df <- data.frame(
  model = c(rep("MBD", length(mbd_nltts)), rep("BD", length(bd_nltts))),
  nltt = c(mbd_nltts, bd_nltts),
  stringsAsFactors = TRUE  
)

plot <- ggplot(
  data = df,
  aes(x = nltt, fill = model)
) + ggplot2::scale_x_continuous(limits = c(0.0, 1.0))

plot + geom_histogram(binwidth = 0.01, alpha = 0.5)
plot + geom_density(alpha = 0.5)

## ------------------------------------------------------------------------
esses_filename <- create_esses_file(project_folder_name = project_folder_name)
knitr::kable(utils::read.csv(esses_filename)[-1])

## ------------------------------------------------------------------------
if (rappdirs::app_dir()$os != "win") {
  marg_liks_file <- create_marg_liks_file(
    project_folder_name = project_folder_name
  )
  knitr::kable(utils::read.csv(marg_liks_file)[-1])
}

