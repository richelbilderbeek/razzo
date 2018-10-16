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

<<<<<<< HEAD
# Create all parameter files
# parameters_filenames <- raz_create_parameters_files()
# testit::assert("1.csv" %in% parameters_filenames)
# 
# # Create all true trees, true alignments and their twins
# for (parameters_filename in parameters_filenames) {
#   input_filenames <- raz_create_input_files(parameters_filename)
#   # True MBD tree
#   testit::assert("1a.tree" %in% input_filenames)
#   # True MBD alignment
#   testit::assert("1a.fasta" %in% input_filenames)
#   # Twin BD tree
#   testit::assert("1b.tree" %in% input_filenames)
#   # Twin BD alignment
#   testit::assert("1b.fasta" %in% input_filenames)
# }
# 
# # Do the inference
# fasta_filenames <- c("1a.fasta") # Search the folder
# for (fasta_filename in fasta_filenames) 
# {
#   output_filenames <- raz_create_inference_files(fasta_filename)
#   # Posterior trees
#   testit::assert("1a.trees" %in% output_filenames)
#   # Trace of MCMC, to estimate the Effective Sample Sizes
#   testit::assert("1a.log" %in% output_filenames)
#   # Marginal likelihood
#   testit::assert("1a_mar_lik.csv" %in% output_filenames)
# }
# 
# # Create the nLTT distribution
# trees_filenames <- c("1a.trees") # Search the folder
# for (trees_filename in trees_filenames)
# {
#   nltt_filename <- raz_create_nltt_file(trees_filename)
#   testit::assert("1a_nltts.csv" %in% nltt_filename)
# }
# 
# # All files are in place!
=======
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

## ------------------------------------------------------------------------
# TODO: Issue #8: actually create an MBD tree and save it
if (1 == 2) {
  plot(file.path(folder_name, "1", "mbd.tree"))
}

## ------------------------------------------------------------------------
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

# Create the nLTT distribution
trees_filenames <- c("1a.trees") # Search the folder
for (trees_filename in trees_filenames)
{
  nltt_filename <- raz_create_nltt_file(trees_filename)
  testit::assert("1a_nltts.csv" %in% nltt_filename)
}

# All files are in place!
>>>>>>> 785f01b06c2d81fc9b10363f8ade087bd728482e

## ------------------------------------------------------------------------
if (1 == 2) {
  BD_pars <- DDD::bd_ML(brts = abs(sim$brts),
                        cond = 2,
                        initparsopt = MBD_pars[1:2],
                        idparsopt = 1:2,
                        missnumspec = 0,
                        tdmodel = 0,
                        btorph = 1,
                        soc = soc)
  
  # BD_pars2 <- DDD::bd_ML(brts = abs(MBDsim$brts),
  #                       cond = 1,
  #                       initparsopt = simpars[1:2],
  #                       idparsopt = 1:2,
  #                       missnumspec = 0,
  #                       tdmodel = 0,
  #                       btorph = 1,
  #                       soc = soc)
}

## ----simulate_twin_bd_tree-----------------------------------------------
<<<<<<< HEAD
set.seed(seed)
BD_tree0 <- TESS::tess.sim.taxa.age(n = 1,
                                    lambda = as.numeric(unname(BD_pars[1])),
                                    mu     = as.numeric(unname(BD_pars[2])),
                                    nTaxa = ((soc - 1) + length(MBD_sim$brts)),
                                    age = age,
                                    MRCA = TRUE)[[1]]

BD_brts <- ape::branching.times(BD_tree0)
BD_l_matrix <- MBD_l_matrix
alive <- BD_l_matrix[, 4] == -1
alive2 <- alive
t <- length(alive); while (sum(alive2) > length(BD_brts)) {if (alive2[t] == 1) {alive2[t] = 0}; t = t - 1}
vec <- BD_l_matrix[, 1]
vec[seq_along(vec) * alive2] <- BD_brts
BD_l_matrix[, 1] <- vec
par(mfrow = c(1,2))
BD_tree <- DDD:::L2phylo(BD_l_matrix)
plot(MBD_tree, main = "MBD tree"); plot(BD_tree, main = "twin BD tree")
=======
if (1 == 2) {
  set.seed(seed)
  BD_tree0 <- TESS::tess.sim.taxa.age(n = 1,
                                      lambda = as.numeric(unname(BD_pars[1])),
                                      mu     = as.numeric(unname(BD_pars[2])),
                                      nTaxa = ((soc - 1) + length(MBD_sim$brts)),
                                      age = age,
                                      MRCA = TRUE)[[1]]
  
  BD_brts <- ape::branching.times(BD_tree0)
  BD_l_matrix <- MBD_l_matrix
  alive <- BD_l_matrix[, 4] == -1
  alive2 <- alive
  t <- length(alive); while (sum(alive2) > length(BD_brts)) {if (alive2[t] == 1) {alive2[t] = 0}; t = t - 1}
  vec <- BD_l_matrix[, 1]
  vec[seq_along(vec) * alive2] <- BD_brts
  BD_l_matrix[, 1] <- vec
  par(mfrow = c(1,2))
  BD_tree <- DDD:::L2phylo(BD_l_matrix)
  plot(MBD_tree, main = "MBD tree"); plot(BD_tree, main = "twin BD tree")
}

## ------------------------------------------------------------------------
if (1 == 2) {
  MBD_sequence_length <- 1e3;
  MBD_mutation_rate   <- 1e-3;
  MBD_chain_length    <- 1e6;
  MBD_sample_interval <- 1e3;
  set.seed(seed)
  MBD_posterior <- pirouette::pir_run( #test1 is a posterior of 10001 trees generated from sim_tes[[1]]
    phylogeny = MBD_tree,
    sequence_length = MBD_sequence_length,
    mutation_rate = MBD_mutation_rate,
    mcmc = beautier::create_mcmc(chain_length = MBD_chain_length, store_every = MBD_sample_interval), #store_every = -1
    site_models = beautier::create_jc69_site_model(),
    clock_models = beautier::create_strict_clock_model(),
    tree_priors = beautier::create_bd_tree_prior(),
    mrca_distr = beautier::create_normal_distr(mean  = beautier::create_mean_param(value = age), 
                                               sigma = beautier::create_sigma_param(value = 0.001)),
    alignment_rng_seed = 0,
    beast2_rng_seed = 1,
    verbose = FALSE
  )
}

## ------------------------------------------------------------------------
if (1 == 2) {
  BD_sequence_length <- MBD_sequence_length;
  BD_mutation_rate   <- MBD_mutation_rate * (sum(MBD_tree$edge.length)/sum(BD_tree$edge.length));
  BD_chain_length    <- MBD_chain_length;
  BD_sample_interval <- MBD_sample_interval;
  set.seed(seed)
  BD_posterior <- pirouette::pir_run( #test1 is a posterior of 10001 trees generated from sim_tes[[1]]
    phylogeny = BD_tree,
    sequence_length = BD_sequence_length,
    mutation_rate = BD_mutation_rate,
    mcmc = beautier::create_mcmc(chain_length = BD_chain_length, store_every = BD_sample_interval), #store_every = -1
    site_models = beautier::create_jc69_site_model(),
    clock_models = beautier::create_strict_clock_model(),
    tree_priors = beautier::create_bd_tree_prior(),
    mrca_distr = beautier::create_normal_distr(mean = beautier::create_mean_param(value = age), sigma = beautier::create_sigma_param(value = 0.001)),
    alignment_rng_seed = 0,
    beast2_rng_seed = 1,
    verbose = FALSE
  )
}

## ------------------------------------------------------------------------
if (1 == 2) {
  MBD_nLTT.diff <- rep(NA, length(MBD_posterior$trees))
  for (i in 1:length(MBD_posterior$trees))
  {
    MBD_nLTT.diff[i] <- nLTT::nLTTstat(MBD_tree, MBD_posterior$trees[[i]])
  }
  MBD_mean.nLTT <- mean(MBD_nLTT.diff)
  MBD_std.nLTT  <- sqrt(stats::var(MBD_nLTT.diff)); MBD_std.nLTT
  MBD_df.nLTT   <- data.frame(diff = MBD_nLTT.diff)
  
  BD_nLTT.diff <- rep(NA, length(BD_posterior$trees))
  for (i in 1:length(BD_posterior$trees))
  {
    BD_nLTT.diff[i] <- nLTT::nLTTstat(BD_tree, BD_posterior$trees[[i]])
  }
  BD_mean.nLTT <- mean(BD_nLTT.diff)
  BD_std.nLTT  <- sqrt(stats::var(BD_nLTT.diff)); BD_std.nLTT
  BD_df.nLTT   <- data.frame(diff = BD_nLTT.diff)
  
  par(mfrow = c(1,2))
  hist(unlist(MBD_df.nLTT), main = "MBD nLTT")
  hist(unlist(BD_df.nLTT), main = "BD nLTT")
  cat("Average nLTT for MBD", MBD_mean.nLTT, "\nAverage nLTT for BD ", BD_mean.nLTT)
}
>>>>>>> 785f01b06c2d81fc9b10363f8ade087bd728482e

