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

# Create all parameter files
parameters_filenames <- raz_create_parameters_files()
testit::assert("1.csv" %in% parameters_filenames)

# Create all true trees, true alignments and their twins
for (parameters_filename in parameters_filenames) {
  input_filenames <- raz_create_input_files(parameters_filename)
  # True MBD tree
  testit::assert("1a.tree" %in% input_filenames)
  # True MBD alignment
  testit::assert("1a.fasta" %in% input_filenames)
  # Twin BD tree
  testit::assert("1b.tree" %in% input_filenames)
  # Twin BD alignment
  testit::assert("1b.fasta" %in% input_filenames)
}

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

## ------------------------------------------------------------------------
seed <- 1

## ------------------------------------------------------------------------
MBD_pars <- c(0.2, 0.15, 2, 0.10)
soc <- 2
age <- 10
cond <- 1
set.seed(seed)
MBD_sim  <- mbd::mbd_sim(pars = MBD_pars, soc = soc, age = age, cond = cond)
MBD_brts <- MBD_sim$brts
MBD_tree <- MBD_sim$tes
MBD_l_matrix <- MBD_sim$l_matrix
plot(MBD_tree)

## ------------------------------------------------------------------------
BD_pars <- DDD::bd_ML(brts = abs(MBD_sim$brts),
                      cond = 2,
                      initparsopt = MBD_pars[1:2],
                      idparsopt = 1:2,
                      missnumspec = 0,
                      tdmodel = 0,
                      btorph = 1,
                      soc = soc); BD_pars

# BD_pars2 <- DDD::bd_ML(brts = abs(MBDsim$brts),
#                       cond = 1,
#                       initparsopt = simpars[1:2],
#                       idparsopt = 1:2,
#                       missnumspec = 0,
#                       tdmodel = 0,
#                       btorph = 1,
#                       soc = soc)


## ----simulate_twin_bd_tree-----------------------------------------------
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

