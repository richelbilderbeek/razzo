nLTT_test <- function(seed = 1,
                      MBD_pars = c(0.2, 0.15, 2, 0.10),
                      soc = 2,
                      age = 10,
                      cond = 1,
                      MBD_sequence_length = 1e3,
                      MBD_mutation_rate   = 1e-3,
                      MBD_chain_length    = 1e6,
                      MBD_sample_interval = 1e3
                      ) {
if (!require(mbd)) {devtools::install_github("Giappo/mbd")}
if (!require(TESS)) {install.packages("TESS")}
# rm(list = ls()); seed <- 3
# step 1: simulate MBD tree
# MBD_pars <- c(0.2, 0.15, 2, 0.10); soc <- 2; age <- 10; cond <- 1
set.seed(seed); print(seed)
MBD_sim  <- mbd::mbd_sim(pars = MBD_pars, soc = soc, age = age, cond = cond)
MBD_brts <- MBD_sim$brts
MBD_tree <- MBD_sim$tes
MBD_l_matrix <- MBD_sim$l_matrix

#step 2: calculate the best pars for the twin BD tree (be careful with the estimated parameters!)
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

#step 3: simulate the twin BD tree
#(check if the method to keep the topology is right. from the figures it seems okish. check with small tree)
set.seed(seed)
BD_tree0 <- TESS::tess.sim.taxa.age(n = 1,
                                    lambda = as.numeric(unname(BD_pars[1])),
                                    mu     = as.numeric(unname(BD_pars[2])),
                                    nTaxa = ((soc - 1) + length(MBD_sim$brts)),
                                    age = age,
                                    MRCA = TRUE)[[1]]

BD_brts <- branching.times(BD_tree0)
BD_l_matrix <- MBD_l_matrix
alive <- BD_l_matrix[, 4] == -1
alive2 <- alive
t <- length(alive); while (sum(alive2) > length(BD_brts)) {if (alive2[t] == 1) {alive2[t] = 0}; t = t - 1}
vec <- BD_l_matrix[, 1]

vec[seq_along(vec) * alive2] <- BD_brts
BD_l_matrix[, 1] <- vec
par(mfrow = c(1,2))
BD_tree <- DDD:::L2phylo(BD_l_matrix)
# plot(MBD_tree); plot(BD_tree)

#step 4: generate MBD posterior, given BD prior
# MBD_sequence_length <- 1e3;
# MBD_mutation_rate   <- 1e-3;
# MBD_chain_length    <- 1e6;
# MBD_sample_interval <- 1e3;
set.seed(seed)
MBD_posterior <- pirouette::pir_run( #test1 is a posterior of 10001 trees generated from sim_tes[[1]]
  phylogeny = MBD_tree,
  sequence_length = MBD_sequence_length,
  mutation_rate = MBD_mutation_rate,
  mcmc = beautier::create_mcmc(chain_length = MBD_chain_length, store_every = MBD_sample_interval), #store_every = -1
  site_models = beautier::create_jc69_site_model(),
  clock_models = beautier::create_strict_clock_model(),
  tree_priors = beautier::create_bd_tree_prior(),
  mrca_distr = beautier::create_normal_distr(mean = beautier::create_mean_param(value = age), sigma = beautier::create_sigma_param(value = 0.001)),
  alignment_rng_seed = 0,
  beast2_rng_seed = 1,
  verbose = FALSE,
  beast_jar_path = beastier::get_default_beast2_jar_path()
)

#step 5: generate BD posterior, given BD prior
#(check BD_substitution rate, see Rampal's email)
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
  verbose = FALSE,
  beast_jar_path = beastier::get_default_beast2_jar_path()
)

# step 6: calculating nLTT statistics for MBD
MBD_nLTT.diff <- rep(NA, length(MBD_posterior$trees))
for (i in 1:length(MBD_posterior$trees))
{
  MBD_nLTT.diff[i] <- nLTT::nLTTstat(MBD_tree, MBD_posterior$trees[[i]])
}
MBD_mean.nLTT <- mean(MBD_nLTT.diff)
MBD_std.nLTT  <- sqrt(stats::var(MBD_nLTT.diff)); MBD_std.nLTT
MBD_df.nLTT   <- data.frame(diff = MBD_nLTT.diff)

# step 7: calculating nLTT statistics for BD
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
cat("Average nLTT for MBD", MBD_mean.nLTT)
cat("Average nLTT for BD ", BD_mean.nLTT )
return(list(MBD_nLTT = unlist(MBD_df.nLTT),
            BD_nLTT = unlist(BD_df.nLTT)))
}

BDout <- MBDout <- vector("list", maxs <- 100)
for (s in 1:maxs)
{
  test <- nLTT_test(seed = s)
  BDout[[s]]  <- test$BD_nLTT
  MBDout[[s]] <- test$MBD_nLTT
}
par(mfrow = c(1,2))
hist(unlist(MBDout), main = "MBD nLTT")
hist(unlist(BDout), main = "BD nLTT")
