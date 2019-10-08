# Check different epsilons in Nested MCMC runs
#
# https://github.com/richelbilderbeek/razzo/issues/294
#
# Method:
#
# For different epsilons and number of particles,
# estimate the marginal likelihood of one model
#
# Because we use the same alignment, there is one
# best estimate. The estimate with more particles
# will be closer.

library(mcbette)
library(ggplot2)

inference_model <- mcbette::create_test_ns_inference_model()
beast2_options <- mcbette::create_mcbette_beast2_options(rng_seed = 42)

epsilons <- c(0.01, 1.0, 100.0)
n_particleses <- c(1, 2)

df <- expand.grid(epsilons, n_particleses)
names(df) <- c("epsilon", "n_particles")

for (i in seq(1, nrow(df))) {
  inference_model$mcmc$epsilon <- df$epsilon[i]
  inference_model$mcmc$particle_count <- df$n_particles[i]

  evidence <- mcbette::est_marg_lik(
    fasta_filename = beautier::get_beautier_path("anthus_aco_sub.fas"),
    inference_model = inference_model,
    beast2_options = beast2_options
  )
  df$marg_log_lik[i] <- evidence$marg_log_lik
  df$marg_log_lik_sd[i] <- evidence$marg_log_lik_sd
}
df

