#' Measure number of taxa and mb-ness for each parameter setting
#' @inheritParams default_params_doc
#' @return a dataframe with parameter settings, a sample of number of taxa and
#'  a sample of mb-ness (in percentage and absolute).
#' @author Giovanni Laudanno
#' @export
measure_taxa_and_mbness <- function(n_replicates = 1e4) {
  n_0 <- 2
  x <- create_mbd_params_table(n_replicates = n_replicates)
  x <- x %>% dplyr::distinct()
  percentage_mb_species <- n_mb_species <- n_taxas <- rep(NA, nrow(x))
  for (m in 1:nrow(x)) {
    pars <- x[m, ]
    brts <- mbd::mbd_sim(
      pars = c(pars$lambda, pars$mu, pars$nu, pars$q),
      n_0 = n_0,
      age = pars$crown_age,
      cond = pars$cond,
      seed = pars$seed
    )$brts
    n_mb_species[m] <- mbd::count_n_mb_events(brts)
    percentage_mb_species[m] <- mbd::count_percentage_mb_events(brts)
    n_taxas[m] <- length(brts) + n_0 - 1
    if (
      is.nan(n_taxas[m]) ||
      is.nan(n_mb_species[m]) ||
      is.nan(percentage_mb_species[m])
    ) {stop("")}
  }
  x$n_taxas <- n_taxas
  x$n_mb_species <- n_mb_species
  x$percentage_mb_species <- percentage_mb_species
  x$setting <-
    interaction(x$lambda, x$mu, x$nu, x$q, x$crown_age, x$cond, sep = "-")
  x
}
