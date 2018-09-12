#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param A_abstol something
#' @param A_reltol something
#' @param abstol something
#' @param account_name something
#' @param alpha something
#' @param alpha0 something
#' @param b something
#' @param brts something
#' @param chain_length something
#' @param changeloglikifnoconv something
#' @param colormap something
#' @param idparsfix something
#' @param idparsopt The ids of the parameters that must be optimized. The ids are defined as follows:
#' \itemize{
#' \item id == 1 corresponds to lambda (speciation rate)
#' \item id == 2 corresponds to mu (extinction rate)
#' \item id == 3 corresponds to nu (multiple speciation trigger rate)
#' \item id == 4 corresponds to q (single-lineage speciation probability)
#' }
#' @param initparsopt something
#' @param interval.max something
#' @param interval.min something
#' @param iterations something
#' @param k something
#' @param lambda something
#' @param logs something
#' @param lx It is the number of ODEs considered for the computation.
#' @param lx0 something
#' @param matrix something
#' @param matrix_builder something
#' @param max_iter Sets the maximum number of iterations in the optimization
#' @param max_k something
#' @param max_number_of_species something
#' @param max_repetitions something
#' @param max_sims something
#' @param maxiter something
#' @param mbd.lambda something
#' @param methode something
#' @param minimum_multiple_births something
#' @param missnumspec something
#' @param mu something
#' @param mutation_rate something
#' @param N something
#' @param N0 something
#' @param Nsteps something
#' @param Nsubs something
#' @param nu something
#' @param optimmethod something
#' @param pars vector of parameters:
#' \itemize{
#'   \item pars[1] is the multiple speciation trigger rate;
#'   \item pars[2] is the extinction rate;
#'   \item pars[3] is the single-lineage speciation probability.
#' }
#' or
#' \itemize{
#'   \item pars[1] is lambda, the sympatric speciation rate;
#'   \item pars[2] is mu, the extinction rate;
#'   \item pars[3] is nu, the multiple allopatric speciation trigger rate;
#'   \item pars[4] is q, the single-lineage speciation probability.
#' }
#' @param pars.transform something
#' @param parsfix The values of the parameters that should not be optimized.
#' @param precision something
#' @param print_errors something
#' @param Q something
#' @param q something
#' @param reltol something
#' @param res something
#' @param s something
#' @param sample_interval something
#' @param sequence_length something
#' @param sim_pars something
#' @param sim_phylo something
#' @param t something
#' @param t1 something
#' @param t2 something
#' @param time_interval something
#' @param tol something
#' @param transition_matrix something
#' @param trparsfix something
#' @param trparsopt something
#' @param verbose something
#' @param x something
#' @param x.name something
#' @param x.splits something
#' @param y something
#' @param y.name something
#' @param y.splits something
#' @param z something
#' @param z.name something
#' @param age The age of the tree.
#' @param cond Set 1 if you want to condition on stem or crown age and non-extinction of the phylogeny. Set 0 otherwise.
#' @param soc Sets whether stem or crown age should be used (1 or 2).
#' @param tips_interval Sets tips boundaries constrain on simulated dataset.
#' @author Documentation by Giovanni Laudanno, use of this function by Richel J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
default_params_doc <- function(
  A_abstol,
  A_reltol,
  abstol,
  account_name,
  alpha,
  alpha0,
  b,
  brts,
  chain_length,
  changeloglikifnoconv,
  colormap,
  idparsfix,
  idparsopt,
  initparsopt,
  interval.max,
  interval.min,
  iterations,
  k,
  lambda,
  logs,
  lx,
  lx0,
  matrix,
  matrix_builder,
  max_iter,
  max_k,
  max_number_of_species,
  max_repetitions,
  max_sims,
  maxiter,
  mbd.lambda,
  methode,
  minimum_multiple_births,
  missnumspec,
  mu,
  mutation_rate,
  N,
  N0,
  Nsteps,
  Nsubs,
  nu,
  optimmethod,
  pars.transform,
  parsfix,
  precision,
  print_errors,
  Q,
  q,
  reltol,
  res,
  s,
  sample_interval,
  sequence_length,
  sim_pars,
  sim_phylo,
  soc,
  t,
  t1,
  t2,
  time_interval,
  tol,
  transition_matrix,
  trparsfix,
  trparsopt,
  verbose,
  x,
  x.name,
  x.splits,
  y,
  y.name,
  y.splits,
  z,
  z.name,
  age,
  cond,
  pars,
  tips_interval
) {
  # Nothing
}
