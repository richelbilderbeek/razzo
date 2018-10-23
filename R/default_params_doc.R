#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param bd_mutation_rate the mutation rate when creating an alignment
#'   from a BD tree
#' @param bd_tree_filename name of the file that stores a BD twin tree
#' @param chain_length something
#' @param fasta_filename name of a FASTA file
#' @param folder_name name of the main folder
#' @param init_speciation_rate a speciation rate
#' @param init_extinction_rate an extinction rate
#' @param iterations something
#' @param k something
#' @param lambda per-lineage speciation rate
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
#' @param bd_mutation_rate the mutation rate when creating an alignment
#'   from a BD tree
#' @param mbd_tree an MBD tree
#' @param methode something
#' @param minimum_multiple_births something
#' @param missnumspec something
#' @param mu something
#' @param mutation_rate something
#' @param nu something
#' @param optimmethod something
#' @param parameters the razzo parameters
#' @param parameter_filename full path to a 'parameters.csv' file
#' @param parameters_filename full path to a 'parameters.csv' file
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
#' @param pars_transform something
#' @param parsfix The values of the parameters that should not be optimized.
#' @param precision something
#' @param print_errors something
#' @param q something
#' @param reltol something
#' @param res something
#' @param s something
#' @param sample_interval something
#' @param sequence_length something
#' @param sim_pars something
#' @param sim_phylo something
#' @param sub_chain_length length of the sub-chain used by the Nested Sampling
#'   algorithm to estimate the marginal likelihood
#' @param t something
#' @param t1 something
#' @param t2 something
#' @param time_interval something
#' @param tol something
#' @param transition_matrix something
#' @param tree_filename name of the phylogeny file
#' @param trees_filename name of the BEAST2 posterior phylogenies file
#' @param trparsfix something
#' @param trparsopt something
#' @param verbose give more output
#' @param age The age of the tree.
#' @param cond Set 1 if you want to condition on stem or crown age
#'   and non-extinction of the phylogeny. Set 0 otherwise.
#' @param soc Sets whether stem or crown age should be used (1 or 2).
#' @param tips_interval Sets tips boundaries constrain on simulated dataset.
#' @author Documentation by Giovanni Laudanno,
#'   use of this function by Richel J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
#'
default_params_doc <- function(
  bd_mutation_rate,
  bd_tree_filename,
  chain_length,
  fasta_filename,
  folder_name,
  init_speciation_rate,
  init_extinction_rate,
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
  mbd_mutation_rate,
  mbd_tree,
  methode,
  minimum_multiple_births,
  missnumspec,
  mu,
  mutation_rate,
  nu,
  optimmethod,
  parameters,
  parameter_filename,
  parameters_filename,
  pars_transform,
  parsfix,
  precision,
  print_errors,
  q,
  reltol,
  res,
  s,
  sample_interval,
  sequence_length,
  sim_pars,
  sim_phylo,
  soc,
  sub_chain_length,
  t,
  t1,
  t2,
  time_interval,
  tol,
  transition_matrix,
  tree_filename,
  trees_filename,
  trparsfix,
  trparsopt,
  verbose,
  age,
  cond,
  pars,
  tips_interval
) {
  # Nothing
}
