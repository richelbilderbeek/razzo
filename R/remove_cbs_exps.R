#' Remove all experiments that have a CBS tree prior
#' @export
remove_cbs_exps <- function(experiments) {

  all_experiments <- experiments
  experiments <- list()


  index <- 1
  for (experiment in all_experiments) {
    cbs_tree_prior_name <- "coalescent_bayesian_skyline"
    testit::assert(cbs_tree_prior_name %in% beautier::get_tree_prior_names())
    if (experiment$inference_model$tree_prior$name != cbs_tree_prior_name) {
      experiments[[index]] <- experiment
      index <- index + 1
    }
  }
  if (length(experiments) == 0) {
    stop(
      "No experiments left after removing all experiments with a CBS tree prior"
    )
  }
  pirouette::check_experiments(experiments)
  experiments
}
