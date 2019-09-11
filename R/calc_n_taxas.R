#' Calculate the number of taxa that trees will have when
#' simulated from the set of MBD parameters.
#'
#' @inheritParams default_params_doc
#' @return a numeric vector of whole numbers,
#'   with as much values as the \code{mbd_paramses},
#'   containing the number of taxa each MBD parameter set will create
#' @author Richel J.C. Bilderbeek
#' @export
calc_n_taxas <- function(mbd_paramses) {
  check_mbd_paramses(mbd_paramses)

  # STUB! These are just rounded off random values
  # Instead, simulate per MBD params set an MBD tree and
  # count the number of taxa
  n_taxa <- round(stats::runif(n = length(mbd_paramses), min = 2, max = 1000))

  # Simple data structure
  testit::assert(length(mbd_paramses) == length(n_taxa))
  n_taxa
}
