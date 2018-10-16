#' Create an MBD tree from the razzo parameters
#' and save it as a file
#' @inheritParams default_params_doc
#' @return nothing. Will create a file with name in \code{tree_filename}
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_mbd_tree <- function(parameters, tree_filename)
{
  # Create the MBD tree
  if (1 == 2) {
    MBD_pars <- c(0.2, 0.15, 2, 0.10)
    soc <- 2
    age <- 10
    cond <- 1
    set.seed(seed)
    sim  <- mbd::mbd_sim(pars = MBD_pars, soc = soc, age = age, cond = cond)
    brts <- sim$brts
    tree <- sim$tes
    l_matrix <- sim$l_matrix
    testit::assert(!is.null(brts))
    testit::assert(!is.null(tree))
    testit::assert(!is.null(l_matrix))
  }

  # Save the tree to a file
}
