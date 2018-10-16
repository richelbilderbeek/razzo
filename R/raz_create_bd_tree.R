#' Create an BD twin tree from an MBD tree
#' and save it as a file
#' @inheritParams default_params_doc
#' @return nothing. Will create a file with name in \code{tree_filename}
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_bd_tree <- function(
  init_speciation_rate,
  init_extinction_rate,
  mbd_tree,
  bd_tree_filename
)
{
  if (1 == 2) {
    brts <- ape::branching.times(mbd_tree) # nolint
    bd_pars <- DDD::bd_ML( # nolint
      brts = abs(brts),
      cond = 2,
      initparsopt = c(init_speciation_rate, init_extinction_rate),
      idparsopt = 1:2,
      missnumspec = 0,
      tdmodel = 0,
      btorph = 1,
      soc = soc
    )
    # bd_pars2 <- DDD::bd_ML(brts = abs(MBDsim$brts),
    #                       cond = 1,
    #                       initparsopt = simpars[1:2],
    #                       idparsopt = 1:2,
    #                       missnumspec = 0,
    #                       tdmodel = 0,
    #                       btorph = 1,
    #                       soc = soc)
    set.seed(seed)
    BD_tree0 <- TESS::tess.sim.taxa.age(n = 1,
                                        lambda = as.numeric(unname(bd_pars[1])),
                                        mu     = as.numeric(unname(bd_pars[2])),
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


  # Save the tree to a file
}
