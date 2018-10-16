#' Create an BD twin tree from an MBD tree
#' and save it as a file
#' @inheritParams default_params_doc
#' @return A twin BD tree
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export

raz_create_bd_tree <- function(
  parameters,
  folder_name
)
{
    parameters_folder <- raz_get_parameters_path(parameters, folder_name)
    mbd_tree_filename <- file.path(parameters_folder, list.files(parameters_folder, pattern = "mbd.tree"))
    mbd_tree <- get(load(mbd_tree_filename))
    seed <- parameters$seed
    age <- parameters$age
    soc <- parameters$soc
    cond <- parameters$cond

    mbd_brts <- abs(mbd_tree$brts)
    mbd_l_matrix <- mbd_tree$l_matrix
    set.seed(seed)
    bd_pars <- DDD::bd_ML( # nolint
      brts = abs(mbd_brts),
      cond = 2,
      initparsopt = c(parameters$lambda, parameters$mu),
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
    bd_tree0 <- TESS::tess.sim.taxa.age(n = 1,
                                        lambda = as.numeric(unname(bd_pars[1])),
                                        mu     = as.numeric(unname(bd_pars[2])),
                                        nTaxa = ((soc - 1) + length(mbd_brts)),
                                        age = age,
                                        MRCA = TRUE)[[1]]

    bd_brts <- ape::branching.times(bd_tree0)
    bd_l_matrix <- mbd_l_matrix
    alive <- bd_l_matrix[, 4] == -1
    alive2 <- alive
    t <- length(alive); while (sum(alive2) > length(bd_brts)) {if (alive2[t] == 1) {alive2[t] = 0}; t = t - 1}
    vec <- bd_l_matrix[, 1]
    vec[seq_along(vec) * alive2] <- bd_brts
    bd_l_matrix[, 1] <- vec
    bd_tree <- DDD:::L2phylo(bd_l_matrix)

    # Show the comparison between the original MBD tree and the twin BD tree
    par(mfrow = c(1,2))
    plot(mbd_tree$tes, main = "MBD tree"); plot(bd_tree, main = "twin BD tree")

    # Save the tree to a file
    parameters_folder <- dirname(mbd_tree_filename)
    bd_tree_filename <- paste0(parameters_folder, "/bd.tree")
    bd.sim <- list(bd_tree = bd_tree, bd_l_matrix = bd_l_matrix, bd_brts = bd_brts)
    save(bd.sim, file = bd_tree_filename)

    return(bd.sim)
}
