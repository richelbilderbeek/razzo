#' Create an BD twin tree from an MBD tree
#' and save it as a file
#' @inheritParams default_params_doc
#' @return The twin BD tree obtained from the corresponding MBD tree. It will create a file with name \code{bd.tree} in the folder relative to the chosen parameters
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_bd_tree <- function(
  parameters,
  folder_name,
  verbose = FALSE
)
{
  parameters_folder <- raz_get_parameters_path(parameters, folder_name)
  mbd_tree_filename <- razzo::raz_create_filename_mbd_tree(parameters, folder_name)
  if (!file.exists(mbd_tree_filename)) {
    stop("There is no mbd.tree for these parameters. Create it first!")
  }
  mbd_tree <- get(load(mbd_tree_filename))
  seed <- parameters$seed
  age  <- parameters$age
  soc  <- parameters$soc
  cond <- parameters$cond

  mbd_brts     <- abs(mbd_tree$brts)
  mbd_l_matrix <- mbd_tree$l_matrix
  set.seed(seed)
  bd_pars <- DDD::bd_ML( # nolint
    brts = abs(mbd_brts),
    cond = 2, #cond = 1 #?
    initparsopt = c(parameters$lambda, parameters$mu),
    idparsopt = 1:2,
    missnumspec = 0,
    tdmodel = 0,
    btorph = 1,
    soc = soc
  )

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
  t <- length(alive)
  while (sum(alive2) > length(bd_brts)) {
    if (alive2[t] == 1) {
      alive2[t] = 0
    }
    t = t - 1
  }
  vec <- bd_l_matrix[, 1]
  vec[seq_along(vec) * alive2] <- bd_brts
  bd_l_matrix[, 1] <- vec
  bd_tree <- DDD::L2phylo(bd_l_matrix)

  if (verbose == TRUE) {
    # Show the comparison between the original MBD tree and the twin BD tree
    graphics::par(mfrow = c(1, 2))
    graphics::plot(mbd_tree$tes, main = "MBD tree")
    graphics::plot(bd_tree, main = "twin BD tree")
  }

  # Save the tree to a file
  parameters_folder <- dirname(mbd_tree_filename)
  bd_tree_filename <- razzo::raz_create_filename_bd_tree(parameters, folder_name)
  bd.sim <- list(bd_tree = bd_tree, bd_l_matrix = bd_l_matrix, bd_brts = bd_brts)
  save(bd.sim, file = bd_tree_filename)
  if (!file.exists(bd_tree_filename)) {
    stop("bd.tree has not been created!")
  }

  # Return the tree
  return(bd.sim)
}
