#' Create an BD twin tree from an MBD tree
#' and save it as a file
#' @inheritParams default_params_doc
#' @return a twin BD tree of class \code{phylo},
#'   obtained from the corresponding MBD tree.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_bd_tree <- function(
  parameters,
  mbd_tree,
  mbd_l_matrix
) {
  seed <- parameters$seed
  age  <- parameters$age
  soc  <- parameters$soc

  mbd_brts     <- abs(ape::branching.times(mbd_tree))
  set.seed(seed)
  # TODO and NOTE: should cond be 1 or 2?
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

  set.seed(seed)
  bd_tree0 <- TESS::tess.sim.taxa.age(
    n = 1,
    lambda = as.numeric(unname(bd_pars[1])),
    mu     = as.numeric(unname(bd_pars[2])),
    nTaxa = ((soc - 1) + length(mbd_brts)),
    age = age,
    MRCA = TRUE
  )[[1]]

  bd_brts <- ape::branching.times(bd_tree0)
  bd_l_matrix <- mbd_l_matrix
  alive <- bd_l_matrix[, 4] == -1
  alive2 <- alive
  t <- length(alive)
  while (sum(alive2) > length(bd_brts)) {
    if (alive2[t] == 1) {
      alive2[t] <- 0
    }
    t <- t - 1
  }
  vec <- bd_l_matrix[, 1]
  vec[seq_along(vec) * alive2] <- bd_brts
  bd_l_matrix[, 1] <- vec
  DDD::L2phylo(bd_l_matrix)
}
