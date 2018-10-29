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
  lambda <- parameters$lambda
  mu <- parameters$mu
  seed <- parameters$seed
  age  <- parameters$crown_age
  soc  <- 2 # Use crown age
  testit::assert(!is.null(lambda))
  testit::assert(!is.null(mu))
  testit::assert(!is.null(seed))
  testit::assert(!is.null(age))
  testit::assert(!is.null(soc))

  mbd_brts     <- abs(ape::branching.times(mbd_tree))
  set.seed(seed)
  { # nolint indeed bracket incorrect, is to scope the sink, which is thanks to DDD
    # Suppress output
    if (rappdirs::app_dir()$os != "win") {
      sink("/dev/null")
    } else {
      sink(rappdirs::user_cache_dir())
    }
    # TODO and NOTE: Issue #32: should cond be 1 or 2?
    bd_pars <- DDD::bd_ML( # nolint
      brts = abs(mbd_brts),
      cond = 2,
      initparsopt = c(lambda, mu),
      idparsopt = 1:2,
      missnumspec = 0,
      tdmodel = 0,
      btorph = 1,
      soc = soc
    )
    sink()
  }
  lambda_bd <- as.numeric(unname(bd_pars[1]))
  mu_bd <- as.numeric(unname(bd_pars[2]))
  testit::assert(!is.null(lambda_bd))
  testit::assert(is.numeric(lambda_bd))
  testit::assert(!is.null(mu_bd))
  testit::assert(is.numeric(mu_bd))

  set.seed(seed)
  bd_tree0 <- TESS::tess.sim.taxa.age(
    n = 1,
    lambda = lambda_bd,
    mu     = mu_bd,
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

  # TODO: Issue #43: DDD::L2phylo gives unclear error
  if (1 == 2) {
    return(DDD::L2phylo(bd_l_matrix))
  }
  # FAKE
  mbd_tree
}
