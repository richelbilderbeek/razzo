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
    # TODO: Issue #52: check the quality of inference of lambda and mu provided by bd_ML # nolint
    bd_pars <- DDD::bd_ML( # nolint
      brts = sort(mbd_brts, decreasing = TRUE),
      cond = 2, #conditioning on stem or crown age and on the total number of extant taxa (including missing species) # nolint
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

  # generate bd branching times from the inferred
  # parameters
  set.seed(seed)
  bd_tree0 <- TESS::tess.sim.taxa.age(
    n = 1,
    lambda = lambda_bd,
    mu     = mu_bd,
    nTaxa = ((soc - 1) + length(mbd_brts)),
    age = age,
    MRCA = TRUE
  )[[1]]
  bd_brts0 <- ape::branching.times(bd_tree0)

  bd_tree <- raz_combine_brts_and_topology(
    brts = bd_brts0,
    tree = mbd_tree
  )

  bd_tree
}
