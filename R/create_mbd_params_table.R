#' Create a table of which each row holds a unique MBD parameter combination.
#' @inheritParams default_params_doc
#' @seealso use \link{create_mbd_paramses} to create a list
#' of \code{mbd_params}
#' @return a data frame with the following columns:
#' \itemize{
#'   \item lambda the speciation rate
#'   \item mu the extinction rate
#'   \item nu the co-occurring speciation event rate
#'   \item q the proportion of species that speciates at a co-occurring
#'     speciation event
#'   \item crown_age the crown age of the MBD tree
#'   \item cond the conditioning
#'   \item seed the RNG seed for creating an MBD tree
#' }
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' df <- create_mbd_params_table()
#' expect_true(is.data.frame(df))
#' expect_equal(nrow(unique(df)), nrow(df))
#' expect_true("lambda" %in% names(df))
#' expect_true("mu" %in% names(df))
#' expect_true("nu" %in% names(df))
#' expect_true("q" %in% names(df))
#' expect_true("crown_age" %in% names(df))
#' expect_true("seed" %in% names(df))
#' @export
create_mbd_params_table <- function(
  lambda = 0.2,
  mu = c(0, 0.15),
  nu = c(1.0, 1.5, 2.0, 2.5),
  q = c(0.1, 0.15, 0.2),
  n_replicates = 2,
  crown_age = 6.0,
  cond = 1
) {
  lambda <- unique(lambda)
  mu <- unique(mu)
  nu <- unique(nu)
  q <- unique(q)
  seed <- 1:n_replicates
  mbd_params_table <- expand.grid(
    seed = seed,
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q,
    crown_age = crown_age,
    cond = cond
  )
  seeds <- 1:nrow(mbd_params_table)
  mbd_params_table$seed <- NULL
  mbd_params_table$seed <- seeds
  testit::assert(
   length(crown_age) == 1
  )
  testit::assert(
   length(cond) == 1
  )
  mbd_params_table
}
