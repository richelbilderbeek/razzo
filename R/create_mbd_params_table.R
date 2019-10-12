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
  lambdas = c(0.2),
  mus = get_razzo_mus(),
  nus = get_razzo_nus(),
  qs = get_razzo_qs(),
  cond = 1,
  crown_age = get_razzo_crown_age(),
  n_replicates = get_razzo_n_replicates()
) {

  x <- expand.grid(lambda = lambdas, mu = mus, nu = nus, q = qs)
  no_mbd_lines <-
    apply(X = x, MARGIN = 1, FUN = function(y) y[3] == 0 | y[4] == 0)
  no_mbd_x <- expand.grid(lambda = lambdas, mu = mus, nu = 0, q = 0)
  x[no_mbd_lines, ] <- no_mbd_x
  x <- dplyr::distinct(x)
  x$crown_age <- crown_age
  x$cond <- cond
  x2 <- x[rep(1:nrow(x), rep(n_replicates, nrow(x))), ]
  x2$seed <- 1:nrow(x2)
  rownames(x2) <- NULL
  x2
}
