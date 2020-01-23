#' Create a list of MBD parameters
#'
#' Each element has a unique MBD parameter combination
#' @inheritParams default_params_doc
#' @return a list of \code{mbd_params}
#' @seealso use \link{create_mbd_params} to create one
#'   MBD parameter combination
#' @author Richel J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' mbd_paramses <- create_mbd_paramses()
#' for (mbd_params in mbd_paramses) {
#'   expect_silent(check_mbd_params(mbd_params))
#' }
#' @export
create_mbd_paramses <- function(
  n_replicates = get_razzo_n_replicates()
) {
  issue <- 381

  if (issue != 381) {
    df <- mbd.SimTrees::create_params_table(
      n_replicates = n_replicates
    )
  } else {
    # Dirty hack for https://github.com/richelbilderbeek/razzo/issues/381 :
    # - Select the settings with the strongest MBDness
    # - Use 20 replicates whatsoever
    df <- mbd.SimTrees::create_params_table(
      n_replicates = 20
    )
    df <- df[ df$nu == max(df$nu) & df$q == max(df$q), ]
  }

  n_paramses <- nrow(df)
  testit::assert(n_paramses > 0)
  mbd_paramses <- list()
  for (i in seq(1, n_paramses)) {
    mbd_params <- razzo::create_mbd_params(
      lambda = df$lambda[i],
      mu = df$mu[i],
      nu = df$nu[i],
      q = df$q[i],
      cond = df$cond[i],
      crown_age = df$crown_age[i],
      seed = df$seed[i]
    )
    mbd_paramses[[i]] <- mbd_params
  }
  mbd_paramses
}
