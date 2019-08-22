#' Check all elements in the list of \code{mbd_params}.
#'
#' For example, each element should have a unique MBD parameter setting
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' mbd_paramses <- create_mbd_paramses()
#' expect_silent(check_mbd_paramses(mbd_paramses))
#' @author Richel J.C. Bilderbeek
#' @export
check_mbd_paramses <- function(mbd_paramses) {

  for (mbd_params in mbd_paramses) {
    check_mbd_params(mbd_params)
  }

  for (i in seq(1, length(mbd_paramses) - 1)) {
    testit::assert(i >= 1)
    testit::assert(i <= length(mbd_paramses))
    # lhs: left hand side
    lhs <- mbd_paramses[[i]]
    for (j in seq(i + 1, length(mbd_paramses))) {
      testit::assert(j >= 1)
      testit::assert(j <= length(mbd_paramses))
      testit::assert(j > i)
      # rhs: right hand side
      rhs <- mbd_paramses[[j]]
      # Assume already all seeds are unique
      if (lhs$seed != rhs$seed) next
      stop("All mbd_params in mbd_paramses must be unique")
    }
  }

}
