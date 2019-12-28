#' Create a set of testing MBD parameters
#' @author Richel J.C. Bilderbeekc
#' @export
create_test_mbd_paramses <- function(
) {
  mbd_paramses <- list()
  mbd_paramses[[1]] <- razzo::create_test_mbd_params(seed = 1)
  mbd_paramses[[2]] <- razzo::create_test_mbd_params(seed = 2)
  mbd_paramses
}
