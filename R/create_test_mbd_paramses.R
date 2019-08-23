#' @export
create_test_mbd_paramses <- function(
) {
  mbd_paramses <- list()
  mbd_paramses[[1]] <- create_test_mbd_params(seed = 1)
  mbd_paramses[[2]] <- create_test_mbd_params(seed = 2)
  mbd_paramses
}
