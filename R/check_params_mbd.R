#' Create mbd parameters
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @aliases check_mbd_params check_params_mbd
#' @export check_mbd_params check_params_mbd
check_mbd_params <- check_params_mbd <- function(
  mbd_params
) {
  if (!is.numeric(mbd_params$lambda)) {
    stop("'lambda' must be numeric")
  } else {
    if (mbd_params$lambda < 0.0) {
      stop("'lambda' must be positive")
    }
  }
  if (!is.numeric(mbd_params$mu)) {
    stop("'mu' must be numeric")
  } else {
    if (mbd_params$mu < 0.0) {
      stop("'mu' must be positive")
    }
  }
  if (!is.numeric(mbd_params$nu)) {
    stop("'nu' must be numeric")
  } else {
    if (mbd_params$nu < 0.0) {
      stop("'nu' must be positive")
    }
  }
  if (!is.numeric(mbd_params$q)) {
    stop("'q' must be numeric")
  } else {
    if (mbd_params$q < 0.0) {
      stop("'q' must be positive")
    }
    if (mbd_params$q > 1.0) {
      stop("'q' must be less or equal to 1.0")
    }
  }
  if (!is.numeric(mbd_params$cond)) {
    stop("'cond' must be numeric")
  }
  if (mbd_params$crown_age < 0) {
    stop("'crown_age' must be positive")
  }
  if (!is.na(mbd_params$seed)) {
    if (!is.numeric(mbd_params$seed)) {
      stop("'seed' must be integer or NA")
    } else {
      if (!(mbd_params$seed %% 1 == 0)) {
        stop("'seed' must be integer or NA")
      }
    }
  }
}
