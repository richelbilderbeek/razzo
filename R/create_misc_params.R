#' Create the misc parameters
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
create_misc_params <- function(
    tree_filename,
    mbd_sim_rng_seed
) {
  misc_params <- list(
    tree_filename = tree_filename,
    mbd_sim_rng_seed = mbd_sim_rng_seed
  )
  check_misc_params(misc_params) # nolint razzo function
  misc_params
}
