#' Check the misc parameters
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @aliases check_misc_params check_params_misc
#' @export check_misc_params check_params_misc
check_misc_params <- check_params_misc <- function(
  misc_params
) {
  argument_names <- c(
    "tree_filename", "razzo_version"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(misc_params)) {
      stop(
        "'", arg_name, "' must be an element of a 'misc_params'"
      )
    }
  }
  assertive::assert_is_a_string(misc_params$tree_filename)
  if (!peregrine::is_pff(misc_params$tree_filename)) {
    stop(
      "'misc_params$tree_filename' must be Peregrine-friendly. \n",
      "Actual value: ", misc_params$tree_filename
    )
  }

  if (
      is.na(
      stringr::str_match(
        string = misc_params$tree_filename,
        pattern = "mbd\\.tree"
      )[1, 1]
    )
  ) {
    stop("'misc_params$tree_filename' must end with 'mbd.tree'")
  }
}
