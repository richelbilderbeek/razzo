#' Check the misc parameters
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
check_misc_params <- function(
  misc_params
) {
  argument_names <- c(
    "tree_filename"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(misc_params)) {
      stop(
        "'", arg_name, "' must be an element of a 'misc_params'"
      )
    }
  }
  testit::assert(is.character(misc_params$tree_filename))
  testit::assert(is_pff(misc_params$tree_filename))
}
