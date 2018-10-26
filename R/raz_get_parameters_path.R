#' Produces the right path from parameters
#' @inheritParams default_params_doc
#' @return Path
#' @author Giovanni Laudanno
#' @export
raz_get_parameters_path <- function(
  parameters,
  folder_name
) {
  path <- file.path(
    folder_name,
    "razzo_project",
    paste0(
      parameters$lambda, "-",
      parameters$mu, "-",
      parameters$nu, "-",
      parameters$q
    ),
    parameters$seed#, "parameters.csv"
  )

  if (!file.exists(path)) {
    stop("Cannot find path '", path, "'")
  }

  return(path)
}
