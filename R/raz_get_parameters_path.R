#' Produces the right path from parameters
#' @inheritParams default_params_doc
#' @return Path
#' @author Giovanni Laudanno
#' @export
raz_get_parameters_path <- function(parameters, folder_name)
{
  path <- paste0(
    folder_name, "/",
    parameters$lambda, "-",
    parameters$mu, "-",
    parameters$nu, "-",
    parameters$q, "/",
    parameters$seed, "/"
  )

  if (!file.exists(path)) {print("There is no folder for this parameter setting!")}

  return(path)
}
