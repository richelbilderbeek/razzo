#' Determines parameters checks
#' @return parameter checks
#' @author Giovanni Laudanno
#' @noRd
raz_get_param_checks <- function(name, value) {
  out <- 1
  if (name == 'lambda') {
    if (!
        (out <- value > 0)
    ) {stop('lambda has to be positive!')}
  }
  if (name == 'mu') {
    if (!
        (out <- value >= 0)
    ){stop('mu has to be non negative!')}
  }
  if (name == 'nu') {
    if (!
        (out <- value >= 0)
    ){stop('mu has to be non negative!')}
  }
  if (name == 'q') {
    if (!
        (out <- value >= 0 && value <= 1)
    ){stop('q has to be between zero and one!')}
  }
  if (name == 'seed') {
    if (!
        (floor(value) == ceiling(value))
    ){stop('seed must be an integer!')}
  }
  if (name == 'cond') {
    if (!
        (value == 0 || value == 1)
    ){stop('cond must be either TRUE or FALSE!')}
  }
  if (name == 'age') {
    if (!
        (value >= 0)
    ){stop('age has to be non negative!')}
  }
  if (name == 'soc') {
    if (!
        (value == 1 || value == 2)
    ){stop('soc has to be either 1 (stem) or 2 (crown)!')}
  }
  if (name == 'sequence_length') {
    if (!
        (floor(value) == ceiling(value) && value > 0)
    ){stop('sequence_length has to be a positive integer number!')}
  }
  if (!
      (out <- out * razzo:::raz_is_param_name(name))
  ){stop(paste0(name, " is not a correct parameter name for this model!"))}
  if (!
      (out <- out * is.numeric(value))
  ){stop(paste0(name, " must be numeric!"))}
  out
}

#' Determines if the name is a valid parameter name
#' @param name the name to be tested
#' @return TRUE if the name is a valid parameter name, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @noRd
raz_is_param_name <- function(name) {
  name %in% razzo:::raz_get_param_names() # nolint internal function
}

#' Get the parameter names
#' @return the parameter names
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @noRd
raz_get_param_names <- function() {
  c(
    "lambda",
    "mu",
    "nu",
    "q",
    "seed",
    "cond",
    "age",
    "soc",
    "sequence_length"
  )
}

#' General function to create a parameter.
#' @inheritParams default_parameters_doc
#' @note Prefer using the
#'   named functions
#'   \code{\link{create_param_lambda}},
#'   \code{\link{create_param_mu}},
#'   \code{\link{create_param_nu}},
#'   \code{\link{create_param_q}},
#'   \code{\link{create_param_seed}},
#'   \code{\link{create_param_cond}},
#'   \code{\link{create_param_age}},
#'   \code{\link{create_param_soc}},
#'   and \code{\link{create_param_sequence_length}}
#' @return a parameter
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_param <- function(
  name,
  id,
  value,
  ...
) {
  if (!razzo:::raz_is_param_name(name)) {
    parameters_as_string <- function() {
      s <- NULL
      for (p in razzo:::raz_get_param_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid parameter name, must be one these: ",
      parameters_as_string()
    )
  }
  parameter <- list(
    name = name,
    id = id,
    value = value,
    ...
  )
  testit::assert(razzo:::raz_get_param_checks(name, value))
  parameter
}

#' Create a parameter called lambda
#' @inheritParams default_parameters_doc
#' @return a parameter called lambda
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_lambda <- function(
  id = NA,
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "lambda",
    id = id,
    value = value
  )
}

#' Create a parameter called mu
#' @inheritParams default_parameters_doc
#' @return a parameter called mu
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_mu <- function(
  id = NA,
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "mu",
    id = id,
    value = value
  )
}

#' Create a parameter called nu
#' @inheritParams default_parameters_doc
#' @return a parameter called nu
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_nu <- function(
  id = NA,
  value = 0.0
) {
  razzo::raz_create_param(
    name = "nu",
    id = id,
    value = value
  )
  testit::assert(out$value >= 0)
  out
}

#' Create a parameter called q
#' @inheritParams default_parameters_doc
#' @return a parameter called q
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_q <- function(
  id = NA,
  value = 0.0
) {
  razzo::raz_create_param(
    name = "q",
    id = id,
    value = value
  )
}

#' Create a parameter called seed
#' @inheritParams default_parameters_doc
#' @return a parameter called seed
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_lambda <- function(
  id = NA,
  value = 0.0
) {
  razzo::raz_create_param(
    name = "seed",
    id = id,
    value = value
  )
}

#' Create a parameter called cond
#' @inheritParams default_parameters_doc
#' @return a parameter called cond
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_lambda <- function(
  id = cond,
  value = 0.0
) {
  razzo::raz_create_param(
    name = "cond",
    id = id,
    value = value
  )
}

#' Create a parameter called age
#' @inheritParams default_parameters_doc
#' @return a parameter called age
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_lambda <- function(
  id = NA,
  value = 0.0
) {
  razzo::raz_create_param(
    name = "age",
    id = id,
    value = value
  )
}

#' Create a parameter called soc
#' @inheritParams default_parameters_doc
#' @return a parameter called soc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_lambda <- function(
  id = NA,
  value = 0.0
) {
  razzo::raz_create_param(
    name = "soc",
    id = id,
    value = value
  )
}

#' Create a parameter called sequence_length
#' @inheritParams default_parameters_doc
#' @return a parameter called sequence_length
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_lambda <- function(
  id = NA,
  value = 0.0
) {
  razzo::raz_create_param(
    name = "sequence_length",
    id = id,
    value = value
  )
}
