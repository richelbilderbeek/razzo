#' Determines parameters checks
#' @param name name of the parameter
#' @param value value of the parameter
#' @return parameter checks
#' @author Giovanni Laudanno
#' @noRd
raz_get_param_checks <- function(name, value) {
  out <- 1
  if (name == "lambda") {
    if (!
        (out <- value > 0)
    ) {
      stop("lambda has to be positive!")
    }
  }
  if (name == "mu") {
    if (!
        (out <- value >= 0)
    ) {
      stop("mu has to be non negative!")
    }
  }
  if (name == "nu") {
    if (!
        (out <- value >= 0)
    ) {
      stop("mu has to be non negative!")
    }
  }
  if (name == "q") {
    if (!
        (out <- value >= 0 && value <= 1)
    ) {
      stop("q has to be between zero and one!")
    }
  }
  if (name == "seed") {
    if (!
        (out <- floor(value) == ceiling(value))
    ) {
      stop("seed must be an integer!")
    }
  }
  if (name == "cond") {
    if (!
        (out <- value == 0 || value == 1)
    ) {
      stop("cond must be either TRUE or FALSE!")
    }
  }
  if (name == "age") {
    if (!
        (out <- value >= 0)
    ) {
      stop("age has to be non negative!")
    }
  }
  if (name == "soc") {
    if (!
        (out <- value == 1 || value == 2)
    ) {
      stop("soc has to be either 1 (stem) or 2 (crown)!")
    }
  }
  if (name == "sequence_length") {
    if (!
        (out <- floor(value) == ceiling(value) && value > 0)
    ) {
      stop("sequence_length has to be a positive integer number!")
    }
  }
  if (name == "mbd_mutation_rate") {
    if (!
        (out <- value > 0)
    ) {
      stop("mbd_mutation_rate has to be a positive number!")
    }
  }
  if (name == "sample_interval") {
    if (!
        (out <- value > 0)
    ) {
      stop("sample_interval has to be a positive number!")
    }
  }
  if (name == "sample_interval") {
    if (!
        (out <- value > 0)
    ) {
      stop("sample_interval has to be a positive number!")
    }
  }
  if (name == "chain_length") {
    if (!
        (out <- value > 0)
    ) {
      stop("chain_length has to be a positive number!")
    }
  }
  if (name == "sub_chain_length") {
    if (!
        (out <- value > 0)
    ) {
      stop("sub_chain_length has to be a positive number!")
    }
  }

  if (!
      (out <- out * raz_is_param_name(name))
  ) { stop(paste0(name, " is not a correct parameter name for this model!"))
  }
  if (!
      (out <- out * is.numeric(value))
  ) {
    stop(paste0(name, " must be numeric!"))
  }
  out
}

#' Determines if the name is a valid parameter name
#' @param name the name to be tested
#' @return TRUE if the name is a valid parameter name, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @noRd
raz_is_param_name <- function(name) {
  name %in% raz_get_param_names() # nolint internal function
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
    "sequence_length",
    "mbd_mutation_rate",
    "sample_interval",
    "chain_length",
    "sub_chain_length"
  )
}

#' General function to create a parameter.
#' @inheritParams default_params_doc
#' @param name name of the parameter
#' @param value value of the parameter
#' @param ... other aspects of more complex parameters (never
#'   applies within this package yet)
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
#'   \code{\link{create_param_sequence_length}},
#'   \code{\link{create_param_mbd_mutation_rate}}
#'   and \code{\link{create_param_sample_interval}}
#' @return a parameter
#' @author Giovanni Laudanno and Richel J.C. Bilderbeek
#' @export
raz_create_param <- function(
  name,
  value,
  ...
) {
  if (!raz_is_param_name(name)) {
    parameters_as_string <- function() {
      s <- NULL
      for (p in raz_get_param_names()) {
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
    value = value,
    ...
  )
  testit::assert(raz_get_param_checks(
    name = parameter$name,
    value = parameter$value) == TRUE
  )
  parameter
}

#' Create a parameter called lambda
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called lambda
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_lambda <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "lambda",
    value = value
  )
}

#' Create a parameter called mu
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called mu
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_mu <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "mu",
    value = value
  )
}

#' Create a parameter called nu
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called nu
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_nu <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "nu",
    value = value
  )
  testit::assert(out$value >= 0)
  out
}

#' Create a parameter called q
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called q
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_q <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "q",
    value = value
  )
}

#' Create a parameter called seed
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called seed
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_seed <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "seed",
    value = value
  )
}

#' Create a parameter called cond
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called cond
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_cond <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "cond",
    value = value
  )
  out
}

#' Create a parameter called age
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called age
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_age <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "age",
    value = value
  )
  out
}

#' Create a parameter called soc
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called soc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_soc <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "soc",
    value = value
  )
  out
}

#' Create a parameter called sequence_length
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called sequence_length
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_sequence_length <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "sequence_length",
    value = value
  )
  out
}

#' Create a parameter called mbd_mutation_rate
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called mbd_mutation_rate
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_mbd_mutation_rate <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "mbd_mutation_rate",
    value = value
  )
}

#' Create a parameter called sample_interval
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called sample_interval
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_sample_interval <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "sample_interval",
    value = value
  )
}

#' Create a parameter called chain_length
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called chain_length
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_chain_length <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "chain_length",
    value = value
  )
}
#' Create a parameter called sub_chain_length
#' @inheritParams default_params_doc
#' @param value value of the parameter
#' @return a parameter called sub_chain_length
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_param_sub_chain_length <- function(
  value = 0.0
) {
  out <- razzo::raz_create_param(
    name = "sub_chain_length",
    value = value
  )
}
