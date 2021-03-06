% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_razzo_paramses.R
\name{create_razzo_paramses}
\alias{create_razzo_paramses}
\title{Create the parameters for all experiment}
\usage{
create_razzo_paramses(
  project_folder_name,
  mbd_paramses = razzo::create_mbd_paramses(),
  error_measure_params = pirouette::create_error_measure_params()
)
}
\arguments{
\item{project_folder_name}{project folder name,
will be the full path to \code{razzo_project}}

\item{mbd_paramses}{data frame with one MBD parameter set per row.
If a simulation with one MBD parameter set has 10 replicates,
this data frame will hold 10 rows, each with different seeds}

\item{error_measure_params}{parameters to set how the error
between given tree and inferred trees in measure,
as can be created by \code{\link[pirouette]{create_error_measure_params}}}
}
\value{
a set of \code{razzo_params}
}
\description{
Create the parameters for all experiment
}
\author{
Richèl J.C. Bilderbeek, Giovanni Laudanno
}
