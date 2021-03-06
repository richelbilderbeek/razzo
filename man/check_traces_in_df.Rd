% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_traces_in_df.R
\name{check_traces_in_df}
\alias{check_traces_in_df}
\title{Check if all the traces' names are in the names of the data frames}
\usage{
check_traces_in_df(traces_names, df)
}
\arguments{
\item{traces_names}{names of the traces}

\item{df}{a data frame}
}
\description{
Check if all the traces' names are in the names of the data frames
}
\examples{
df <- data.frame(my_name = 3.14)
check_traces_in_df("my_name", df)
}
\author{
Richèl J.C. Bilderbeek
}
