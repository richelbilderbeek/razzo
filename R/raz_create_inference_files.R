#' Create the inference files from a FASTA file:
#' \itemize{
#'   \item A a
#'   \item B aqwf
#'   \item C aewf
#' }
#' Assumes for a FASTA file named '1x.fasta'
#'   that there is a parameters file called '1.csv'
#' @return names of the files created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_inference_files <- function(fasta_filename)
{
  base <- tools::file_path_sans_ext(fasta_filename)
  trees_filename <- paste0(c(base, ".trees"), collapse = "")
  log_filename <- paste0(c(base, ".log"), collapse = "")
  mar_lik_filename <- paste0(c(base, "_mar_lik.csv"), collapse = "")
  inference_filenames <- c(
    trees_filename,
    log_filename,
    mar_lik_filename
  )
  inference_filenames
}
