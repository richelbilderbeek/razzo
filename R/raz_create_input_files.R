#' Create the input files from a parameters file
#' @return names of the files created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_input_files <- function(parameters_filename)
{
  base <- tools::file_path_sans_ext(parameters_filename)
  mbd_tree_filename <- paste0(c(base, "a.tree"), collapse = "")
  mbd_alignment_filename <- paste0(c(base, "a.fasta"), collapse = "")
  bd_tree_filename <- paste0(c(base, "b.tree"), collapse = "")
  bd_alignment_filename <- paste0(c(base, "b.fasta"), collapse = "")
  input_filenames <- c(
    mbd_tree_filename,
    mbd_alignment_filename,
    bd_tree_filename,
    bd_alignment_filename
  )
  input_filenames
}
