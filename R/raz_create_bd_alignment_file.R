#' @title Create the twin BD alignment from a parameters file in the same folder
#' @description Create the twin BD alignment
#'   from a parameters file in the same folder
#' as that parameters file.
#' For example, for a path '/my_folder/parameters.csv', this function creates:
#' '/my_folder/bd.fasta'.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_bd_alignment_file <- function(
  parameters_filename
) {
  testit::assert(file.exists(parameters_filename))
  bd_tree_filename <- file.path(dirname(parameters_filename), "bd.tree")
  testit::assert(file.exists(bd_tree_filename))

  alignment <- raz_create_bd_alignment(
    parameters = raz_open_parameters_file(parameters_filename),
    bd_tree = ape::read.tree(bd_tree_filename)
  )

  # Save the alignment
  bd_alignment_filename <- file.path(dirname(parameters_filename), "bd.fasta")
  ape::write.FASTA(
    x = alignment,
    file = bd_alignment_filename
  )
  bd_alignment_filename
}
