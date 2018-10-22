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
raz_create_bd_alignment <- function(
  parameters,
  folder_name
) {
  # Create the name of the alignent file, e.g. '/myfolder/bd.fasta'
  bd_alignment_filename <- razzo::raz_create_filename_bd_alignment(
    parameters, folder_name)

  # Get the twin BD phylogeny for 'bd.tree'
  bd_phylogeny <- NULL

  # Get the sequence length from the parameters filename
  # Found
  #   BD_mutation_rate <-  MBD_mutation_rate *                 # nolint
  #     * (sum(MBD_tree$edge.length)/sum(BD_tree$edge.length)) # nolint
  sequence_length <- parameters$sequence_length

  # Calculate the mutation rate from the tree
  mutation_rate <- NULL

  # Root sequence is e.g. AAACCCGGGTTT
  root_sequence <- pirouette::create_blocked_dna(sequence_length)
  root_sequence <- pirouette::create_blocked_dna(20)

  alignment <- pirouette::sim_alignment(
    phylogeny = bd_phylogeny,
    sequence_length = NULL,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate
  )

  # Save the alignment
  ape::write.dna(
    alignment,
    file = bd_alignment_filename,
    format = "fasta"
  )

  testit::assert(file.exists(bd_alignment_filename))
}
