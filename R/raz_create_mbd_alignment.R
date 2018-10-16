#' Create an MBD alignment from a parameters file in the same folder
#' as that parameters file.
#' For example, for a path '/my_folder/parameters.csv', this function creates:
#' '/my_folder/mbd.fasta'.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_mbd_alignment <- function(parameters, folder_name) {

  # parameters <- raz_standard_parameters()

  parameters_folder <- raz_get_parameters_path(parameters, folder_name)
  mbd_tree_filename <- file.path(parameters_folder, "mbd.tree")
  testit::assert(file.exists(mbd_tree_filename))
  testit::assert("mbd.tree" %in% list.files(parameters_folder))
  testit::assert(length(mbd_tree_filename) > 0)

  # Create the name of the alignent file, e.g. '/myfolder/mbd.fasta'
  mbd_alignment_filename <- file.path(parameters_folder, "mbd.fasta")

  # Get the MBD phylogeny for 'mbd.tree'
  mbd_phylogeny <- get(load(mbd_tree_filename))$tes

  # Get the sequence length from the parameters filename
  sequence_length <- parameters$sequence_length

  # Calculate the mutation rate from the tree
  # Found:
  #   BD_mutation_rate <-  MBD_mutation_rate * (sum(MBD_tree$edge.length)/sum(BD_tree$edge.length)); # nolint
  mutation_rate <- NULL

  # Root sequence is e.g. AAACCCGGGTTT
  root_sequence <- pirouette::create_blocked_dna(sequence_length)

  alignment <- pirouette::sim_alignment(
    phylogeny = mbd_phylogeny,
    sequence_length = NULL,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate
  )

  # Save the alignment
  ape::write.dna(
    alignment,
    file = mbd_alignment_filename,
    format = "fasta"
  )

  testit::assert(file.exists(mbd_alignment_filename))
}
