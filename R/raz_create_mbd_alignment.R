#' @title Create an MBD alignment from a parameters file in the same folder
#' @description Create an MBD alignment from a parameters file in the same folder
#' as that parameters file.
#' For example, for a path '/my_folder/parameters.csv', this function creates:
#' '/my_folder/mbd.fasta'.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_mbd_alignment <- function(
  parameters,
  folder_name
)
{
  parameters_folder <- razzo::raz_get_parameters_path(parameters, folder_name)
  mbd_tree_filename <- razzo::raz_create_filename_mbd_tree(parameters, folder_name)
  testit::assert(file.exists(mbd_tree_filename))
  testit::assert("mbd.tree" %in% list.files(parameters_folder))
  testit::assert(length(mbd_tree_filename) > 0)

  # Create the name of the alignent file, e.g. '/myfolder/mbd.fasta'
  mbd_alignment_filename <- razzo::raz_create_filename_mbd_alignment(parameters, folder_name)

  # Get the MBD phylogeny for 'mbd.tree'
  mbd_phylogeny <- get(load(mbd_tree_filename))$tes

  # Get the sequence length from the parameters filename
  sequence_length <- parameters$sequence_length

  # Calculate the mutation rate from the tree
  # Found:
  #   BD_mutation_rate <-  MBD_mutation_rate * (sum(MBD_tree$edge.length)/sum(BD_tree$edge.length)); # nolint
  # mbd_total_branch_length <- sum(mbd_phylogeny$edge.length)
  mutation_rate <- parameters$mbd_mutation_rate

  # Root sequence is e.g. AAACCCGGGTTT
  root_sequence <- pirouette::create_blocked_dna(sequence_length)
  testit::assert(length(root_sequence) > 0)

  alignment <- pirouette::sim_alignment(
    phylogeny = mbd_phylogeny,
    sequence_length = sequence_length,
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
