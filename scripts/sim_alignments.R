# Simulate alignments of different numbers of taxa and alignment length

devtools::install_github("richelbilderbeek/becosys", ref = "develop")
library(becosys)

sim_phylo <- function(n_taxa, crown_age) {

  speciation_rate <- log(n_taxa / 2, base = crown_age) / 10 # Divide by 10 appears to be better

  n_tips <- n_taxa - 1 # Must be different
  while (n_tips != n_taxa) {
    phylo <- becosys::bco_pbd_sim(
      create_pbd_params(erg = 0.0, eri = 0.0, scr = 1e8, sirg = speciation_rate, siri = 0.0),
      crown_age = crown_age
    )$recontree
    n_tips <- ape::Ntip(phylo)
    if (n_tips > n_taxa) speciation_rate <- speciation_rate / 1.01
    if (n_tips < n_taxa) speciation_rate <- speciation_rate * 1.01
    message(n_tips)
  }
  phylo
}

set.seed(42)
for (n_taxa in c(10, 20, 40, 80, 160, 320, 640)) {

  phylogeny <- sim_phylo(n_taxa = n_taxa, crown_age = 15)

  alignment <- sim_alignment(
    phylogeny = phylogeny,
    sequence_length = NA,
    root_sequence = pirouette::create_blocked_dna(1000),
    mutation_rate = 1.0 / crown_age
  )
  ape::write.FASTA(x = alignment, file = paste0("~/", n_taxa, ".fas"))
}
