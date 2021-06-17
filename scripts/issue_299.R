# While working on my thesis,
# I've found some evidence that the mutation rate
# is twice as high as I would have expected.
#
# Get the distribution of the fraction of mutations per mutation rate
#
library(pirouette)
library(ggplot2)

dna_sequence_length <- 100
set.seed(314)

# Tree
phylogeny  <- ape::read.tree(text = "(((A:0.8, B:0.8):0.1, C:0.9):0.1, ((D:0.8, E:0.8):0.1, F:0.9):0.1);")
n_taxa <- ape::Ntip(phylogeny)
ggtree::ggtree(phylogeny, size = 2) +
  ggtree::geom_tiplab(size = 8) +
  ggtree::geom_treescale()

# Alignments
mutation_rates <- seq(0.1, 3.0, 0.1)
rng_seeds <- seq(1, 10)
n_rows <- length(mutation_rates) * length(rng_seeds)

df <- data.frame(mutation_rate = rep(NA, n_rows), rng_seed = NA, f_mutations_per_taxon = NA)
head(df)

row_index <- 1
for (mutation_rate in mutation_rates) {
  for (rng_seed in rng_seeds) {

    alignment_params  <- create_alignment_params(
      root_sequence = pirouette::create_blocked_dna(length = dna_sequence_length),
      mutation_rate = mutation_rate,
      rng_seed = rng_seed
    )

    alignment <- pirouette::create_alignment_impl(
      phylogeny = phylogeny,
      root_sequence = alignment_params$root_sequence,
      rng_seed = alignment_params$rng_seed,
      mutation_rate = alignment_params$mutation_rate,
      site_model = beautier::create_jc69_site_model()
    )

    n_mutations <- pirouette::count_n_mutations(
      alignment = alignment,
      root_sequence = alignment_params$root_sequence,
    )
    f_mutations <- n_mutations / nchar(alignment_params$root_sequence)
    f_mutations_per_taxon <- f_mutations / ape::Ntip(phylogeny)

    df$mutation_rate[row_index] <- mutation_rate
    df$rng_seed[row_index] <- rng_seed
    df$f_mutations_per_taxon[row_index] <- f_mutations_per_taxon
    row_index <- row_index + 1
  }
}

head(df)


ggplot(df, aes(f_mutations_per_taxon)) +
  geom_histogram() +
  facet_grid(mutation_rate ~ .) +
  geom_vline(xintercept = 0.75) +
  labs(
    title("Fraction of mutation per taxon for different mutation rates"),
    caption = paste("Crown age", max(ape::branching.times(phylogeny)), "Issue 299")
  ); ggsave("~/issue_299.png", width = 7, height = 14)
