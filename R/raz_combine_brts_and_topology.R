#' @title Substitute branching times keeping the topology
#' @description Given a tree topology and a set of branching times it combines them together. They have to be compatible.
#' @inheritParams default_params_doc
#' @return a tree
#' @author Giovanni Laudanno, David Bapst
#' @export
raz_combine_brts_and_topology <- function(
  brts,
  tree
)
{
  if(length(brts) != ape::Nnode(tree)){
    stop("brts must be same length as number of nodes on input tree")}
  # if(!is.null(tree$edge.lengths)){
  #   message("Warning: input tree has $edge.lengths present, these
  #           will be replaced")}
  tree$edge.length <- NULL
  #add zero ages for tips
  allAges <- c(rep(0, ape::Ntip(tree)), brts)
  # get mother node age for each edge
  momAges <- allAges[tree$edge[, 1]]
  # get node ages for child nodes of each edge
  childAges <- allAges[tree$edge[, 2]]
  #edge lengths = mom - child
  edgeLengths <- momAges - childAges
  tree$edge.length <- edgeLengths
  return(tree)
}
