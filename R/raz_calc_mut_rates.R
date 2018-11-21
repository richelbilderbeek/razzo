#' Calculate the mutation rates
#' @description Analyze the twin trees to assign to each of them the right
#' mutation rate. In this a way it's possible to create alignments with the
#' same amount of information.
#' @inheritParams default_params_doc
#' @return mutation rates
#' @author Giovanni Laudanno
#' @export
calc_mut_rates <- function(
  bd_tree,
  mbd_tree
) {
  bd_crown_age <- max(ape::branching.times(bd_tree))
  mbd_crown_age <- max(ape::branching.times(mbd_tree))
  testit::assert(
    all.equal(bd_crown_age, mbd_crown_age)
  )
  bd_tot_branch_length <- sum(bd_tree$edge.length)
  mbd_tot_branch_length <- sum(mbd_tree$edge.length)

  if (mbd_tot_branch_length <= bd_tot_branch_length) {
    mbd_mut_rate <- 1.0 / mbd_crown_age
    bd_mut_rate <- mbd_mut_rate * mbd_tot_branch_length / bd_tot_branch_length
  } else {
    bd_mut_rate <- 1.0 / bd_crown_age
    mbd_mut_rate <- bd_mut_rate * bd_tot_branch_length / mbd_tot_branch_length
  }

  return(list(mbd_mut_rate = mbd_mut_rate, bd_mut_rate = bd_mut_rate))
}
