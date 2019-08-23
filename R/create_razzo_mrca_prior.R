#' Create the MRCA prior as used in the article
#' @export
create_razzo_mrca_prior <- function() {
  beautier::create_mrca_prior(
    is_monophyletic = TRUE,
    mrca_distr = beautier::create_normal_distr(
      mean = get_razzo_crown_age(),
      sigma = 0.0001
    )
  )
}
