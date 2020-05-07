#' Update the \code{razzo} dependencies
#' @author Richèl J.C. Bilderbeek
#' @export
update_razzo <- function() {
  babette::update_babette()
  repo_names <- c("richelbilderbeek/mcbette", "richelbilderbeek/pirouette")
  for (repo_name in repo_names) {
    remotes::install_github(
      repo_name,
      quiet = TRUE,
      dependencies = TRUE,
      upgrade = "always"
    )
  }
}
