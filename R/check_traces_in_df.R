#' Check if all the traces' names are in the names of the data frames
#' @param traces_names names of the traces
#' @param df a data frame
#' @author Richel J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' df <- data.frame(my_name = 3.14)
#' expect_silent(check_traces_in_df("my_name", df))
#' expect_error(check_traces_in_df("absent", df))
#' @export
check_traces_in_df <- function(traces_names, df) {
  if (!all(traces_names %in% names(df))) {
    msg <- "Not all 'traces_names' are present in data frame. \n"
    for (traces_name in traces_names) {
      if (!traces_name %in% names(df)) {
        msg <- c(msg, paste0("'", traces_name, "' is absent. \n"))
      }
    }
    stop(paste0(msg, collapse = ""))
  }
}
