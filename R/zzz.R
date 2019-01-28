.onLoad <- function(libname, pkgname){

  suppressPackageStartupMessages(
    lapply(
      c(
        "beautier", "beastier", "tracerer", "mauricer", "babette", "mcbette",
        "pirouette"
      ),
      library,
      character.only = TRUE,
      warn.conflicts = FALSE
    )
  )
  invisible()
}
