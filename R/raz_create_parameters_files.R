#' For each parameter setting create a folder and a parameters file
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
raz_create_parameters_files <- function(
  folder_name = getwd(),
  lambda.interval = c(0.2, 0.2),
  mu.interval = c(0.15, 0.15),
  nu.interval = seq(from = 1, to = 2.5, by = 0.5),
  q.interval = seq(from = 0.10, to = 0.20, by = 0.05),
  seed.interval = 1:1000,
  soc = 2,
  age = 15,
  cond = 1,
  sequence_length = 1000
)
{
  lambda.interval <- unique(lambda.interval)
  mu.interval     <- unique(mu.interval)
  nu.interval     <- unique(nu.interval)
  q.interval      <- unique(q.interval)

  Lpars <- length(lambda.interval) *
           length(mu.interval) *
           length(nu.interval) *
           length(q.interval)

  project_name <- "razzo_project"

  dir.create(file.path(folder_name, project_name), showWarnings = TRUE)
  testit::assert(dir.exists(file.path(folder_name, project_name)))
  parameters_filenames <- rep(NA, Lpars)
  i <- 1
  for (lambda in lambda.interval) {
    for (mu in mu.interval) {
      for (nu in nu.interval) {
        for (q in q.interval) {
          parsettings_name <- paste0(lambda, "-", mu, "-", nu, "-", q)
          dir.create(file.path(folder_name,
                               project_name,
                               parsettings_name),
                     showWarnings = FALSE)
          for (seed in seed.interval) {
            seedfolder <- file.path(folder_name,
                                    project_name,
                                    parsettings_name,
                                    seed)
            dir.create(file.path(seedfolder),
                       showWarnings = FALSE)

            # TODO: Use 'raz_create_params',
            # like 'parameters <- raz_create_params(...)'
            parameters <- c(lambda = lambda,
                            mu = mu,
                            nu = nu,
                            q = q,
                            seed = seed,
                            cond = cond,
                            age = age,
                            soc = soc,
                            sequence_length = sequence_length)
            parameters_filenames[i] <- file.path(seedfolder, "parameters.csv")
            utils::write.csv(parameters, file = parameters_filenames[i])
            i <- i + 1
          }
        }
      }
    }
  }
  return(parameters_filenames)
}
