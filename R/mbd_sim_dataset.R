# mbd_sim_dataset---------------------------------
#' @author Giovanni Laudanno
#' @title Creates a full simulated dataset of trees under the multiple birth
#'   death process
#' @description mbd_sim_dataset produces a full dataset of max_sims simulated
#'   trees including multiple speciations at the same time. This second version
#'   takes into account the possibility both of allopatric and sympatric
#'   speciation.
#' @inheritParams default_params_doc
#' @param edge the program automatically detects the (estimated) average
#'   number of tips. "edge" defines the width of the spread around the mean.
#' @return The function returns a list of brts vectors, one for each of the
#'   max_sims simulations.
#'   It also saves those in a file called "sim_data" and saves all the settings
#'   in a "general_settings" file.
#'   N.B.: At each call of the function you overwrite the previous files.
#'
#' @examples
#' # @Giappo: does not work
#' # out <- mbd_sim_dataset(
#' #  pars=c(0.4, 0.1, 0.2, 0.15), soc=2, age=10, cond=1, edge=Inf
#' #)
#'
#' @export
mbd_sim_dataset <- function(
  sim_pars = c(0.5, 0.1, 0.3, 0.15),
  soc = 2,
  cond = 1,
  age = 10,
  max_sims = 1000,
  tips_interval = c(0, 100),
  edge = Inf,
  minimum_multiple_births = 0
) {
  # mbd_sim_dataset creates a full simulated dataset of "max_sims" trees
  #"edge" gives the extent of fluctuations
  #  around the mean that i want to consider

  if (sim_pars[2] == 0) {
    #this allows me to use the analytical formula CHECK THIS!
    cond <- 0
    tips_interval <- c(0, Inf)
  }

  init_n_lineages <- soc
  # Would call hyperA function
  testit::assert(!(edge != Inf && tips_interval == c(0, Inf)))

  #simulate trees
  sim_data <- sim_tes <- sim_tas <- vector("list", max_sims)
  ext_species <- rep(NA, max_sims)
  for (s in 1:max_sims) {
    set.seed(s)
    simulation <- mbd_sim(
      pars = sim_pars,
      soc = soc,
      age = age,
      cond = cond,
      tips_interval = tips_interval,
      minimum_multiple_births = minimum_multiple_births
    )
    sim_data[[s]]  <- simulation$brts
    ext_species[s] <- simulation$extinct_species
    sim_tes[[s]]   <- simulation$tes
    sim_tas[[s]]   <- simulation$tas
  }

  max_k <- (init_n_lineages - 1) + (is.list(sim_data)) *
    max(sapply(sim_data, length)) + (1 - is.list(sim_data)) * length(sim_data)
  if (is.list(sim_data)) {
    all_the_births <- sapply(
      sim_data, FUN = function(brts) {
        return(brts2time_intervals_and_births(brts)$births) # nolint internal function
      }
    )
  } else{
    all_the_births <- brts2time_intervals_and_births(sim_data)$births  # nolint internal function
  }
  max_b <- max(unlist(all_the_births))

  additional_species <- tips <- rep(0, max_sims)
  for (s in 1:max_sims) {
    additional_species[s] <- sum(duplicated(sim_data[[s]]))
    tips[s] <- length(sim_data[[s]]) + 1
  }
  #saving sims and settings
  simpath <- getwd()
  datapath <- paste0(simpath, "/ data")
  sim_data_name <- paste0(datapath, "/ sim_data")
  general_settings_name <- paste0(datapath, "/ general_settings")
  sim_trees_name <- paste0(datapath, "/ sim_trees")
  if (file.exists(sim_data_name)) {
    suppressWarnings(file.remove(sim_data_name))
  }
  save(sim_data, file = sim_data_name)
  if (file.exists(general_settings_name)) {
    suppressWarnings(file.remove(general_settings_name))
  }
  save(
    sim_pars, soc, age, cond, max_sims, tips_interval, max_k,
    max_b, ext_species, additional_species, tips,
    minimum_multiple_births, file = general_settings_name
  )
  if (file.exists(sim_trees_name)) {
    suppressWarnings(file.remove(sim_trees_name))
  }
  save(sim_tas, sim_tes, file = sim_trees_name)
  return(sim_data)
}
