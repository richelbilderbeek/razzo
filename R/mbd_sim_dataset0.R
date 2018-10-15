#' @author Giovanni Laudanno
#' @title Creates a full simulated dataset of trees under the multiple birth death process
#' @description mbd_sim_dataset0 produces a full dataset of max_sims simulated trees including multiple speciations at the same time.
#' @inheritParams default_params_doc
#' @param edge the program automatically detects the (estimated) average number of tips. "edge" defines the width of the spread around the mean.
#' @return The function returns a list of brts vectors, one for each of the max_sims simulations.
#' It also saves those in a file called "sim_data" and saves all the settings in a "general_settings" file.
#' N.B.: At each call of the function you overwrite the previous files.
#'
#' @examples
#' # @Giappo: does not work
#' # out <- mbd_sim_dataset0( pars=c(2.5,0.1,0.1), soc=2, age=10, cond=1, edge=Inf )
#'
#' @export
mbd_sim_dataset0 <- function(
  sim_pars = c(2.5,0.1,0.10),
  soc = 2,
  cond = 1,
  age = 10,
  max_sims = 1000,
  tips_interval = c(0,Inf),
  edge = Inf,
  minimum_multiple_births = 0
) {
  # mbd_sim_dataset0 creates a full simulated dataset of "max_sims" trees, using only three parameters.
  #"edge" gives the extent of fluctuations around the mean that i want to consider

  init_n_lineages <- soc
  if (cond == 1) {
    #if the tree is conditioned on the survival of crown species the minimum amount of tips has to be raised!!!
    tips_interval[1] <- max(init_n_lineages, tips_interval[1])
  }
  if (edge != Inf && tips_interval == c(0, Inf)) {
    estimation <- mbd_P_eq(
      test_parameters = sim_pars,
      age = age,
      max_number_of_species = 3000,
      precision = 50L,
      soc = soc,
      output = 0
    )
    max_tips <- round(estimation$avg_n * (1 + edge));
    min_tips <- max(0, round(estimation$avg_n * (1 - edge)))
    tips_interval <- c(min_tips, max_tips) #c(10,45) #setting the upper limit over 50 may be a problem #min and max number of tips for simulated trees
  }

  #simulate trees
  sim_data <- sim_tes <- sim_tas <- vector("list", max_sims)
  ext_species <- rep(NA, max_sims)
  for (s in 1:max_sims) {
    simulation <- mbd_sim0(
      pars = sim_pars, 
      soc = soc, 
      age = age, 
      cond = cond,
      tips_interval = tips_interval, 
      minimum_multiple_births = minimum_multiple_births
    )
    sim_data[[s]] <- simulation$brts
    ext_species[s] <- simulation$extinct_species
    sim_tes[[s]] <- simulation$tes
    sim_tas[[s]] <- simulation$tas
  }

  max_k <- (init_n_lineages - 1) + (is.list(sim_data)) * 
    max(sapply(sim_data, length)) + (1 - is.list(sim_data)) * length(sim_data)
  if (is.list(sim_data)) {
    all_the_births <- sapply(sim_data, FUN = function(brts){return(brts2time_intervals_and_births(brts)$births)})
  } else {
    all_the_births <- brts2time_intervals_and_births(sim_data)$births
  }
  max_b <- max(unlist(all_the_births))

  additional_species <- tips <- rep(0, max_sims)
  for (s in 1:max_sims) {
    additional_species[s] <- sum( duplicated(sim_data[[s]]) )
    tips[s] <- length(sim_data[[s]])+1
  }
  #saving sims and settings
  # simpath=paste("sims/", sim_pars[1], "-", sim_pars[2], "-", sim_pars[3], "/", sep = '')
  simpath = getwd()
  datapath=paste(simpath, "/data", sep = '')
  sim_data_name = paste(datapath, "/sim_data", sep = '')
  general_settings_name = paste(datapath, "/general_settings", sep = '')
  sim_trees_name = paste(datapath, "/sim_trees", sep = '')
  if (file.exists(sim_data_name)){suppressWarnings( file.remove(sim_data_name) )}
  save(sim_data, file=sim_data_name)
  if (file.exists(general_settings_name)){suppressWarnings( file.remove(general_settings_name) )}
  save(sim_pars, soc, age, cond, max_sims, tips_interval, max_k, max_b, ext_species, additional_species, tips, file=general_settings_name)
  if (file.exists(sim_trees_name)) {
    suppressWarnings( file.remove(sim_trees_name) )
  }
  save(sim_tas, sim_tes, file=sim_trees_name)
  sim_data
}

