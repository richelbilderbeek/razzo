# pmb_ML_cluster----------------
# @Giappo: add doc
#' Does something I
#' @inheritParams default_params_doc
#' @export
pmb_ML_cluster <- function(
  s,
  initparsopt = c(0.5, 0, 1.7, 0.15)
) {
  print(s)
  parnames <- c("lambda","mu","nu","q"); Npars <- length(parnames)
  idparsopt <- 1:Npars; parsfix <- NULL;

  #load general sim settings in order to make the estimation coherent with sim conditions
  # simpath=paste("sims/",sim_pars[1],"-",sim_pars[2],"-",sim_pars[3],"/",sep = '')
  simpath  <- getwd()
  datapath <- paste0(simpath,"/data")
  load(file = paste0(datapath,"/general_settings"))
  load(file = paste0(datapath,"/sim_data"))
  if (sim_pars[2] == 0 && cond == 0 && tips_interval == c(0, Inf)) {
    # pure birth adjustments
    idparsopt2 <- idparsopt[idparsopt != 2]
    idparsfix2 <- (1:4)[-idparsopt2]
    parsfix2 <- initparsopt2 <- rep(NA,4);
    parsfix2[idparsfix] <- parsfix
    parsfix2[2] <- 0
    parsfix2 <- parsfix2[!is.na(parsfix2)]
    initparsopt2[idparsopt] <- initparsopt
    initparsopt2 <- initparsopt2[c(1,3,4)]
    initparsopt2 <- initparsopt2[!is.na(initparsopt2)]

    parsfix     <- parsfix2
    idparsfix   <- idparsfix2
    idparsopt   <- idparsopt2
    initparsopt <- initparsopt2
  }
  if (sim_pars[2] != 0) {stop("You should not use this function if mu > 0")}

  if (!file.exists(paste0(simpath,"/errors"))){dir.create(paste0(simpath,"/errors"))}
  sink(file = paste0(simpath,"/errors/mbd_MLE_errors",s,".txt"), append = T)

  res <- mbd_ML(
    brts = sim_data[[s]],
    initparsopt = initparsopt,
    idparsopt = idparsopt,
    idparsfix = (1:Npars)[-idparsopt],
    parsfix = parsfix,
    missnumspec = 0,
    cond = cond,
    soc = soc,
    tips_interval = tips_interval,
    res = 10 * (1 + length(brts) + missnumspec),
    tol = c(1E-3, 1E-4, 1E-6),
    maxiter = 1000 * round((1.25)^length(idparsopt)),
    changeloglikifnoconv = FALSE,
    optimmethod = 'simplex',
    minimum_multiple_births = minimum_multiple_births
  )

  #additional tree info
  how_many_multiple <- percent_multiple <- -1;
  if (length(sim_data[[s]]) > 2)
  {
    test0 <- sim_data[[s]][-1]; #actual branching events
    test1 <- duplicated( test0 ); #additional species
    test2 <- test1; for (iii in 2:length(test1)){if(test1[iii] == T){test2[iii - 1] = T}} #considering also the first species at each multiple event
    how_many_multiple <- sum(test2);
    percent_multiple  <- how_many_multiple/length(test2);
  }
  # additional_species = sum( duplicated(sim_data[[s]]) );
  tips <- length(sim_data[[s]]) + 1;

  out  <- c(res[1:(Npars + 1)], how_many_multiple, tips, percent_multiple, s);
  out2 <- out; #names(out2)=c("lambda","mu","nu","q","LL","species born from multiple events","number of tips","percentage of species born from multiple events","tree id")
  names(out2) <- c(parnames,"LL","species born from multiple events","number of tips","percentage of species born from multiple events","tree id")
  print(out2)
  #out[1] = lambda
  #out[2] = mu
  #out[3] = nu
  #out[4] = q
  #out[5] = LL
  #out[6] = species born from multiple events
  #out[7] = number of tips
  #out[8] = percentage of species born from multiple events
  #out[9] = tree id
  sink()
  print(out2)

  utils::write.table(matrix(out,ncol = length(out)),file = paste(simpath,"/mbd_MLE",s,".txt",sep = ''),append = T,row.names = F,col.names = F, sep = ",")
  if (res[1:4] != rep(-1, Npars)){suppressWarnings(  file.remove( paste(simpath,"/errors/mbd_MLE_errors",s,".txt",sep = '') )  )}
}



# @Giappo: add doc
# TODO: may be same as pmb_ML_cluster
#' Does something O
#' @export
pmb_ML_cluster2 <- function(s,initparsopt=c(0.5,0.15,0.1)){
  # initparsopt=c(1.8,0.3,0.15);
  parnames <- c("lambda","mu","nu","q"); Npars <- length(parnames)
  idparsopt <- 1:Npars; parsfix <- NULL;

  # simpath=paste("sims/",sim_pars[1],"-",sim_pars[2],"-",sim_pars[3],"/",sep = '')
  simpath <- getwd()

  datapath <- paste0(simpath,"/data")
  load(file=paste0(datapath,"/general_settings"))
  load(file=paste0(datapath,"/sim_data"))
  print(s)

  if ( !file.exists(paste(simpath,"/errors",sep = '')) ){dir.create(paste(simpath,"/errors",sep = ''))}
  sink(file = paste0(simpath,"/errors/mbd_MLE_errors",s,".txt"), append = T)

  res <- pmb_ML(brts = sim_data[[s]],
                      initparsopt = initparsopt,
                      res = 10 * (1 + length(brts) + 0), #this 0 used to be missnumspec
                      tol = c(1E-3, 1E-4, 1E-6),
                      maxiter = 1000 * round((1.25)^length(idparsopt)),
                      changeloglikifnoconv = FALSE,
                      optimmethod = 'simplex')

  #additional tree info
  how_many_multiple <- percent_multiple <- -1;
  if (length(sim_data[[s]])>2)
  {
    test0 <- sim_data[[s]][-1]; #actual branching events
    test1 <- duplicated(test0); #additional species
    test2 <- test1;for (iii in 2:length(test1)){if(test1[iii]==T){test2[iii-1]=T}} #considering also the first species at each multiple event
    how_many_multiple <- sum(test2);
    percent_multiple  <- how_many_multiple/length(test2);
  }
  # additional_species = sum( duplicated(sim_data[[s]]) );
  tips <- length(sim_data[[s]])+1;

  out  <- c(res[1:(Npars+1)], how_many_multiple, tips, percent_multiple, s);
  out2 <- out; #names(out2)=c("lambda","mu","nu","q","LL","species born from multiple events","number of tips","percentage of species born from multiple events","tree id")
  names(out2) <- c(parnames,"LL","species born from multiple events","number of tips","percentage of species born from multiple events","tree id")
  print(out2)
  #out[1] = lambda
  #out[2] = mu
  #out[3] = nu
  #out[4] = q
  #out[5] = LL
  #out[6] = species born from multiple events
  #out[7] = number of tips
  #out[8] = percentage of species born from multiple events
  #out[9] = tree id
  sink()
  print(out2)

  utils::write.table(matrix(out,ncol = length(out)), file = paste0(simpath,"/mbd_MLE",s,".txt"),
              append = T, row.names = F, col.names = F, sep = ",")
  if (res[1:4] != rep(-1, Npars)){suppressWarnings(  file.remove( paste0(simpath,"/errors/mbd_MLE_errors",s,".txt") )  )}
}

