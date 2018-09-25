# mbd functions with 3 parameters (all deprecated as of now)
# mbd_loglik0---------------------------------
#' @author Giovanni Laudanno
#' @title Calculates the likelihood for a multiple birth-death process
#' @description mbd_loglik0 provides the likelihood for a process in which multiple births (from different parents) at the same time are possible.
#' @param pars vector of parameters:
#' \itemize{
#'   \item pars[1] is the multiple speciation trigger rate;
#'   \item pars[2] is the extinction rate;
#'   \item pars[3] is the single-lineage speciation probability.
#' }
#' @param brts A set of branching times of a phylogeny.
#' @param soc Sets whether stem or crown age should be used (1 or 2)
#' @param cond Set 1 if you want to condition on stem or crown age and non-extinction of the phylogeny. Set 0 otherwise.
#' @param tips_interval It takes into account tips boundaries constrain on simulated dataset.
#' @param missnumspec The number of species that are in the clade but missing in the phylogeny.
#' @param methode Specifies how the integration must be performed: set "sexpm" if you want to use sexpm; set "expo" if you want to use expoRkit; set "lsoda" if you want to use the "lsoda" method with the "deSolve::ode" function.
#' @param safety_threshold It determines the precision on the parameters.
#' @return The function returns the natural logarithm of the likelihood for the process.
#'
#' @examples
#' set.seed(11)
#' simulated_data = mbd:::mbd_sim0( pars=c(2.5,0.1,0.1),soc=2,age=10,cond=1 )
#' plot(simulated_data$tas)
#' mbd_loglik0( pars=c(1.2,0.05,0.1),brts=simulated_data$brts,soc=2,cond=1,missnumspec=0 )
#'
#' @export

#MAIN
mbd_loglik0 <- function(pars, brts, soc = 2, cond = 0, tips_interval = c(0,Inf), missnumspec = 0,
                        safety_threshold = 1e-4,methode = "expo", debug_check = 0,alpha = 20){

  #Optional stuff that I might need to run the program one line at the time:
  #brts=sim_data[[1]];missnumspec=0;pars=sim_pars;missing_interval=c(1,Inf)

  #BASIC SETTINGS AND CHECKS
  lambda=pars[1]; mu=pars[2]; q=pars[3]; min_tips=tips_interval[1]; max_tips=tips_interval[2]; abstol=1e-16; reltol=1e-10;
  starting_alpha = alpha

  condition1 = ( any(is.nan(pars))!=0 | any(is.infinite(pars))!=0 )
  condition2 = ( mu<0 | lambda<=0+safety_threshold | q<=0+safety_threshold | q>=1-safety_threshold ) # mu>=lambda |
  if (condition1 | condition2){loglik = -Inf}
  else if(length(pars)!=3){print("input parameters are wrong");loglik = -Inf}
  else{
    Pc=-1
    while (Pc<0 && alpha <= max(starting_alpha,80)){

      #ADJUSTING DATA
      data=brts2time_intervals_and_births(brts)
      time_intervals=c(0,data$time_intervals)
      births=c(0,data$births)

      #SET UP
      N0 = soc #number of starting species
      k_interval = N0 + cumsum(births)
      max_k = max(k_interval)
      max_number_of_species = alpha*max_k; #alpha is the proportionality factor between max_k and the edge of the matrix
      nvec=0:max_number_of_species

      #SETTING INITIAL CONDITIONS (there's always a +1 because of Q0)
      Qi=c(1,rep(0,max_number_of_species))
      Qt=matrix(0,ncol = (max_number_of_species+1), nrow = length(time_intervals))
      Qt[1,]=Qi
      dimnames(Qt)[[2]] = paste("Q",0:max_number_of_species,sep="")
      k=N0 #N0 is the number of species at t=1
      t=2  #t is starting from 2 so everything is ok with birth[t] and time_intervals[t] vectors
      C=rep(1,(length(time_intervals))); D=C
      logB=0;

      #EVOLVING THE INITIAL STATE TO THE LAST BRANCHING POINT
      while ( t<length(time_intervals) ){

        #Applying A operator
        transition_matrix=create_A0(max_number_of_species = max_number_of_species,lambda = lambda,mu = mu,q = q,k = k)
        Qt[t,]=A_operator(Q = Qt[(t-1),],transition_matrix = transition_matrix,time_interval = time_intervals[t],precision = 50L,methode=methode,A_abstol=abstol,A_reltol=reltol)
        if (methode!="sexpm"){Qt[t,]=mbd:::negatives_correction(Qt[t,],pars)} #it removes some small negative values that can occurr as bugs from the integration process

        #Applying C operator (this is a trick to avoid precision issues)
        if (debug_check==1){print(head(Qt[t,]))}
        C[t]=1/(sum(Qt[t,]))
        Qt[t,]=Qt[t,]*C[t]

        #Applying B operator
        B <- create_B0(max_number_of_species = max_number_of_species, q = q, k = k, b = births[t])
        # B[(row(B)>(2*col(B)+k-births[t])) | col(B)>row(B) ]=0 #this is a constrain due to maximum number of speciations being (2*n+k-b); probably it is redundant
        # if (max(is.nan(B))>0){print(paste("NaN were produced in the B matrix at time=",t))}
        Qt[t,]=(B %*% Qt[t,])
        if (methode!="sexpm"){Qt[t,]=mbd:::negatives_correction(Qt[t,],pars)}
        logB = logB + log(lambda) + lchoose(k,births[t]) + births[t]*log(q)

        #Applying D operator (this works exactly like C)
        if (debug_check==1){print(head(Qt[t,]))}
        D[t]=1/(sum(Qt[t,]))
        Qt[t,]=Qt[t,]*D[t]

        #Updating running parameters
        k=k+births[t]
        t=t+1
      }

      #Applying A operator from the last branching time to the present
      transition_matrix=create_A0(max_number_of_species = max_number_of_species,lambda = lambda,mu = mu,q = q,k = k)
      Qt[t,]=A_operator(Q = Qt[(t-1),],transition_matrix = transition_matrix,time_interval = time_intervals[t],precision = 50L,methode=methode,A_abstol=abstol,A_reltol=reltol)
      if (methode!="sexpm"){Qt[t,]=mbd:::negatives_correction(Qt[t,],pars)}
      if (debug_check==1){print(head(Qt[t,]))}

      #Selecting the state I am interested in
      vm = 1/choose((k+missnumspec),k)
      P  = vm * Qt[t,(missnumspec+1)] #I have to include +1 because of Q0

      #Removing C and D effects from the LL
      loglik = log(P) + logB - sum(log(C)) - sum(log(D))

      #Various checks
      loglik = as.numeric(loglik)
      if ( is.nan(loglik) | is.na(loglik) ){loglik=-Inf}

      #CONDITIONING THE LIKELIHOOD ON THE SURVIVAL OF CROWN SPECIES
      Pc=1
      if ( (cond==1 | tips_interval[1]>1 | tips_interval[2]<Inf ) & !is.infinite(loglik) ){ #difference between sexpm and expo are not here

        total_time=max(abs(brts));
        m=0:max_number_of_species
        one_over_Cm=(3*(m+1))/(m+3)
        one_over_qm_binom=1/choose((m+N0),N0)
        tips_components=(1+min_tips):(1+min(max_tips,max_number_of_species)) #applying tips constrain

        Mk_N0=mbd:::create_A0(max_number_of_species = max_number_of_species,lambda = lambda,mu = mu,q = q,k = N0)
        A2_v1=A_operator(Q = Qt[1,],transition_matrix = Mk_N0,time_interval = total_time,precision = 50L,methode=methode,A_abstol=abstol,A_reltol=reltol)
        if (methode != "sexpm"){A2_v1=mbd:::negatives_correction(A2_v1,pars)} #it removes some small negative values that can occurr as bugs from the integration process
        if (debug_check == 1){print(head(A2_v1, max_tips))}
        total_product=A2_v1 * one_over_Cm * one_over_qm_binom
        Pc=sum(total_product[tips_components])

        if (Pc==0){#slowest and best accuracy
          # ode_matrix=as.matrix(Mk_N0) #use this only if you use sparsematrices
          ode_matrix=MK_N0
          times=c(0,total_time)
          A2_v1=deSolve::ode(y = Qt[1,], times = times, func = mbd:::mbd_loglik_rhs, parms = ode_matrix,atol=abstol,rtol=reltol)[2,-1] #evolving crown species to the present
          total_product=A2_v1*one_over_Cm*one_over_qm_binom
          Pc=sum(total_product[tips_components])
        }

      }
      alpha = alpha + 5
    }
    loglik=loglik-log(Pc) #conditioned likelihood

  }
  # loglik=-loglik #Rampal's optimizer uses loglik rather than -loglik
  return(loglik)
}

# mbd_loglik_choosepar0----------------
#' @title Internal mbd function
#' @description Internal mbd function.
#' @details This is not to be called by the user.
#' @export
mbd_loglik_choosepar0 <- function(trparsopt, trparsfix, idparsopt = 1:3,
                                  idparsfix = (1:3)[-idparsopt], brts, cond = 1, soc = 2,
                                  tips_interval = c(0, Inf), missnumspec = 0,
                                  methode = "expo", alpha = 20, pars.transform = 0){
  #This function provides a likelihood for a subset of parameters. This is built to work inside mbd_minusLL_vs_single_parameter or any optimizer like optim or subplex
  #idparsopt are the ids for parameters you want to analyze
  #trparsopt are the values for parameters you want to analyze
  #idparsfix are the ids of the parameters you want to fix
  #trparsfix are the values for parameters you want to fix

  namepars <- c("lambda","mu","q"); Npars <- length(namepars);
  # idparsopt=(1:3)[-c(idparsfix)] #this argument is useless but I let the user specify it because Rampal also did it (for some reason)
  trpars1 = rep(0,Npars)
  trpars1[idparsopt] = trparsopt
  if (length(idparsfix) != 0)
  {
    trpars1[idparsfix] = trparsfix
  }
  if (min(trpars1[1:Npars]) < 0){loglik <- -Inf}else
  {
    if (pars.transform == 1)
    {
      #Rampal's transformation
      pars1 = trpars1/(1 - trpars1)
    }else
    {
      pars1 <- trpars1
    }
    loglik <- mbd:::mbd_loglik0(pars = pars1, brts = brts, cond = cond, soc = soc,
                                tips_interval = tips_interval, methode = methode,
                                alpha = alpha)
  }
  if(is.nan(loglik) || is.na(loglik))
  {
    cat("There are parameter values used which cause numerical problems.\n")
    loglik <- -Inf
  }
  return(loglik)
}



# mbd_ML0----------------
#' @author Giovanni Laudanno
#' @title Maximization of the loglikelihood under a multiple birth-death diversification model
#' @description mbd_ML0 computes the maximum likelihood estimates of the parameters of a multiple birth-death diversification model for a given set of phylogenetic branching times. It also outputs the corresponding loglikelihood that can be used in model comparisons.
#' @param brts A set of branching times of a phylogeny.
#' @param initparsopt The initial values of the parameters that must be optimized
#' @param idparsopt The ids of the parameters that must be optimized. The ids are defined as follows:
#' \itemize{
#' \item id == 1 corresponds to lambda (multiple speciation trigger rate)
#' \item id == 2 corresponds to mu (extinction rate)
#' \item id == 3 corresponds to q (single-lineage speciation probability)
#' }
#' @param idparsfix The ids of the parameters that should not be optimized. The default is to fix all parameters not specified in idparsopt.
#' @param parsfix The values of the parameters that should not be optimized.
#' @param missnumspec The number of species that are in the clade but missing in the phylogeny.
#' @param cond Set 1 if you want to condition on stem or crown age and non-extinction of the phylogeny. Set 0 otherwise.
#' @param soc Sets whether stem or crown age should be used (1 or 2).
#' @param tips_interval It takes into account tips boundaries constrain on simulated dataset.
#' @param res Sets the maximum number of species for which a probability must be computed, must be larger than 1 + length(brts).
#' @param tol Sets the tolerances in the optimization. Consists of:
#' \itemize{
#' \item reltolx = relative tolerance of parameter values in optimization
#' \item reltolf = relative tolerance of function value in optimization
#' \item abstolx = absolute tolerance of parameter values in optimization
#' }
#' @param max_iter Sets the maximum number of iterations in the optimization.
#' @param changeloglikifnoconv If TRUE the loglik will be set to -Inf if ML does not converge.
#' @param optimmethod Method used in optimization of the likelihood. Current default is 'subplex'. Alternative is 'simplex' (default of previous versions).
#' @param methode Set "sexpm" if you want to use sexpm. Set "expo" if you want to use expoRkit. Set "lsoda" if you want to use "lsoda".
#' @return The output is a dataframe containing estimated parameters and maximum
#' loglikelihood. The computed loglikelihood contains the factor q! m! / (q + m)!
#' where q is the number of species in the phylogeny and m is the number of
#' missing species, as explained in the supplementary material to Etienne et al. 2012.
#'
#' @examples
#' set.seed(11)
#' test_pars = c(1.6,0.1,0.08)
#' simulated_data = mbd:::mbd_sim0( pars=test_pars,soc=2,age=10,cond=1 )
#' plot(simulated_data$tas)
#' mbd:::mbd_ML0(brts=simulated_data$brts, initparsopt = 0.11 ,idparsopt = 3,
#' idparsfix = 1:2 ,parsfix = test_pars[1:2],missnumspec=0,cond=1, soc = 2)
#' @export
mbd_ML0 <- function(brts, initparsopt, idparsopt, idparsfix = (1:3)[-idparsopt],
                    parsfix, missnumspec = 0, cond = 1, soc = 2, tips_interval=c(0,Inf),
                    res = 10 * (1 + length(brts) + missnumspec), tol = c(1E-3, 1E-4, 1E-6),
                    maxiter = 1000 * round((1.25)^length(idparsopt)), changeloglikifnoconv = FALSE,
                    optimmethod = 'subplex', methode = "expo", alpha = 20, pars.transform = 1)
{# bracket#1
  # - tol = tolerance in optimization
  # - changeloglikifnoconv = if T the loglik will be set to -Inf if ML does not converge
  # - maxiter = the maximum number of iterations in the optimization
  # - changeloglikifnoconv = if T the loglik will be set to -Inf if ML does not converge
  # - optimmethod = 'subplex' (current default) or 'simplex' (default of previous versions)

  if (missing(parsfix) && (length(idparsfix)==0)){parsfix <- NULL}

  options(warn=-1)
  namepars <- c("lambda","mu","q"); Npars <- length(namepars); #if you add more parameters to your model just change this
  failpars <- rep(-1,Npars); names(failpars) <- namepars; #those are the parameters that you get if something goes sideways
  if (is.numeric(brts) == FALSE)
  {# bracket#2
    cat("The branching times should be numeric.\n")
    out2 <- data.frame(t(failpars), loglik = -1, df = -1, conv = -1)
  } else {
    idpars <- sort(c(idparsopt, idparsfix))
    if ( (sum(idpars == (1:Npars)) != Npars) || (length(initparsopt) != length(idparsopt)) || (length(parsfix) != length(idparsfix)) )
    {# bracket#3
      cat("The parameters to be optimized and/or fixed are incoherent.\n")
      out2 <- data.frame(t(failpars), loglik = -1, df = -1, conv = -1)
    } else {
      if(length(namepars[idparsopt]) == 0) { optstr = "nothing" } else { optstr = namepars[idparsopt] }
      cat("You are optimizing",optstr,"\n")
      if(length(namepars[idparsfix]) == 0) { fixstr = "nothing" } else { fixstr = namepars[idparsfix] }
      cat("You are fixing",fixstr,"\n")
      cat("Optimizing the likelihood - this may take a while.","\n")
      utils::flush.console()
      if (pars.transform == 1)
      {
        #Rampal's transformation
        trparsopt = initparsopt/(1 + initparsopt)
        trparsopt[which(initparsopt == Inf)] = 1
        trparsfix = parsfix/(1 + parsfix)
        trparsfix[which(parsfix == Inf)] = 1
      }else
      {
        trparsopt  <- initparsopt
        trparsfix  <- parsfix
      }
      optimpars  <- c(tol, maxiter)
      initloglik <- mbd:::mbd_loglik_choosepar0(trparsopt = trparsopt, trparsfix = trparsfix,
                                                idparsopt = idparsopt, idparsfix = idparsfix,
                                                brts = brts, missnumspec = missnumspec,
                                                cond = cond, soc = soc,
                                                tips_interval = tips_interval, methode = methode,
                                                alpha = alpha, pars.transform = pars.transform) #there's no pars2 here and instead 3 more args at the end
      cat("The loglikelihood for the initial parameter values is",initloglik,"\n")
      utils::flush.console()
      if(initloglik == -Inf)
      {# bracket#4
        cat("The initial parameter values have a likelihood that is equal to 0 or below machine precision. Try again with different initial values.\n")
        out2 <- data.frame(t(failpars), loglik = -1, df = -1, conv = -1)
      } else {
        out <- DDD::optimizer(optimmethod = optimmethod, optimpars = optimpars,
                               fun = mbd:::mbd_loglik_choosepar0, trparsopt = trparsopt,
                               trparsfix = trparsfix, idparsopt = idparsopt,
                               idparsfix = idparsfix, brts = brts, missnumspec = missnumspec,
                               cond = cond, soc = soc, tips_interval = tips_interval,
                               methode = methode, alpha = alpha, pars.transform = pars.transform)
        if(out$conv != 0)
        {# bracket#5
          cat("Optimization has not converged. Try again with different initial values.\n")
          out2 = data.frame(t(failpars), loglik = -1, df = -1, conv = -1)
        } else {
          MLtrpars = as.numeric(unlist(out$par))
          if (pars.transform == 1)
          {
            #Rampal's transformation
            MLpars = MLtrpars/(1-MLtrpars)
          }else
          {
            MLpars <- MLtrpars
          }
          MLpars1 <- rep(0, Npars); names(MLpars1) <- namepars
          MLpars1[idparsopt] <- MLpars
          if(length(idparsfix) != 0) {MLpars1[idparsfix] <- parsfix}
          ML <- as.numeric(unlist(out$fvalues))
          out2 <- data.frame(t(MLpars1), loglik = ML, df = length(initparsopt), conv = unlist(out$conv))

          tobeprint <- "Maximum likelihood parameter estimates:"
          for (ii in 1:Npars)
          {
            tobeprint <- paste(tobeprint,paste(names(MLpars1[ii]),":",sep = ""),MLpars1[ii])
          }
          s1 <- sprintf(tobeprint)

          if(out2$conv != 0 & changeloglikifnoconv == T) { out2$loglik = -Inf }
          s2 = sprintf('Maximum loglikelihood: %f',ML)
          cat("\n",s1,"\n",s2,"\n\n")
        }# bracket#5
      }# bracket#4
    }# bracket#3
  }# bracket#2
  invisible(out2)
}# bracket#1
# mbd_ML_cluster0----------------
#' @author Giovanni Laudanno
#' @title Maximization of the loglikelihood under a multiple birth-death
#'   diversification model, wrapped for cluster usage.
#' @description mbd_ML_cluster0 computes the maximum likelihood estimates
#'   of the parameters of a multiple birth-death diversification model.
#'   Differently from mbd_ML it needs only the number "s" of the simulations,
#'   making it suitable to run on a cluster.
#'   You will need two files to make it work: "general_settings",
#'   "sim_data"; they are both generated by mbd_sim_dataset0.
#' @param s The number of the simulation you want to evaluate.
#' @param initparsopt The initial values of the parameters that must be optimized
#' @param idparsopt The ids of the parameters that must be optimized. The ids are defined as follows:
#' \itemize{
#' \item id == 1 corresponds to lambda (multiple speciation trigger rate)
#' \item id == 2 corresponds to mu (extinction rate)
#' \item id == 3 corresponds to q (single-lineage speciation probability)
#' }
#' @param parsfix The values of the parameters that should not be optimized.
#' @return The output is saved on the document "mbd_MLE.txt".
#' \itemize{
#' \item First column contains ML estimates for lambda.
#' \item Second column contains ML estimates for mu.
#' \item Third column contains ML estimates for lambda.
#' \item Fourth column contains maximum likelihood.
#' \item Fifth column contains the number of additional species coming from multiple births in the evaluated tree.
#' }
#'
#' @examples
#' #You will need two files to make it work: "general_settings","sim_data".
#' mbd:::mbd_ML_cluster0(1)
#'
#' @export
mbd_ML_cluster0 <- function(s, initparsopt = c(1.8,0.3,0.15)){
  # initparsopt=c(1.8,0.3,0.15);
  parnames=c("lambda","mu","nu","q"); Npars = length(parnames)
  idparsopt=1:Npars;parsfix=NULL;

  # simpath=paste("sims/",sim_pars[1],"-",sim_pars[2],"-",sim_pars[3],"/",sep = '')
  simpath = getwd()

  datapath=paste(simpath,"/data",sep = '')
  load(file=paste(datapath,"/general_settings",sep = ''))
  load(file=paste(datapath,"/sim_data",sep = ''))
  print(s)

  if ( !file.exists(paste(simpath,"/errors",sep = '')) ){dir.create(paste(simpath,"/errors",sep = ''))}
  sink(file = paste(simpath,"/errors/mbd_MLE_errors",s,".txt",sep = ''), append = T)

  res <- mbd:::mbd_ML0(brts=sim_data[[s]],
                       initparsopt=initparsopt,
                       idparsopt=idparsopt,
                       idparsfix = (1:Npars)[-idparsopt],
                       parsfix = parsfix,
                       missnumspec=0,
                       cond= cond,
                       soc = soc,
                       tips_interval=tips_interval,
                       res = 10*(1+length(brts)+missnumspec),
                       tol = c(1E-3, 1E-4, 1E-6),
                       maxiter = 1000 * round((1.25)^length(idparsopt)),
                       changeloglikifnoconv = FALSE,
                       optimmethod = 'subplex')

  #additional tree info
  how_many_multiple=percent_multiple=-1;
  if (length(sim_data[[s]])>2){
    test0=sim_data[[s]][-1]; #actual branching events
    test1=duplicated( test0 ); #additional species
    test2=test1;for (iii in 2:length(test1)){if(test1[iii]==T){test2[iii-1]=T}} #considering also the first species at each multiple event
    how_many_multiple=sum(test2);
    percent_multiple <- how_many_multiple/length(test2);
  }
  # additional_species = sum( duplicated(sim_data[[s]]) );
  tips=length(sim_data[[s]])+1;

  out=c(res[1:(Npars+1)],how_many_multiple,tips,percent_multiple,s);
  out2=out;#names(out2)=c("lambda","mu","q","LL","species born from multiple events","number of tips","percentage of species born from multiple events","tree id")
  names(out2)=c(parnames,"LL","species born from multiple events","number of tips","percentage of species born from multiple events","tree id")
  print(out2)
  #out[1] = lambda
  #out[2] = mu
  #out[3] = q
  #out[4] = LL
  #out[5] = species born from multiple events
  #out[6] = number of tips
  #out[7] = percentage of species born from multiple events
  #out[8] = tree id
  sink()
  print(out2)

  utils::write.table(matrix(out,ncol = length(out)),file = paste(simpath,"/mbd_MLE",s,".txt",sep = ''),append = T,row.names = F,col.names = F, sep = ",")
  if (res[1:4]!=c(-1,-1,-1,-1)){suppressWarnings(  file.remove( paste(simpath,"/errors/mbd_MLE_errors",s,".txt",sep = '') )  )}
}
