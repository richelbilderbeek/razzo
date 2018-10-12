# mbd functions with 3 parameters (all deprecated as of now)
# mbd_loglik0---------------------------------
#' @author Giovanni Laudanno
#' @title Calculates the likelihood for a multiple birth-death process
#' @description mbd_loglik0 provides the likelihood for a process in which multiple births (from different parents) at the same time are possible.
#' @inheritParams default_params_doc
#' @param pars vector of parameters:
#' \itemize{
#'   \item pars[1] is the multiple speciation trigger rate;
#'   \item pars[2] is the extinction rate;
#'   \item pars[3] is the single-lineage speciation probability.
#' }
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
mbd_loglik0 <- function(
  pars, brts, soc = 2, cond = 0, tips_interval = c(0,Inf), missnumspec = 0,
  safety_threshold = 1e-4, methode = "expo", debug_check = 0, alpha = 20
) {
  
  #Optional stuff that I might need to run the program one line at the time:
  #brts=sim_data[[1]];missnumspec=0;pars=sim_pars;missing_interval=c(1,Inf)
  
  #BASIC SETTINGS AND CHECKS
  lambda <- pars[1]
  mu <- pars[2]
  q <- pars[3]
  min_tips <- tips_interval[1]
  max_tips <- tips_interval[2]
  abstol <- 1e-16
  reltol <- 1e-10;
  starting_alpha = alpha
  
  condition1 <- (any(is.nan(pars)) != 0 | any(is.infinite(pars)) != 0)
  condition2 <- (mu < 0 | 
      lambda <= 0 + safety_threshold | 
      q <= 0 + safety_threshold | 
      q >= 1 - safety_threshold
  )
  if (condition1 | condition2) {
    loglik <- -Inf
  } else if (length(pars) != 3) { 
    print("input parameters are wrong")
    loglik <- -Inf
  } else{
    pc <- -1
    while (pc < 0 && alpha <= max(starting_alpha, 80)) {
      
      #ADJUSTING DATA
      data <- brts2time_intervals_and_births(brts)
      time_intervals <- c(0, data$time_intervals)
      births <- c(0, data$births)
      
      #SET UP
      init_n_species = soc #number of starting species
      k_interval = init_n_species + cumsum(births)
      max_k = max(k_interval)
      # alpha is the proportionality factor between max_k 
      # and the edge of the matrix
      max_number_of_species = alpha*max_k; 
      #nvec <- 0:max_number_of_species
      
      #SETTING INITIAL CONDITIONS (there's always a +1 because of Q0)
      q_i <- c(1,rep(0, max_number_of_species))
      q_t <- matrix(
        0, 
        ncol = (max_number_of_species + 1), 
        nrow = length(time_intervals)
      )
      q_t[1, ] <- q_i
      dimnames(q_t)[[2]] <- paste("Q", 0:max_number_of_species, sep = "")
      # init_n_species is the number of species at t=1
      k <- init_n_species 
      # t is starting from 2 so everything is ok 
      # with birth[t] and time_intervals[t] vectors
      t <- 2  
      C <- rep(1,(length(time_intervals))) 
      D <- C
      log_b <- 0
      
      #EVOLVING THE INITIAL STATE TO THE LAST BRANCHING POINT
      while (t < length(time_intervals)) {
        
        #Applying A operator
        transition_matrix <- create_a_zero(
          max_number_of_species = max_number_of_species,
          lambda = lambda,
          mu = mu,
          q = q,
          k = k
        )
        q_t[t,] <- a_operator(
          Q = q_t[(t - 1),], 
          transition_matrix = transition_matrix,
          time_interval = time_intervals[t],
          precision = 50L,
          methode = methode,
          a_abstol = abstol,
          a_reltol = reltol
        )
        if (methode != "sexpm") {
          # it removes some small negative values 
          # that can occurr as bugs from the integration process
          q_t[t,] <- mbd:::negatives_correction(q_t[t,], pars)
        } 
        
        #Applying C operator (this is a trick to avoid precision issues)
        if (debug_check == 1) {
          print(head(q_t[t,]))
        }
        C[t] <- 1 / (sum(q_t[t,]))
        q_t[t,] <- q_t[t,]*C[t]
        
        #Applying B operator
        B <- create_b_zero(
          max_number_of_species = max_number_of_species, 
          q = q, 
          k = k, 
          b = births[t]
        )
        q_t[t,] <- (B %*% q_t[t,])
        if (methode != "sexpm") {
          q_t[t,] <- mbd:::negatives_correction(q_t[t,],pars)
        }
        log_b  <-  log_b + log(lambda) + lchoose(k,births[t]) + births[t]*log(q)
        
        #Applying D operator (this works exactly like C)
        if (debug_check == 1) {
          print(head(q_t[t,]))
        }
        D[t] <- 1/(sum(q_t[t,]))
        q_t[t,] <- q_t[t,]*D[t]
        
        #Updating running parameters
        k <- k + births[t]
        t <- t + 1
      }
      
      #Applying A operator from the last branching time to the present
      transition_matrix <- create_a_zero(
        max_number_of_species = max_number_of_species,
        lambda = lambda,
        mu = mu,
        q = q,
        k = k
      )
      q_t[t,] <- a_operator(
        Q = q_t[(t - 1),],
        transition_matrix = transition_matrix,
        time_interval = time_intervals[t],
        precision = 50L,
        methode = methode,
        a_abstol = abstol,
        a_reltol = reltol
      )
      if (methode != "sexpm") {
        q_t[t,] <- mbd:::negatives_correction(q_t[t, ], pars)
      }
      if (debug_check == 1) {
        print(head(q_t[t, ]))
      }
      
      #Selecting the state I am interested in
      vm <- 1 / choose((k+missnumspec),k)
      P <- vm * q_t[t,(missnumspec + 1)] #I have to include +1 because of Q0
      
      #Removing C and D effects from the LL
      loglik <- log(P) + log_b - sum(log(C)) - sum(log(D))
      
      #Various checks
      loglik <- as.numeric(loglik)
      if (is.nan(loglik) | is.na(loglik)){
        loglik=-Inf
      }
      
      #CONDITIONING THE LIKELIHOOD ON THE SURVIVAL OF CROWN SPECIES
      pc <- 1
      #difference between sexpm and expo are not here
      if (
        (
          cond == 1 | 
          tips_interval[1] > 1 | 
          tips_interval[2] < Inf 
        ) & !is.infinite(loglik) 
      ) { 
        
        total_time <- max(abs(brts));
        testit::assert(max_number_of_species < Inf)
        m <- 0:max_number_of_species
        one_over_cm <- (3 * (m + 1)) / (m + 3)
        one_over_qm_binom <- 1/choose((m + init_n_species),init_n_species)
        #applying tips constrain
        tips_components <- (1 + min_tips):(
          1 + min(max_tips,max_number_of_species)
        ) 
        
        mk_n_zero <- mbd:::create_a_zero(
          max_number_of_species = max_number_of_species,
          lambda = lambda,
          mu = mu,
          q = q,
          k = init_n_species
        )
        A2_v1 <- a_operator(
          Q = q_t[1, ],
          transition_matrix = mk_n_zero,
          time_interval = total_time,
          precision = 50L,
          methode = methode,
          a_abstol = abstol,
          a_reltol = reltol
        )
        if (methode != "sexpm") {
          # it removes some small negative values 
          # that can occurr as bugs from the integration process
          A2_v1 <- mbd:::negatives_correction(A2_v1,pars)
        } 
        if (debug_check == 1) {
          print(head(A2_v1, max_tips))
        }
        total_product <- A2_v1 * one_over_cm * one_over_qm_binom
        pc <- sum(total_product[tips_components])
        
        if (pc == 0 ) {
          #slowest and best accuracy
          #use this only if you use sparse matrices
          # ode_matrix=as.matrix(mk_n_zero) 
          ode_matrix <- mk_n_zero
          times <- c(0, total_time)
          A2_v1 <- deSolve::ode(
            y = q_t[1, ], 
            times = times, 
            func = mbd_loglik_rhs, 
            parms = ode_matrix,
            atol = abstol,
            rtol = reltol
          )[2, -1] #evolving crown species to the present
          total_product <- A2_v1*one_over_cm*one_over_qm_binom
          pc <- sum(total_product[tips_components])
        }
      }
      alpha = alpha + 5
    }
    loglik <- loglik - log(pc) #conditioned likelihood
    
  }
  # loglik=-loglik #Rampal's optimizer uses loglik rather than -loglik
  return(loglik)
}

# mbd_loglik_choosepar0----------------
#' @title Internal mbd function
#' @description Internal mbd function.
#' @inheritParams default_params_doc
#' @details This is not to be called by the user.
#' @export
mbd_loglik_choosepar0 <- function(
  trparsopt, trparsfix, 
  idparsopt = 1:3,
  idparsfix = (1:3)[-idparsopt], 
  brts, 
  cond = 1, 
  soc = 2,
  tips_interval = c(0, Inf), 
  missnumspec = 0,
  methode = "expo", 
  alpha = 20, 
  pars_transform = 0
){
  #This function provides a likelihood for a subset of parameters. 
  # This is built to work inside mbd_minusLL_vs_single_parameter 
  # or any optimizer like optim or subplex
  #idparsopt are the ids for parameters you want to analyze
  #trparsopt are the values for parameters you want to analyze
  #idparsfix are the ids of the parameters you want to fix
  #trparsfix are the values for parameters you want to fix
  
  namepars <- c("lambda","mu","q")
  n_pars <- length(namepars)
  # but I let the user specify it because Rampal also did it (for some reason)
  trpars1 = rep(0,n_pars)
  trpars1[idparsopt] = trparsopt
  if (length(idparsfix) != 0) {
    trpars1[idparsfix] = trparsfix
  }
  if (min(trpars1[1:n_pars]) < 0) {
    loglik <- -Inf
  } else {
    if (pars_transform == 1) {
      #Rampal's transformation
      pars1 <- trpars1 / (1 - trpars1)
    }else
    {
      pars1 <- trpars1
    }
    loglik <- mbd:::mbd_loglik0(
      pars = pars1, brts = brts, cond = cond, soc = soc,
      tips_interval = tips_interval, methode = methode,
      alpha = alpha
    )
  }
  if (is.nan(loglik) || is.na(loglik)) {
    cat("There are parameter values used which cause numerical problems.\n")
    loglik <- -Inf
  }
  loglik
}



# mbd_ml0----------------
#' @author Giovanni Laudanno
#' @title Maximization of the loglikelihood under a multiple birth-death 
#'   diversification model
#' @description mbd_ml0 computes the maximum likelihood estimates of the 
#'   parameters of a multiple birth-death diversification model 
#'   for a given set of phylogenetic branching times. 
#'   It also outputs the corresponding loglikelihood 
#'   that can be used in model comparisons.
#' @inheritParams default_params_doc
#' @param initparsopt The initial values of the parameters 
#'   that must be optimized
#' @param idparsopt The ids of the parameters that must be optimized. 
#'   The ids are defined as follows:
#'   \itemize{
#'     \item id == 1 corresponds to lambda (multiple speciation trigger rate)
#'     \item id == 2 corresponds to mu (extinction rate)
#'     \item id == 3 corresponds to q (single-lineage speciation probability)
#'   }
#' @param idparsfix The ids of the parameters that should not be optimized. 
#'   The default is to fix all parameters not specified in idparsopt.
#' @param parsfix The values of the parameters that should not be optimized.
#' @param res Sets the maximum number of species for which a probability 
#'   must be computed, must be larger than 1 + length(brts).
#' @param tol Sets the tolerances in the optimization. Consists of:
#' \itemize{
#' \item reltolx = relative tolerance of parameter values in optimization
#' \item reltolf = relative tolerance of function value in optimization
#' \item abstolx = absolute tolerance of parameter values in optimization
#' }
#' @param max_iter Sets the maximum number of iterations in the optimization.
#' @param changeloglikifnoconv If TRUE the loglik will be set 
#'   to -Inf if ML does not converge.
#' @param optimmethod Method used in optimization of the likelihood. 
#'   Current default is 'subplex'. Alternative 
#'   is 'simplex' (default of previous versions).
#' @return The output is a dataframe containing estimated parameters 
#'   and maximum loglikelihood. The computed loglikelihood contains the 
#'   factor q! m! / (q + m)!
#'   where q is the number of species in the phylogeny and m is the number of
#'   missing species, as explained in the supplementary material 
#'   to Etienne et al. 2012.
#'
#' @examples
#' set.seed(11)
#' test_pars = c(1.6,0.1,0.08)
#' simulated_data = mbd:::mbd_sim0( pars=test_pars,soc=2,age=10,cond=1 )
#' plot(simulated_data$tas)
#' mbd:::mbd_ml0(brts=simulated_data$brts, initparsopt = 0.11 ,idparsopt = 3,
#' idparsfix = 1:2 ,parsfix = test_pars[1:2],missnumspec=0,cond=1, soc = 2)
#' @export
mbd_ml0 <- function(
  brts, initparsopt, idparsopt, idparsfix = (1:3)[-idparsopt],
  parsfix, missnumspec = 0, cond = 1, soc = 2, tips_interval=c(0,Inf),
  res = 10 * (1 + length(brts) + missnumspec), tol = c(1E-3, 1E-4, 1E-6),
  maxiter = 1000 * round((1.25)^length(idparsopt)), 
  changeloglikifnoconv = FALSE,
  optimmethod = 'subplex', methode = "expo", alpha = 20, pars_transform = 1
) {
  # - tol = tolerance in optimization
  # - changeloglikifnoconv = if T the loglik will be set to -Inf if ML does not converge
  # - maxiter = the maximum number of iterations in the optimization
  # - changeloglikifnoconv = if T the loglik will be set to -Inf if ML does not converge
  # - optimmethod = 'subplex' (current default) or 'simplex' (default of previous versions)
  
  if (missing(parsfix) && (length(idparsfix) == 0)) {
    parsfix <- NULL
  }
  
  options(warn = -1)
  namepars <- c("lambda","mu","q")
  #if you add more parameters to your model just change this
  n_pars <- length(namepars) 
  failpars <- rep(-1,n_pars) 
  #those are the parameters that you get if something goes sideways
  names(failpars) <- namepars 
  if (is.numeric(brts) == FALSE) {
    cat("The branching times should be numeric.\n")
    out2 <- data.frame(t(failpars), loglik = -1, df = -1, conv = -1)
  } else {
    idpars <- sort(c(idparsopt, idparsfix))
    if ((sum(idpars == (1:n_pars)) != n_pars) || 
      (length(initparsopt) != length(idparsopt)) || 
      (length(parsfix) != length(idparsfix)) 
    ) {
      cat("The parameters to be optimized and/or fixed are incoherent.\n")
      out2 <- data.frame(t(failpars), loglik = -1, df = -1, conv = -1)
    } else {
      if (length(namepars[idparsopt]) == 0) { 
        optstr = "nothing" 
      } else { 
        optstr = namepars[idparsopt] 
      }
      cat("You are optimizing",optstr,"\n")
      if (length(namepars[idparsfix]) == 0) { 
        fixstr = "nothing" 
      } else { 
        fixstr = namepars[idparsfix] 
      }
      cat("You are fixing",fixstr,"\n")
      cat("Optimizing the likelihood - this may take a while.","\n")
      utils::flush.console()
      if (pars_transform == 1) {
        #Rampal's transformation
        trparsopt = initparsopt/(1 + initparsopt)
        trparsopt[which(initparsopt == Inf)] = 1
        trparsfix = parsfix/(1 + parsfix)
        trparsfix[which(parsfix == Inf)] = 1
      } else {
        trparsopt  <- initparsopt
        trparsfix  <- parsfix
      }
      optimpars  <- c(tol, maxiter)
      initloglik <- mbd:::mbd_loglik_choosepar0(
        trparsopt = trparsopt, trparsfix = trparsfix,
        idparsopt = idparsopt, idparsfix = idparsfix,
        brts = brts, missnumspec = missnumspec,
        cond = cond, soc = soc,
        tips_interval = tips_interval, methode = methode,
        alpha = alpha, pars_transform = pars_transform
      ) #there's no pars2 here and instead 3 more args at the end
      cat("The loglikelihood for the initial parameter values is",initloglik,"\n")
      utils::flush.console()
      if (initloglik == -Inf) {
        warning(
          "The initial parameter values have a likelihood that is equal to 0 ",
          "or below machine precision. ",
          "Try again with different initial values"
        )
        out2 <- data.frame(t(failpars), loglik = -1, df = -1, conv = -1)
      } else {
        out <- DDD::optimizer(
          optimmethod = optimmethod, optimpars = optimpars,
          fun = mbd:::mbd_loglik_choosepar0, trparsopt = trparsopt,
          trparsfix = trparsfix, idparsopt = idparsopt,
          idparsfix = idparsfix, brts = brts, missnumspec = missnumspec,
          cond = cond, soc = soc, tips_interval = tips_interval,
          methode = methode, alpha = alpha, pars_transform = pars_transform
        )
        if (out$conv != 0) {
          warning(
            "Optimization has not converged. ",
            "Try again with different initial values"
          )
          out2 = data.frame(t(failpars), loglik = -1, df = -1, conv = -1)
        } else {
          mltrpars = as.numeric(unlist(out$par))
          if (pars_transform == 1) {
            #Rampal's transformation
            ml_pars <- mltrpars / (1 - mltrpars)
          } else {
            ml_pars <- mltrpars
          }
          ml_pars1 <- rep(0, n_pars); names(ml_pars1) <- namepars
          ml_pars1[idparsopt] <- ml_pars
          if (length(idparsfix) != 0) {
            ml_pars1[idparsfix] <- parsfix
          }
          max_lik <- as.numeric(unlist(out$fvalues))
          out2 <- data.frame(
            t(ml_pars1), 
            loglik = max_lik, 
            df = length(initparsopt), 
            conv = unlist(out$conv)
          )
          tobeprint <- "Maximum likelihood parameter estimates:"
          for (ii in 1:n_pars) {
            tobeprint <- paste(
              tobeprint,paste(names(ml_pars1[ii]), ":", sep = ""), 
              ml_pars1[ii]
            )
          }
          s1 <- sprintf(tobeprint)
          
          if (out2$conv != 0 & changeloglikifnoconv == TRUE) { 
            out2$loglik <- -Inf 
          }
          s2 <- sprintf("Maximum loglikelihood: %f", max_lik)
          cat("\n",s1,"\n",s2,"\n\n")
        }# bracket#5
      }# bracket#4
    }# bracket#3
  }# bracket#2
  invisible(out2)
}# bracket#1

# mbd_ml_cluster0----------------
# Moved to razzo
