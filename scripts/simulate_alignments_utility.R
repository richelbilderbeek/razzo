





# calculate_Qt <- function(pars, 
#                          brts, 
#                          soc = 2, 
#                          lx = 700,
#                          safety_threshold = 1e-3,
#                          methode = "expo", 
#                          alpha = 10, 
#                          print_errors = TRUE){
#   
#   #Optional stuff that I might need to run the program one line at the time:
#   #brts = sim_data[[1]]; missnumspec = 0;pars = sim_pars; missing_interval = c(1, Inf); methode = "expo"
#   
#   #BASIC SETTINGS AND CHECKS
#   lambda <- pars[1]; mu <- pars[2]; nu <- pars[3]; q <- pars[4]
#   abstol <- 1e-16; reltol <- 1e-10
#   
#   condition1 <- (any(is.nan(pars)) != 0 | any(is.infinite(pars)) != 0)
#   condition2 <- (lambda < 0 | mu < 0 | nu < 0 |
#                    q <= 0 + safety_threshold | 
#                    q >= 1 - safety_threshold)
#   condition3 <- (length(pars) != 4)
#   if       (condition1| condition2| condition3)
#   {
#     stop("wrong pars")
#   }else if (mu == 0)
#   {
#     loglik <- mbd:::pmb_loglik(pars = pars, brts = brts, soc = soc) #using pure birth analytical formula
#   }else
#   {#MAIN
#     
#   #ADJUSTING DATA
#   data <- mbd:::brts2time_intervals_and_births(brts)
#   time_intervals <- c(0, data$time_intervals)
#   births <- c(0, data$births)
#   N0 <- soc #number of starting species
#   k_interval <- N0 + cumsum(births)
#   max_k <- max(k_interval)
#   nvec <- 0:lx
#   
#   #SETTING INITIAL CONDITIONS (there's always a +1 because of Q0)
#   Qi <- c(1, rep(0, lx))
#   Qt <- matrix(0, ncol = (lx + 1), nrow = length(tm))
#   Qt[1,] <- Qi
#   dimnames(Qt)[[2]] <- paste0("Q", 0:lx)
#   k <- N0 #N0 is the number of species at t=1
#   t <- 2  #t is starting from 2 so everything is ok with birth[t] and time_intervals[t] vectors
#   D <- C <- rep(1, (length(time_intervals)))
#   logB <- 0;
#   
#   #EVOLVING THE INITIAL STATE TO THE LAST BRANCHING POINT
#   while (t <= length(time_intervals))
#   {
#     #Applying A operator
#     transition_matrix <- mbd:::create_A(lambda = lambda, mu = mu, nu = nu, q = q, k = k,max_number_of_species = lx)
#     Qt[t,] <- mbd:::A_operator(Q = Qt[(t-1),], transition_matrix = transition_matrix, time_interval = time_intervals[t], precision = 50L, methode = methode, A_abstol = abstol, A_reltol = reltol)
#     
#     if (t < length(time_intervals))
#     {
#       #Applying B operator
#       B <- mbd:::create_B(lambda = lambda, nu = nu, q = q, k = k, b = births[t],
#                           max_number_of_species = lx)
#       Qt[t,] <- (B %*% Qt[t,])
#       
#       #Updating running parameters
#       k <- k + births[t]
#       t <- t + 1
#     }else{break}
#   }
#     
#   return(loglik)
# }
# 
# calculate_conditional_probability_in_time <- function (brts,
#                                                        tm,
#                                                        pars,
#                                                        lx = 700,
#                                                        soc = 2,
#                                                        tips_interval = c(0, Inf),
#                                                        methode = 'expo',
#                                                        abstol = 1e-16,
#                                                        reltol = 1e-10){
#   
#   lambda <- pars[1]; mu <- pars[2]; nu <- pars[3]; q <- pars[4];
#   total_time <- max(abs(brts));
#   
#   m <- 0:lx; length(m)
#   one_over_Cm <- (3 * (m + 1))/(m + 3); length(one_over_Cm)
#   one_over_qm_binom <- 1/choose((m + soc), soc); length(one_over_qm_binom)
#   Qi <- rep(0, lx + 1);  Qi[3] <- 1 #starting with k = 0 and m = 2 missing species
#   k <- 0 #assuming 0 species
#   
#   TM <- mbd:::create_A(lambda = lambda, mu = mu, nu = nu, q = q, k = 0,
#                        max_number_of_species = lx); #dim(TM); max(is.na(TM)); max(is.infinite(TM))
#   
#   # tm <- seq(0, total_time, by = total_time/100)
#   A2_v1 <- try(expoRkit::expv(v = Qi, x = TM, t = tm, m = 50L), silent = T)
#   
#   total_product <- A2_v1 %*% (one_over_Cm * one_over_qm_binom)
#   test <- apply(t(A2_v1), MARGIN = 1, FUN = function(x) x * (one_over_Cm * one_over_qm_binom))
#   tips_components <- 1 + 0:1 #these are the components I want to exclude (the one corresponding to 0 and 1 tips)
#   Pct <- rep(NA, length(tm))
#   Pct <- apply(test, MARGIN = 2, FUN = function(x) 1 - sum(x[tips_components]))
#   
#   return(Pc)
# }





# 
# lavec <- seq((min.lambda <- lambda), (max.lambda <- 3*lambda), by = abs(max.lambda - min.lambda)/(Nsteps <- 20))
# out <- rep(NA, length(lavec))
# for (i in 1:length(lavec))
# {
#   out[i] <- Nct(lambda = lavec[i], mu = mu, t = abs(age))
# }
# 
# md <- stats::lm(log(out) ~ lavec)
# coef(md)
# plot(log(out)~lavec); graphics::abline(a = coef(md)[1], b = coef(md)[2])
