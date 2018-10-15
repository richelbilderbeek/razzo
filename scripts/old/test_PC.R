
#####
# i <- 1; N_interval <- seq(100, 500, 10); test <- rep(NA, length(N_interval))
# for (max_number_of_species in N_interval)
# {
# 
# time_factor <- 2
# brts <- c(-10, -8, -7) * 10^-time_factor
# pars <- c(0.4, 0, 2, 0.15)
# soc <- 2
# cond <- 1
# tips_interval <- c(0, Inf)
# methode <- 'expo'
# A_abstol <- 1e-16
# A_reltol <- 1e-10
# alpha <- 200
# 
# # mbd::calculate_conditional_probability(brts = brts, pars = pars, cond = 1, soc = 2, alpha = 5)
# 
# lambda <- pars[1]; mu <- pars[2]; nu <- pars[3]; q <- pars[4];
# min_tips <- tips_interval[1]; max_tips <- tips_interval[2];
# min_tips <- max(min_tips, soc * cond) #check this
# N0 <- soc
# total_time <- max(abs(brts));
# births <- c(0, mbd:::brts2time_intervals_and_births(brts)$births)
# k_interval <- N0 + cumsum(births)
# max_k <- max(k_interval)
# # max_number_of_species <- alpha * max_k; #alpha is the proportionality factor between max_k and the edge of the matrix
# 
# m <- 0:max_number_of_species
# one_over_Cm <- (3 * (m + 1))/(m + 3)
# one_over_qm_binom <- 1/choose((m + N0), N0)
# tips_components <- (1 + min_tips):(1 + min(max_tips, max_number_of_species)) #applying tips constrain
# if (cond == 1){tips_components <- tips_components - N0}
# Qi <- c(1, rep(0, max_number_of_species))
# Mk_N0 <- mbd:::create_A(lambda = lambda, mu = mu, nu = nu, q = q, k = soc,
#                         max_number_of_species = max_number_of_species); #image(log(Mk_N0))
# test[i] <- max(is.na(Mk_N0))
# i <- i + 1
# };test
# 
# # A2_v1 <- mbd:::A_operator(Q = Qi, transition_matrix = Mk_N0, time_interval = total_time,
# #                           precision = 50L, methode = methode, A_abstol = A_abstol, A_reltol = A_reltol)
# 
# length(A2_v1)
# length(one_over_Cm)
# length(one_over_qm_binom)
# total_product <- A2_v1 * one_over_Cm * one_over_qm_binom
# Pc <- sum(total_product[tips_components]); Pc
