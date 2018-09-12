### 1 par test
rm(list = ls()); idparsopt <- 4; set.seed(length(idparsopt))
brts <- mbd::mbd_sim(pars = (sim_pars <- c(0.2, 0.15, 2, 0.15)), 
                     soc = (soc <- 2), 
                     age = (age <- 10), 
                     cond = (cond <- 1),
                     tips_interval = (tips_interval <- c(0,70)))$brts
test_ML1 <- mbd::mbd_ML(idparsopt = idparsopt, 
                        idparsfix = (idparsfix <- (1:4)[-idparsopt]), 
                        initparsopt = (initparsopt <- (start_pars <- c(0.6, 0.1, 1.3, 0.16))[idparsopt]),
                        brts = brts, 
                        alpha = 10,
                        parsfix = (parsfix <- sim_pars[idparsfix]), 
                        cond = cond, 
                        soc = soc, 
                        tips_interval = tips_interval
); print(sim_pars)

### 2 par test
rm(list = ls()); idparsopt <- 3:4; set.seed(length(idparsopt))
brts <- mbd::mbd_sim(pars = (sim_pars <- c(0.2, 0.15, 2, 0.15)), 
                     soc = (soc <- 2), 
                     age = (age <- 10), 
                     cond = (cond <- 1),
                     tips_interval = (tips_interval <- c(0,70)))$brts
test_ML2 <- mbd::mbd_ML(idparsopt = idparsopt, 
                        idparsfix = (idparsfix <- (1:4)[-idparsopt]), 
                        initparsopt = (initparsopt <- (start_pars <- c(0.6, 0.1, 1.3, 0.16))[idparsopt]),
                        brts = brts, 
                        alpha = 10,
                        parsfix = (parsfix <- sim_pars[idparsfix]), 
                        cond = cond, 
                        soc = soc, 
                        tips_interval = tips_interval
); print(sim_pars)

### 3 par test
rm(list = ls()); idparsopt <- 2:4; set.seed(length(idparsopt))
brts <- mbd::mbd_sim(pars = (sim_pars <- c(0.2, 0.15, 2, 0.15)), 
                     soc = (soc <- 2), 
                     age = (age <- 10), 
                     cond = (cond <- 1),
                     tips_interval = (tips_interval <- c(0,70)))$brts
test_ML3 <- mbd::mbd_ML(idparsopt = idparsopt, 
                        idparsfix = (idparsfix <- (1:4)[-idparsopt]), 
                        initparsopt = (initparsopt <- (start_pars <- c(0.6, 0.1, 1.3, 0.16))[idparsopt]),
                        brts = brts, 
                        alpha = 10,
                        parsfix = (parsfix <- sim_pars[idparsfix]), 
                        cond = cond, 
                        soc = soc, 
                        tips_interval = tips_interval
); print(sim_pars)

### 4 par test
rm(list = ls()); idparsopt <- 1:4; set.seed(5)#; set.seed(length(idparsopt))
brts <- mbd::mbd_sim(pars = (sim_pars <- c(0.2, 0.15, 2, 0.15)), 
                      soc = (soc <- 2), 
                      age = (age <- 10), 
                      cond = (cond <- 1),
                      tips_interval = (tips_interval <- c(0,70)))$brts
# mbd::mbd_loglik(pars = c(sim_pars[1], 10, sim_pars[3], sim_pars[4]), brts = brts, cond = cond, soc = soc, tips_interval = tips_interval)
test_ML4 <- mbd::mbd_ML(idparsopt = idparsopt, 
                        idparsfix = (idparsfix <- (1:4)[-idparsopt]), 
                        initparsopt = (initparsopt <- (start_pars <- c(0.6, 0.1, 1.3, 0.16))[idparsopt]),
                        brts = brts, 
                        alpha = 10,
                        parsfix = (parsfix <- sim_pars[idparsfix]), 
                        cond = cond, 
                        soc = soc, 
                        tips_interval = tips_interval
); print(sim_pars)

### mu test
rm(list = ls()); idparsopt <- 2; set.seed(4)#; set.seed(length(idparsopt))
brts <- mbd::mbd_sim(pars = (sim_pars <- c(0.2, 0.15, 2, 0.15)), 
                     soc = (soc <- 2), 
                     age = (age <- 10), 
                     cond = (cond <- 1),
                     tips_interval = (tips_interval <- c(0,70)))$brts
# mbd::mbd_loglik(pars = c(sim_pars[1], 10, sim_pars[3], sim_pars[4]), brts = brts, cond = cond, soc = soc, tips_interval = tips_interval)
test_ML5 <- mbd::mbd_ML(idparsopt = idparsopt, 
                        idparsfix = (idparsfix <- (1:4)[-idparsopt]), 
                        initparsopt = (initparsopt <- (start_pars <- c(0.6, 0.1, 1.3, 0.16))[idparsopt]),
                        brts = brts, 
                        parsfix = (parsfix <- sim_pars[idparsfix]), 
                        cond = cond, 
                        soc = soc, 
                        tips_interval = tips_interval
); print(sim_pars)
