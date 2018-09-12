# rm(list = ls())
if (1) {#functions
  draw.triangle  <- function(tm, brt, brt.bar){
    ti.minus <- brt - brt.bar
    ti.plus  <- brt + brt.bar;
    half_interval <- abs(ti.plus - ti.minus)/2  #brts.bars?
    ti <- (ti.minus + ti.plus)/2 #brts?
    m  <- 1/(half_interval)^2
    s <- sign(tm - ti)
    s1 <- sign(abs(tm - ti) - half_interval) #<0 it is in the interval; >0 is outside
    
    out = ((1 - s1)/2) *
      (
        ((1 - s)/2) * ( m * (tm - ti.minus)          ) +
          ((1 + s)/2) * (-m * (tm - ti      ) + sqrt(m))
      )
    
    if (all(out == 0))
    {
      x <- abs(tm - ti)
      out[which(x == min(x))] <- 1
    }
    return(out)
  }
  draw.triangles <- function(tm, brts, brts.bars){
    brts2 <- brts[-1]
    brts.bars2 <- brts.bars[-1]
    triangles <- vector("list", N <- length(brts2))
    for (i in 1:N)
    {
      triangles[[i]] <- draw.triangle(tm = tm, brt = brts2[i], brt.bar = brts.bars2[i])
    }
    return(triangles)
  }
  sample.brts    <- function(tm, brts, brts.bars) {
    triangles <- draw.triangles(tm = tm, brts = brts, brts.bars = brts.bars)
    sampled.brts <- c(max(abs(brts)), rep(NA, length(brts) - 1))
    for (i in 2:length(brts))
    {
      sampled.brts[i] <- sample(x = tm, size = 1, prob = triangles[[i - 1]]) 
    }
    return(sampled.brts)
  }
  MB.percentage_vs_Nbins <- function(Nbrts, repetitions = 1E4, age = -10, bars.sd = (age/Nbrts)/4) {
    
    age <- -abs(age); N <- Nbrts; bars.sd <- abs(bars.sd)
    brts <- c(age, sort(runif(n = N, min = -9.9, max = 0.1), decreasing = F))
    brts.bars <- c(0, abs(rnorm(n = N, mean = 0, sd = bars.sd)))
    
    max_step <- 20; res <- rep(0, max_step); 
    for (i in 1:max_step)
    {
      Nsteps <- (10 * i)
      tm2 <- seq(from = age, to = 0, by = abs(age - 0)/Nsteps)
      len_birth <- rep(0, repetitions)
      for (r in 1:repetitions)
      {
        test <- sample.brts(tm = tm2, brts = brts, brts.bars = brts.bars)
        births <- brts2time_intervals_and_births(test)$births
        res[i] <- res[i] + sum(births > 1)
        len_birth[r] <- length(births)
      }
      res[i] <- res[i]/(sum(len_birth))
    }
    df <- data.frame(percentage_multiple_events = res, Nbins = (10 * 1:max_step))
    plot.integration <- ggplot2::ggplot(data = df, ggplot2::aes(x = Nbins, y = percentage_multiple_events)) + ggplot2::geom_point() +
      ggplot2::ggtitle(paste0("% multiple events for different binning\n",
                              "brts = ", paste(signif(brts,2), collapse = ", "),
                              "\nbrts.bars = ", paste(signif(brts.bars,2), collapse = ", ")))
    
    dir_name <- paste0(dirname(getwd()),"//results//mbd_integration//"); suppressWarnings(dir.create(dir_name))
    
    j <- 1; while ((file_name <- paste0("MB_percentage_vs_Nbins-N=", N, "-bars.sd=", bars.sd, "-", j,".png")) %in% list.files(folder_name)) {j <- j + 1}; file_name
    file_path <- paste0(dir_name, file_name); file_path
    grDevices::png(filename = file_path)
    plot(plot.integration)
    grDevices::dev.off()
    
    return(MB_percentage = df)
  }
}

#test
# N <- 5; repetit-3.5724ions <- 5E4; bars.sd <- 0.5; age <- -10 # tm <- seq(age, 0, 0.1)
Nvec <- (Nmin <- 2):(Nmax <- 10); test_MB.percentage <- vector("list", Nmax)
for (NN in Nvec)
{
  test_MB.percentage[[NN]] <- MB.percentage_vs_Nbins(Nbrts = NN)
}; test_MB.percentage

i <- 1
ggplot2::ggplot(data = test_MB.percentage[[i <- i + (i < Nmax) - (Nmax - Nmin) * (i >= Nmax)]]
                , ggplot2::aes(x = Nbins, y = percentage_multiple_events)) + 
  ggplot2::geom_point() +
  ggplot2::ggtitle(paste0("% multiple events for different binning")); i
