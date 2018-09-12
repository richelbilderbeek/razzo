rm(list = ls())
if (1){
  #triangles functions
  draw.triangle <- function(tm, brt, brt.bar){
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
  draw.triangles<- function(tm, brts, brts.bars){
    brts2 <- brts[-1]
    brts.bars2 <- brts.bars[-1]
    triangles <- vector("list", N <- length(brts2))
    for (i in 1:N)
    {
      triangles[[i]] <- draw.triangle(tm = tm, brt = brts2[i], brt.bar = brts.bars2[i])
    }
    return(triangles)
  }
  merge.triangles <- function(triangles){
    triangles.matrix <- matrix(unlist(triangles), nrow = length(triangles), byrow = T)
    out <- apply(X = triangles.matrix, MARGIN = 2, FUN = prod)
    return(out)
  }
  merge.triangles.ids <- function(ids, triangles){
    id.triangles <- triangles[ids]
    triangles.matrix <- matrix(unlist(id.triangles), nrow = length(id.triangles), byrow = T)
    out <- apply(X = triangles.matrix, MARGIN = 2, FUN = prod)
    return(out)
  }
  #utility functions
  finite.max <- function(x) max(x[is.finite(x)])
  min.finite.max.list <- function(lista){
    n <- min(unname(unlist(lapply(lista, finite.max))))
    who <- which(unname(unlist(lapply(lista, finite.max))) == min(unname(unlist(lapply(lista, finite.max)))))
    return(list(n = n, who = who))
  }
  get.matrix.columns <- function(matrix) lapply(1:ncol(matrix), FUN = function(i) matrix[,i])
  remove.infinites <- function(vector.list) lapply(vector.list, FUN = function(v) v[!is.infinite(v)])
  all.subsets <- function(set) {
    n    <- length(set)
    bin  <- expand.grid(plyr::rlply(n, c(F, T)))
    out  <- plyr::mlply(bin, function(...) { set[c(...)] })
    out2 <- out[1:nrow(bin)]
    out3 <-unique(lapply(out2, FUN = sort))
    out4 <- out3[-(lapply(out3, FUN = length) == 0)]
    return(out4)
  }
  inside.borders <- function (x, brts){
    cond1 <- (x < min(brts))
    cond2 <- (x > max(brts))
    x <- (1 - cond1 - cond2) * x + cond1 * min(brts) + cond2 * max(brts)
    return(x)
  }
  #scenario function
  intersections <- function(tm, brts, brts.bars){
    t.minus <- brts - brts.bars
    t.plus  <- brts + brts.bars 
    t.minus <- t.minus[-1]
    t.plus  <- t.plus[-1];
    intervals <- rbind(t.minus, t.plus); N <- ncol(intervals)
    intervals <- inside.borders(intervals, brts); intervals
    
    countm <- rep(0, length(tm))
    for (i in 1:N)
    {
      countm <- rbind(countm, i * ((tm <= intervals[2, i]) & (tm >= intervals[1, i])))
    }
    intersected <- apply(countm, MARGIN = 2, sort, decreasing = T)
    intersected[intersected == 0] <- Inf
    combs <- unique(intersected, MARGIN = 2)
    combs <- combs[,!apply(combs, MARGIN = 2, function(x) prod(is.infinite(x)))] #remove all INF
    # combs <- combs[,!apply(combs, MARGIN = 2, FUN = function(x) is.infinite(x[1]))] #remove all INF
    N2 <- ncol(intervals)
    
    pippo1 <- get.matrix.columns(combs); pippo1
    pippo2 <- remove.infinites(pippo1); pippo2
    pippo3 <- unique(unlist(lapply(pippo2, FUN = all.subsets),recursive = F)); pippo3
    pippo4 <- lapply(pippo3, FUN = function(v) c(v, rep(Inf, (N2 - length(v)))) ); pippo4
    pippo5 <- lapply(pippo4, sort); pippo5
    combs  <- matrix(unlist(pippo4), nrow = N2); combs
    
    if (!all(1:N2 %in% combs)) 
    {
      stop("Time needs a finer discretization!")
    }
    
    return(combs)
  }
  select.compatible.scenarios <- function(n, combs) {
    
    cond1 <- apply(combs, 2, function(x) n %in% x)
    # cond1 <- unlist(lapply(combs, function(x) n %in% x))
    cond2 <- apply(combs, 2, function(x) prod(x >= n))
    # cond2 <- unlist(lapply(combs, function(x) prod(x >= n)))
    out0  <- combs[, cond1 & cond2]
    # out0  <- combs[cond1 & cond2]
    if (is.matrix(out0))
    {
      out <- split(out0, rep(1:ncol(out0), each = nrow(out0)))
    }else
    {
      out <- list(out0)
    }
    return(out)
  }
  update.scenarios <- function(scen){
    next.one <- min.finite.max.list(scen)
    n   <- next.one$n + 1
    who <- next.one$who
    old.scen <- scen[who]; old.scen
    new.scen <- select.compatible.scenarios(n = n, combs = combs); new.scen
    list.coords <- expand.grid(1:length(old.scen), 1:length(new.scen))
    out.scen <- vector("list", nrow(list.coords))
    for (i in 1:nrow(list.coords))
    {
      out.scen[[i]] <- rbind(old.scen[[list.coords[i,1]]], new.scen[[list.coords[i,2]]])
    }; out.scen
    scen <- do.call(c, list(scen[-who], out.scen))
    scen <- unique(scen)
    coherent_with_n <- unlist(lapply(scen, FUN = function(x) n %in% x))
    scen <- scen[coherent_with_n]
    return(scen)
  }
  find.scenarios <- function(tm, brts, brts.bars){
    combs <- intersections(tm = tm, brts = brts, brts.bars = brts.bars)
    N <- length(brts) - 1
    scen <- select.compatible.scenarios(n = 1, combs = combs); scen
    while (min.finite.max.list(scen)$n != N)
    {
      scen  <- update.scenarios(scen); scen
    } 
    return(scen)
  }
  order.scenarios <- function(scenarios){
    dims <- lapply(scenarios, nrow)
    Udims <- unique(dims)
    dim.scenarios <- vector("list",max(unlist(Udims)))
    for (d in Udims)
    {
      dim.scenarios[[d]] <- scenarios[unlist(dims) == d]
    }
    return(dim.scenarios)
  }
  tree.d.s <- function(d, s, ord.scenarios, triangles) {
    Dist <- vector("list", d)
    BT   <- rep(NA, dmax <- length(ord.scenarios))
    M    <- ord.scenarios[[d]][[s]]
    bt_index <- 2
    for (r in 1:d)
    {
      ids <- M[r,!is.infinite(M[r,])]
      Dist[[r]] <- merge.triangles.ids(ids = ids, triangles = triangles)
      Dist[[r]] <- Dist[[r]]/ sum(Dist[[r]])
      non.zero.D <- which(Dist[[r]] != 0)
      tm2   <- tm[non.zero.D]
      Dist2 <- Dist[[r]][non.zero.D]
      BT[bt_index:(bt_index + length(ids) - 1)] <- sample(x = tm2, prob = Dist2, size = 1)
      bt_index <- bt_index + length(ids)
    }
    BT[1] <- brts[1]; BT
    return(BT)
  }
  area.d.s <- function(d, s, tm, brts, brts.bars, ord.scenarios){
    M    <- ord.scenarios[[d]][[s]]
    Nrow <- nrow(M)
    for (r in 1:d)
    {
      ids <- M[r,!is.infinite(M[r,])]
      Dist[[r]] <- merge.triangles.ids(ids = ids, triangles = triangles)
      right.bins[[r]] <- Dist[[r]] > 0
    }
    lapply(right.bins, FUN = rbind)
    
    
  }
}

# tm <- seq(-10, 0, 0.05)
# brts <- c(-10, -8, -7, -4, -1)
# brts.bars <- c(0, 1, 0.5, 2, 0.1)

N <- 5
# tm <- seq(-10, 0, 0.05)
tm <- seq(-10, 0, 0.1)
brts <- c(-10,sort(runif(n = N, min = -9.9, max = 0.1),decreasing = F))
brts.bars <- c(0, abs(rnorm(n = N, mean = 0, sd = 0.5)))
combs <- intersections(tm, brts, brts.bars); combs

triangles     <- draw.triangles(tm = tm, brts = brts, brts.bars = brts.bars)
scenarios     <- find.scenarios(tm = tm, brts = brts, brts.bars = brts.bars); scenarios
ord.scenarios <- order.scenarios(scenarios = scenarios)

d <- N - 1; s <- 1
ord.scenarios[[d]][[s]]
tree.d.s(d = d, s = s, ord.scenarios = ord.scenarios, triangles = triangles)
