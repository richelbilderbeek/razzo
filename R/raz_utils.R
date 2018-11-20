#' @title Convert a tree into branching times
#' @description Convert a tree into branching times. Differently from the ape's
#'  function, it will keep the multiple events. Since the units are million
#'  years, a precision of 8 means that the approximation goes up to the 8-th
#'  digits. With such approximation we consider events happening within an
#'  interval of 4 days (1 million years / 10^8 = 1 year / 100) as simultaneous.
#' @inheritParams default_params_doc
#' @return the branching times
#' @author Giovanni Laudanno
raz_convert_tree2brts <- function(tree, precision = 8) {

  brts0 <- ape::branching.times(tree)
  brts <- DDD::roundn(brts0, digits = precision)

  brts
}

#' @title Site models in Razzo
#' @description Site models in Razzo
#' @inheritParams default_params_doc
#' @return the site models
#' @author Giovanni Laudanno
raz_get_site_models <- function() {
  c("jc69", "gtr")
}

#' @title Clock models in Razzo
#' @description Clock models in Razzo
#' @inheritParams default_params_doc
#' @return the clock models
#' @author Giovanni Laudanno
raz_get_clock_models <- function() {
  c("strict", "rln")
}

#' @title Generative models in Razzo
#' @description Generative models in Razzo
#' @inheritParams default_params_doc
#' @return the generative models
#' @author Giovanni Laudanno
raz_get_gen_models <- function() {
  c("bd", "mbd")
}

#' @title Convert bd phylo to L table
#' @description Convert bd phylo to L table. Don't use for mbd.
#' @inheritParams default_params_doc
#' @return the L table
#' @author Xu Liang
#' @export
bd_phylo2L <- function(
  phylo
) {
  # get L names
  l_names <- colnames(
    mbd::mbd_sim(
      pars = c(0.1, 0.05, 1, 0.1),
      n_0 = 2,
      age = 5,
      cond = 1
    )$l_matrix
  )

  # compute the relative branching times
  brt <- raz_convert_tree2brts(phylo)

  if (min(brt) < 0) {
    brt <- brt + abs(min(brt))
  }
  # number of species including extinct species.
  num.species <- phylo$Nnode + 1
  brt_pre_l <- c(brt[phylo$edge[, 1] - length(phylo$tip.label)])
  # check if the relative branching times are equal to the real branching times.
  # if not correct it to the real branching times.
  if (min(brt_pre_l) == 0) {
    correction <- max(phylo$edge.length[which(brt_pre_l == 0)])
    brt_pre_l <- brt_pre_l + correction
  }
  # preliminary L table
  pre.l_table <- cbind(
    brt_pre_l,
    phylo$edge,
    phylo$edge.length,
    brt_pre_l - phylo$edge.length
  )
  # identify the extant species and the extinct species
  extantspecies.index <- pre.l_table[which(pre.l_table[, 5] <= 1e-10), 3]
  tipsindex <- c(1:num.species)
  extinct.index3 <- subset(tipsindex, !(tipsindex %in% extantspecies.index))
  # assigen the extinct species with extinct times;
  # the extant species with -1 and the internal nodes with 0.
  eeindicator <- matrix(0, length(phylo$edge.length), 1)
  eeindicator[match(extantspecies.index, pre.l_table[, 3])] <- -1
  ext.pos <- match(extinct.index3, pre.l_table[, 3])
  eeindicator[ext.pos] <- pre.l_table[ext.pos, 5]
  pre.l_table <- cbind(pre.l_table, eeindicator)

  sort.L <- pre.l_table[order(pre.l_table[, 1], decreasing = TRUE), ]
  nodesindex <- unique(phylo$edge[, 1])
  L <- sort.L
  real_l <- NULL
  do <- 0
  while (do == 0) {
    j <- which.min(L[, 3])
    daughter <- L[j, 3]
    parent <- L[j, 2]
    if (parent %in% nodesindex) {
      L[which(L[, 2] == parent), 2] <- daughter
      if (length(which(L[, 3] == parent)) == 0) {
        real_l <- rbind(real_l, L[j, ], row.names = NULL)
        L <- L[-j,
               , drop = FALSE]
      } else {
        L[which(L[, 3] == parent), 6] <- L[j, 6]
        L[which(L[, 3] == parent), 3] <- daughter
        L <- L[-j,
               , drop = FALSE]
      }
    } else {
      real_l <- rbind(real_l, L[j, ], row.names = NULL)
      L <- L[-j,
             , drop = FALSE]
    }

    if (nrow(L) == 0) {
      do <- 1
    }
  }
  real_l <- real_l[order(real_l[, 1], decreasing = T), ]
  L <- real_l[, c(1, 2, 3, 6)]

  daughter.index <- L[, 3]
  daughter.realindex <- c(1:nrow(L))
  parent.index <- L[, 2]
  parent.realindex <- match(parent.index, daughter.index)

  L[, 2] <- parent.realindex
  L[, 3] <- daughter.realindex
  L[1, 2] <- 0
  L[1, 3] <- -1
  L[2, 2] <- -1
  for (i in c(2:nrow(L))) {
    if (L[i - 1, 3] < 0) {
      mrows <- which(L[, 2] == abs(L[i - 1, 3]))
      L[mrows, 2] <- L[i - 1, 3]
      L[mrows, 3] <- -1 * L[mrows, 3]
    }
  }
  dimnames(L) <- NULL
  colnames(L) <- l_names
  return(L)
}
