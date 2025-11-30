source("no_fairness.R")
source("group_fairness.R")
source("individual_fairness.R")
source("new_fairness.R")
source("get_cycles.R")
source("compatibility.R")

edg_sel <- function(Edg, ind, N) {

  if (!all(c("X1", "X2") %in% names(Edg))) {
    stop("edg_sel: Edg must have columns 'X1' and 'X2'.")
  }

  ind <- as.integer(ind)
  N   <- as.integer(N)

  new_i <- match(as.integer(Edg$X1), ind)
  new_j <- match(as.integer(Edg$X2), ind)

  keep <- !is.na(new_i) & !is.na(new_j)
  if (!any(keep)) {
    return(data.frame(X1 = integer(0L), X2 = integer(0L)))
  }

  data.frame(
    X1 = as.integer(new_i[keep]),
    X2 = as.integer(new_j[keep])
  )
}


cycle_sel <- function(cycles, ind) {

  ind <- as.integer(ind)
  if (!length(cycles)) return(list())

  out  <- vector("list", length(cycles))
  cidx <- 0L

  for (cyc in cycles) {
    new_nodes <- match(as.integer(cyc), ind)
    if (any(is.na(new_nodes))) next

    cidx <- cidx + 1L
    out[[cidx]] <- as.integer(new_nodes)
  }

  if (cidx == 0L) return(list())
  out[seq_len(cidx)]
}


data_sim2 <- function(race, sens_level, Edg, cycles, ind){
  require(dplyr)
  require(Matrix)
  N <- length(race)
  race <- race[ind]
  sens_level <- sens_level[ind]
  Edg <- edg_sel(Edg, ind, N)
  cycles <- cycle_sel(cycles, ind)
  number_of_cycles <- length(cycles)
  X <- matrix(0, nrow = number_of_cycles, ncol = length(race))
  for (i in c(1: number_of_cycles)){
    X[i, cycles[[i]]] <- 1
  }
  return(list(X, race, sens_level))
}

rf_prob <- function(sim) {
  X   <- sim[[1]]
  race <- sim[[2]]
  sens <- sim[[3]]
  wt <- apply(X, 1L, sum)

  hwn <- c(X %*% as.numeric((sens == "H") & (race == "W")))
  hbn <- c(X %*% as.numeric((sens == "H") & (race == "B")))
  mwn <- c(X %*% as.numeric((sens == "M") & (race == "W")))
  mbn <- c(X %*% as.numeric((sens == "M") & (race == "B")))
  lwn <- c(X %*% as.numeric((sens == "L") & (race == "W")))
  lbn <- c(X %*% as.numeric((sens == "L") & (race == "B")))

  nhw <- sum((sens == "H") & (race == "W"))
  nhb <- sum((sens == "H") & (race == "B"))
  nmw <- sum((sens == "M") & (race == "W"))
  nmb <- sum((sens == "M") & (race == "B"))
  nlw <- sum((sens == "L") & (race == "W"))
  nlb <- sum((sens == "L") & (race == "B"))

  l_strong <- 0.5 * c(
    1 / max(1, max(nhw, nhb)),
    1 / max(1, max(nmw, nmb)),
    1 / max(1, max(nlw, nlb))
  )

  x_cycles <- race_fairness(
    X, wt,
    hwn, hbn, mwn, mbn, lwn, lbn,
    nhw, nhb, nmw, nmb, nlw, nlb,
    ell = l_strong
  )

  if (!length(x_cycles)) {
    sel <- rep(0, ncol(X))
  } else {
    sel <- as.numeric(crossprod(x_cycles, X)) 
  }

  return(list(sel, sel))
}


n <- 100
KK <- 100
truth <- list()
est <- list()

for (j in c(1: KK)){
  print(j)
  sim <- data_sim1(300)
  est[[j]] <- list()
  for (k in c(1: 4)){
    est[[j]][[k]] <- matrix(0, nrow = 100, ncol = k*20)
    for (i in c(1: 100)){
      res <- data_sim2(sim[[1]], sim[[2]], sim[[3]], sim[[4]], c(1: (k*20), sample(101: 300, 100 - k*20)))
      res2 <- rf_prob(res)
      est[[j]][[k]][i, ] <- res2[[1]][1: (k*20)]
    }
  }
  res <- data_sim2(sim[[1]], sim[[2]], sim[[3]], sim[[4]], c(1: 100))
  res2 <- rf_prob(res)
  truth[[j]] <- res2[[1]]
}
