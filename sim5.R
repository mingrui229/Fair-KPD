source("no_fairness.R")
source("group_fairness.R")
source("individual_fairness.R")
source("new_fairness.R")
source("get_cycles.R")
source("compatibility.R")

data_sim1 <- function(n){
  require(dplyr)
  require(Matrix)
  race <- c(rep("W", n * 0.8), rep("B", n * 0.2))
  sens_level <- c(rep("L", n * 0.56), rep("M", n * 0.16), rep("H", n * 0.08), 
                  rep("L", n * 0.14), rep("M", n * 0.04), rep("H", n * 0.02))
  pc <- c(rep(0.05, n * 0.56), rep(0.45, n * 0.16), rep(0.9, n * 0.08), 
          rep(0.05, n * 0.14), rep(0.45, n * 0.04), rep(0.9, n * 0.02))
  blood_type_d <- rep(NA, n)
  blood_type_p <- rep(NA, n)
  for (i in c(1: n)){
    if (race[i] == "B"){
      blood_type_p[i] <- c("O", "A", "B", "AB")[which(rmultinom(1, 1, c(0.51, 0.26, 0.19, 0.04)) == 1)]
    }else{
      blood_type_p[i] <- c("O", "A", "B", "AB")[which(rmultinom(1, 1, c(0.45, 0.40, 0.11, 0.04)) == 1)]
    }
  }
  
  i <- 1
  while (i <= n){
    if (race[i] == "B"){
      blood_type_d[i] <- c("O", "A", "B", "AB")[which(rmultinom(1, 1, c(0.51, 0.26, 0.19, 0.04)) == 1)]
    }else{
      blood_type_d[i] <- c("O", "A", "B", "AB")[which(rmultinom(1, 1, c(0.45, 0.40, 0.11, 0.04)) == 1)]
    }
    if ((!bt_matching(blood_type_d[i], blood_type_p[i])) | (runif(1) < pc[i])){
      i <- i + 1
    }
  }
  ind <- sample(n, n)
  race <- race[ind]
  sens_level <- sens_level[ind]
  blood_type_d <- blood_type_d[ind]
  blood_type_p <- blood_type_p[ind]
  pc <- pc[ind]
  
  Edg <- data.frame()
  for (i in c(1: n))
    for (j in c(1: n))
      if (i != j)
        if ((bt_matching(blood_type_d[i], blood_type_p[j])) & (runif(1) > pc[j]))
          Edg <- rbind(Edg, c(i, j))
  
  colnames(Edg) <- c("X1", "X2")
  cycles <- get_cycles(n, Edg, 3)
  
  return(list(race, sens_level, Edg, cycles))
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

rf_prob <- function(sim){
  X <- sim[[1]]
  wt <- apply(X, 1, sum)
  
  hwn <- c(X %*% as.numeric((sim[[3]] == "H") & (sim[[2]] == "W")))
  hbn <- c(X %*% as.numeric((sim[[3]] == "H") & (sim[[2]] == "B")))
  mwn <- c(X %*% as.numeric((sim[[3]] == "M") & (sim[[2]] == "W")))
  mbn <- c(X %*% as.numeric((sim[[3]] == "M") & (sim[[2]] == "B")))
  lwn <- c(X %*% as.numeric((sim[[3]] == "L") & (sim[[2]] == "W")))
  lbn <- c(X %*% as.numeric((sim[[3]] == "L") & (sim[[2]] == "B")))
  
  nhw <- sum(as.numeric((sim[[3]] == "H") & (sim[[2]] == "W")))
  nhb <- sum(as.numeric((sim[[3]] == "H") & (sim[[2]] == "B")))
  nmw <- sum(as.numeric((sim[[3]] == "M") & (sim[[2]] == "W")))
  nmb <- sum(as.numeric((sim[[3]] == "M") & (sim[[2]] == "B")))
  nlw <- sum(as.numeric((sim[[3]] == "L") & (sim[[2]] == "W")))
  nlb <- sum(as.numeric((sim[[3]] == "L") & (sim[[2]] == "B")))
  
  dh <- hwn / nhw - hbn / nhb
  dm <- mwn / nmw - mbn / nmb
  dl <- lwn / nlw - lbn / nlb
  if (any(is.na(dh))) dh[is.na(dh)] <- 0
  if (any(is.na(dm))) dm[is.na(dm)] <- 0
  if (any(is.na(dl))) dl[is.na(dl)] <- 0
  rf <- race_fairness1(X, wt, dh, dm, dl)
  l1 = 1/max(nhw,nhb)
  l2 = 1/max(nmw,nmb)
  l3 = 1/max(nlw,nlb)
  race_fairness_result <- race_fairness2(rf, wt, dh, dm, dl, l1, l2, l3)
  print(length(race_fairness_result[[2]]))
  return(list(c(t(race_fairness_result[[1]] %*% X) %*% race_fairness_result[[2]]),
              (race_fairness_result[[1]] %*% X)[which.max(race_fairness_result[[2]]), ]))
}

n <- 100
KK <- 50
truth <- list()
est <- list()

for (j in c(1: KK)){
  sim <- data_sim1(300)
  est[[j]] <- list()
  for (k in c(1: 4)){
    est[[j]][[k]] <- matrix(0, nrow = 1000, ncol = k*20)
    for (i in c(1: 1000)){
      res <- data_sim2(sim[[1]], sim[[2]], sim[[3]], sim[[4]], c(1: (k*20), sample(101: 300, 100 - k*20)))
      res2 <- rf_prob(res)
      est[[j]][[k]][i, ] <- res2[[1]][1: (k*20)]
    }
  }
  res <- data_sim2(sim[[1]], sim[[2]], sim[[3]], sim[[4]], c(1: 100))
  res2 <- rf_prob(res)
  truth[[j]] <- res2[[1]]
}