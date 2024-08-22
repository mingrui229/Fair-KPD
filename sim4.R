source("no_fairness.R")
source("group_fairness.R")
source("individual_fairness.R")
source("new_fairness.R")
source("get_cycles.R")
source("get_subgraphs.R")
source("compatibility.R")

data_sim2 <- function(data, n){
  blood_type_d <- rep(NA, n)
  hla_d <- matrix(nrow = n, ncol = 6)
  tt <- T
  while(tt){
    rn1 <- sample(nrow(data), n)
    hla_p <- as.matrix(data[rn1, 4:9])
    blood_type_p <- data[rn1, 2]
    race <- rep("B", n)
    race[which(data[rn1, 1] == 1)] <- "W"
    pc <- data[rn1, 20] / 100
    sens_level <- rep("M", n)
    sens_level[which(pc == 0)] <- "L"
    sens_level[which(pc > 0.8)] <- "H"
    hb <- which((sens_level == "H") & (race == "B"))
    hw <- which((sens_level == "H") & (race == "W"))
    mb <- which((sens_level == "M") & (race == "B"))
    mw <- which((sens_level == "M") & (race == "W"))
    lb <- which((sens_level == "L") & (race == "B"))
    lw <- which((sens_level == "L") & (race == "W"))
    nhb <- length(hb)
    nhw <- length(hw)
    nmb <- length(mb)
    nmw <- length(mw)
    nlb <- length(lb)
    nlw <- length(lw)
    
    if (nhb * nhw * nmb * nmw * nlb * nlw != 0)
      tt <- F
  }
  
  i <- 1
  while(i <= n){
    rn2 <- sample(nrow(data), 1)
    hla_d[i, ] <- as.numeric(data[rn2, 10:15])
    blood_type_d[i] <- data[rn2, 3]
    if ((!bt_matching(blood_type_d[i], blood_type_p[i])) | (!hla_matching(hla_d[i, ], hla_p[i, ])))
      i <- i+1
  }
  
  
  Edg <- data.frame()
  for (i in c(1: n))
    for (j in c(1: n))
      if (i != j)
        if (bt_matching(blood_type_d[i], blood_type_p[j]))
          if (hla_matching(hla_d[i, ], hla_p[j, ]))
            Edg <- rbind(Edg, c(i, j))
  
  
  colnames(Edg) <- c("X1", "X2")
  cycles <- get_cycles(n, Edg, 3)
  number_of_cycles <- length(cycles)
  X <- matrix(0, nrow = number_of_cycles, ncol = n)
  for (i in c(1: number_of_cycles)){
    X[i, cycles[[i]]] <- 1
  }
  return(list(X, race, sens_level, cycles))
}

KK <- 100
no_fairness_summary <- matrix(nrow = KK, ncol = 7)
group_fairness_summary <- array(dim = c(KK, 2, 7))
individual_fairness_summary <- array(dim = c(KK, 2, 7))
race_fairness_summary <- array(dim = c(KK, 2, 7))

n <- 100
load("unos.rdata")
ind1 <- which(data$donor_ABO %in% c("A1", "A2", "A1B", "A2B"))
ind2 <- which(data$candidate_ABO %in% c("A1", "A2", "A1B", "A2B"))
data <- data[-unique(ind1, ind2), ]

for (ii in c(1: KK)){
  print(ii)
  sim <- data_sim2(data, n)
  vp <- runif(n, 0, 0.3)
  ep <- matrix(runif(n^2, 0, 0.3), n, n)
  RS <- relevant_subset(sim[[4]])
  wt <- rep(NA, length(RS))
  for (i in c(1: length(RS))){
    wt[i] <- expect_cal(RS[[i]], sim[[4]], vp, ep)
  }
  number_of_cycles <- length(RS)
  X <- matrix(0, nrow = number_of_cycles, ncol = n)
  for (i in c(1: number_of_cycles)){
    X[i, RS[[i]]] <- 1
  }

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
  
  no_fairness_result <- no_fairness(X, wt)
  no_fairness_summary[ii, 1] <- sum(no_fairness_result * wt)
  no_fairness_summary[ii, 2] <- sum(no_fairness_result * hwn) / nhw
  no_fairness_summary[ii, 3] <- sum(no_fairness_result * hbn) / nhb
  no_fairness_summary[ii, 4] <- sum(no_fairness_result * mwn) / nmw
  no_fairness_summary[ii, 5] <- sum(no_fairness_result * mbn) / nmb
  no_fairness_summary[ii, 6] <- sum(no_fairness_result * lwn) / nlw
  no_fairness_summary[ii, 7] <- sum(no_fairness_result * lbn) / nlb
  group_fairness_summary[ii, 1, 1] <- sum(no_fairness_result * wt)
  group_fairness_summary[ii, 1, 2] <- sum(no_fairness_result * hwn) / nhw
  group_fairness_summary[ii, 1, 3] <- sum(no_fairness_result * hbn) / nhb
  group_fairness_summary[ii, 1, 4] <- sum(no_fairness_result * mwn) / nmw
  group_fairness_summary[ii, 1, 5] <- sum(no_fairness_result * mbn) / nmb
  group_fairness_summary[ii, 1, 6] <- sum(no_fairness_result * lwn) / nlw
  group_fairness_summary[ii, 1, 7] <- sum(no_fairness_result * lbn) / nlb
  for (jj in c(1: 5)){
    group_fairness_result <- group_fairness(X, wt, hwn+hbn, jj)
    if (length(group_fairness_result) > 0){
      if (sum(group_fairness_result * wt) == no_fairness_summary[ii, 1]){
        group_fairness_summary[ii, 1, 1] <- sum(group_fairness_result * wt)
        group_fairness_summary[ii, 1, 2] <- sum(group_fairness_result * hwn) / nhw
        group_fairness_summary[ii, 1, 3] <- sum(group_fairness_result * hbn) / nhb
        group_fairness_summary[ii, 1, 4] <- sum(group_fairness_result * mwn) / nmw
        group_fairness_summary[ii, 1, 5] <- sum(group_fairness_result * mbn) / nmb
        group_fairness_summary[ii, 1, 6] <- sum(group_fairness_result * lwn) / nlw
        group_fairness_summary[ii, 1, 7] <- sum(group_fairness_result * lbn) / nlb
      }
      group_fairness_summary[ii, 2, 1] <- sum(group_fairness_result * wt)
      group_fairness_summary[ii, 2, 2] <- sum(group_fairness_result * hwn) / nhw
      group_fairness_summary[ii, 2, 3] <- sum(group_fairness_result * hbn) / nhb
      group_fairness_summary[ii, 2, 4] <- sum(group_fairness_result * mwn) / nmw
      group_fairness_summary[ii, 2, 5] <- sum(group_fairness_result * mbn) / nmb
      group_fairness_summary[ii, 2, 6] <- sum(group_fairness_result * lwn) / nlw
      group_fairness_summary[ii, 2, 7] <- sum(group_fairness_result * lbn) / nlb
    }
  }
  im <- individual_fairness1(X, wt)
  for (jj in c(1: 2)){
    individual_fairness_result <- individual_fairness2(im[[1]], im[[2]], im[[3]], im[[4]], seq(from = 0.15, to = 0.25, length.out = 2)[jj])
    individual_fairness_summary[ii, jj, 1] <- sum((individual_fairness_result[[1]] %*% wt) * individual_fairness_result[[2]])
    individual_fairness_summary[ii, jj, 2] <- sum((individual_fairness_result[[1]] %*% hwn) * individual_fairness_result[[2]]) / nhw
    individual_fairness_summary[ii, jj, 3] <- sum((individual_fairness_result[[1]] %*% hbn) * individual_fairness_result[[2]]) / nhb
    individual_fairness_summary[ii, jj, 4] <- sum((individual_fairness_result[[1]] %*% mwn) * individual_fairness_result[[2]]) / nmw
    individual_fairness_summary[ii, jj, 5] <- sum((individual_fairness_result[[1]] %*% mbn) * individual_fairness_result[[2]]) / nmb
    individual_fairness_summary[ii, jj, 6] <- sum((individual_fairness_result[[1]] %*% lwn) * individual_fairness_result[[2]]) / nlw
    individual_fairness_summary[ii, jj, 7] <- sum((individual_fairness_result[[1]] %*% lbn) * individual_fairness_result[[2]]) / nlb
  }
  rf <- race_fairness1(X, wt, hwn / nhw - hbn / nhb, 
                       mwn / nmw - mbn / nmb, 
                       lwn / nlw - lbn / nlb)
  for (jj in c(1: 2)){
    delta <- seq(0, 1, length.out = 2)[jj]
    l1 = delta * 1/min(nhw,nhb) + (1-delta) * 1/max(nhw,nhb)
    l2 = delta * 1/min(nmw,nmb) + (1-delta) * 1/max(nmw,nmb)
    l3 = delta * 1/min(nlw,nlb) + (1-delta) * 1/max(nlw,nlb)
    race_fairness_result <- race_fairness2(rf, wt, hwn / nhw - hbn / nhb, 
                                           mwn / nmw - mbn / nmb, 
                                           lwn / nlw - lbn / nlb, l1, l2, l3)
    race_fairness_summary[ii, jj, 1] <- sum((race_fairness_result[[1]] %*% wt) * race_fairness_result[[2]]) 
    race_fairness_summary[ii, jj, 2] <- sum((race_fairness_result[[1]] %*% hwn) * race_fairness_result[[2]]) / nhw
    race_fairness_summary[ii, jj, 3] <- sum((race_fairness_result[[1]] %*% hbn) * race_fairness_result[[2]]) / nhb
    race_fairness_summary[ii, jj, 4] <- sum((race_fairness_result[[1]] %*% mwn) * race_fairness_result[[2]]) / nmw
    race_fairness_summary[ii, jj, 5] <- sum((race_fairness_result[[1]] %*% mbn) * race_fairness_result[[2]]) / nmb
    race_fairness_summary[ii, jj, 6] <- sum((race_fairness_result[[1]] %*% lwn) * race_fairness_result[[2]]) / nlw
    race_fairness_summary[ii, jj, 7] <- sum((race_fairness_result[[1]] %*% lbn) * race_fairness_result[[2]]) / nlb
  }
  
}
