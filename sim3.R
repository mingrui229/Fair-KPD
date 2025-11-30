source("no_fairness.R")
source("group_fairness.R")
source("individual_fairness.R")
source("new_fairness.R")
source("get_cycles.R")
source("compatibility.R")

safe_div <- function(num, den) if (den > 0) num / den else NA_real_
eq_tol <- 1e-6


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


set.seed(1)

load("unos.rdata")
ind1 <- which(data$donor_ABO %in% c("A1", "A2", "A1B", "A2B"))
ind2 <- which(data$candidate_ABO %in% c("A1", "A2", "A1B", "A2B"))
data <- data[-unique(ind1, ind2), ]

KK <- 100
no_fairness_summary <- matrix(nrow = KK, ncol = 8)
group_fairness_summary <- array(dim = c(KK, 2, 8))
individual_fairness_summary <- array(dim = c(KK, 2, 8))
race_fairness_summary <- array(dim = c(KK, 2, 8))

n <- 100

for (ii in seq_len(KK)){
  print(ii)
  sim <- data_sim2(data, n)
  X <- sim[[1]]
  wt <- rowSums(X)

  hwn <- c(X %*% as.numeric((sim[[3]] == "H") & (sim[[2]] == "W")))
  hbn <- c(X %*% as.numeric((sim[[3]] == "H") & (sim[[2]] == "B")))
  mwn <- c(X %*% as.numeric((sim[[3]] == "M") & (sim[[2]] == "W")))
  mbn <- c(X %*% as.numeric((sim[[3]] == "M") & (sim[[2]] == "B")))
  lwn <- c(X %*% as.numeric((sim[[3]] == "L") & (sim[[2]] == "W")))
  lbn <- c(X %*% as.numeric((sim[[3]] == "L") & (sim[[2]] == "B")))

  nhw <- sum((sim[[3]] == "H") & (sim[[2]] == "W"))
  nhb <- sum((sim[[3]] == "H") & (sim[[2]] == "B"))
  nmw <- sum((sim[[3]] == "M") & (sim[[2]] == "W"))
  nmb <- sum((sim[[3]] == "M") & (sim[[2]] == "B"))
  nlw <- sum((sim[[3]] == "L") & (sim[[2]] == "W"))
  nlb <- sum((sim[[3]] == "L") & (sim[[2]] == "B"))

  no_fairness_result <- no_fairness(X, wt)
  no_fairness_summary[ii, 1] <- sum(no_fairness_result * wt)
  no_fairness_summary[ii, 2] <- safe_div(sum(no_fairness_result * hwn), nhw)
  no_fairness_summary[ii, 3] <- safe_div(sum(no_fairness_result * hbn), nhb)
  no_fairness_summary[ii, 4] <- safe_div(sum(no_fairness_result * mwn), nmw)
  no_fairness_summary[ii, 5] <- safe_div(sum(no_fairness_result * mbn), nmb)
  no_fairness_summary[ii, 6] <- safe_div(sum(no_fairness_result * lwn), nlw)
  no_fairness_summary[ii, 7] <- safe_div(sum(no_fairness_result * lbn), nlb)
  no_fairness_summary[ii, 8] <- compute_mad(X, no_fairness_result) # MAD

  group_fairness_summary[ii, 1, 1] <- no_fairness_summary[ii, 1]
  group_fairness_summary[ii, 1, 2] <- no_fairness_summary[ii, 2]
  group_fairness_summary[ii, 1, 3] <- no_fairness_summary[ii, 3]
  group_fairness_summary[ii, 1, 4] <- no_fairness_summary[ii, 4]
  group_fairness_summary[ii, 1, 5] <- no_fairness_summary[ii, 5]
  group_fairness_summary[ii, 1, 6] <- no_fairness_summary[ii, 6]
  group_fairness_summary[ii, 1, 7] <- no_fairness_summary[ii, 7]
  group_fairness_summary[ii, 1, 8] <- no_fairness_summary[ii, 8]

  for (jj in 1:5){
    group_fairness_result <- group_fairness(X, wt, hwn + hbn, jj)
    if (length(group_fairness_result) > 0){
      if (abs(sum(group_fairness_result * wt) - no_fairness_summary[ii, 1]) <= eq_tol){
        group_fairness_summary[ii, 1, 1] <- sum(group_fairness_result * wt)
        group_fairness_summary[ii, 1, 2] <- safe_div(sum(group_fairness_result * hwn), nhw)
        group_fairness_summary[ii, 1, 3] <- safe_div(sum(group_fairness_result * hbn), nhb)
        group_fairness_summary[ii, 1, 4] <- safe_div(sum(group_fairness_result * mwn), nmw)
        group_fairness_summary[ii, 1, 5] <- safe_div(sum(group_fairness_result * mbn), nmb)
        group_fairness_summary[ii, 1, 6] <- safe_div(sum(group_fairness_result * lwn), nlw)
        group_fairness_summary[ii, 1, 7] <- safe_div(sum(group_fairness_result * lbn), nlb)
        group_fairness_summary[ii, 1, 8] <- compute_mad(X, group_fairness_result)
      }
      group_fairness_summary[ii, 2, 1] <- sum(group_fairness_result * wt)
      group_fairness_summary[ii, 2, 2] <- safe_div(sum(group_fairness_result * hwn), nhw)
      group_fairness_summary[ii, 2, 3] <- safe_div(sum(group_fairness_result * hbn), nhb)
      group_fairness_summary[ii, 2, 4] <- safe_div(sum(group_fairness_result * mwn), nmw)
      group_fairness_summary[ii, 2, 5] <- safe_div(sum(group_fairness_result * mbn), nmb)
      group_fairness_summary[ii, 2, 6] <- safe_div(sum(group_fairness_result * lwn), nlw)
      group_fairness_summary[ii, 2, 7] <- safe_div(sum(group_fairness_result * lbn), nlb)
      group_fairness_summary[ii, 2, 8] <- compute_mad(X, group_fairness_result)
    }
  }

  base_obj <- no_fairness_summary[ii, 1]

  ind_keep_same <- individual_fairness_minMAD(X, wt, base_obj = base_obj, alpha_keep = 1.00)

  ind_strong    <- individual_fairness_minMAD(X, wt, base_obj = base_obj, alpha_keep = 0.80)

  if (length(ind_keep_same) == 0) ind_keep_same <- no_fairness_result
  if (length(ind_strong)    == 0) ind_strong    <- ind_keep_same

  individual_fairness_summary[ii, 2, 1] <- sum(ind_keep_same * wt)
  individual_fairness_summary[ii, 2, 2] <- safe_div(sum(ind_keep_same * hwn), nhw)
  individual_fairness_summary[ii, 2, 3] <- safe_div(sum(ind_keep_same * hbn), nhb)
  individual_fairness_summary[ii, 2, 4] <- safe_div(sum(ind_keep_same * mwn), nmw)
  individual_fairness_summary[ii, 2, 5] <- safe_div(sum(ind_keep_same * mbn), nmb)
  individual_fairness_summary[ii, 2, 6] <- safe_div(sum(ind_keep_same * lwn), nlw)
  individual_fairness_summary[ii, 2, 7] <- safe_div(sum(ind_keep_same * lbn), nlb)
  individual_fairness_summary[ii, 2, 8] <- compute_mad(X, ind_keep_same)

  individual_fairness_summary[ii, 1, 1] <- sum(ind_strong * wt)
  individual_fairness_summary[ii, 1, 2] <- safe_div(sum(ind_strong * hwn), nhw)
  individual_fairness_summary[ii, 1, 3] <- safe_div(sum(ind_strong * hbn), nhb)
  individual_fairness_summary[ii, 1, 4] <- safe_div(sum(ind_strong * mwn), nmw)
  individual_fairness_summary[ii, 1, 5] <- safe_div(sum(ind_strong * mbn), nmb)
  individual_fairness_summary[ii, 1, 6] <- safe_div(sum(ind_strong * lwn), nlw)
  individual_fairness_summary[ii, 1, 7] <- safe_div(sum(ind_strong * lbn), nlb)
  individual_fairness_summary[ii, 1, 8] <- compute_mad(X, ind_strong)

  l_weak   <- 0.5 * c(1 / max(1, min(nhw, nhb)), 1 / max(1, min(nmw, nmb)), 1 / max(1, min(nlw, nlb)))
  l_strong <- 0.5 * c(1 / max(1, max(nhw, nhb)), 1 / max(1, max(nmw, nmb)), 1 / max(1, max(nlw, nlb)))

  race_w <- race_fairness(X, wt, hwn, hbn, mwn, mbn, lwn, lbn,
                          nhw, nhb, nmw, nmb, nlw, nlb, ell = l_weak)
  race_s <- race_fairness(X, wt, hwn, hbn, mwn, mbn, lwn, lbn,
                          nhw, nhb, nmw, nmb, nlw, nlb, ell = l_strong)

  race_fairness_summary[ii, 2, 1] <- sum(race_w * wt)
  race_fairness_summary[ii, 2, 2] <- safe_div(sum(race_w * hwn), nhw)
  race_fairness_summary[ii, 2, 3] <- safe_div(sum(race_w * hbn), nhb)
  race_fairness_summary[ii, 2, 4] <- safe_div(sum(race_w * mwn), nmw)
  race_fairness_summary[ii, 2, 5] <- safe_div(sum(race_w * mbn), nmb)
  race_fairness_summary[ii, 2, 6] <- safe_div(sum(race_w * lwn), nlw)
  race_fairness_summary[ii, 2, 7] <- safe_div(sum(race_w * lbn), nlb)
  race_fairness_summary[ii, 2, 8] <- compute_mad(X, race_w)

  race_fairness_summary[ii, 1, 1] <- sum(race_s * wt)
  race_fairness_summary[ii, 1, 2] <- safe_div(sum(race_s * hwn), nhw)
  race_fairness_summary[ii, 1, 3] <- safe_div(sum(race_s * hbn), nhb)
  race_fairness_summary[ii, 1, 4] <- safe_div(sum(race_s * mwn), nmw)
  race_fairness_summary[ii, 1, 5] <- safe_div(sum(race_s * mbn), nmb)
  race_fairness_summary[ii, 1, 6] <- safe_div(sum(race_s * lwn), nlw)
  race_fairness_summary[ii, 1, 7] <- safe_div(sum(race_s * lbn), nlb)
  race_fairness_summary[ii, 1, 8] <- compute_mad(X, race_s)
}