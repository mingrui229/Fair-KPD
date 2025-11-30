source("no_fairness.R")
source("group_fairness.R")
source("individual_fairness.R")
source("new_fairness.R")
source("get_cycles.R")
source("compatibility.R")

safe_div <- function(num, den) if (den > 0) num / den else NA
eq_tol <- 1e-6

data_sim <- function(n){
  race <- c(rep("W", n * 0.8), rep("B", n * 0.2))
  sens_level <- c(rep("L", n * 0.56), rep("M", n * 0.16), rep("H", n * 0.08),
                  rep("L", n * 0.14), rep("M", n * 0.04), rep("H", n * 0.02))
  pc <- c(rep(0.05, n * 0.56), rep(0.45, n * 0.16), rep(0.9, n * 0.08),
          rep(0.05, n * 0.14), rep(0.45, n * 0.04), rep(0.9, n * 0.02))

  idxB <- which(race == "B")
  idxW <- which(race == "W")
  map <- c("O","A","B","AB")
  pP_B <- rmultinom(length(idxB), 1, c(0.51, 0.26, 0.19, 0.04))
  pP_W <- rmultinom(length(idxW), 1, c(0.45, 0.40, 0.11, 0.04))
  blood_type_p <- character(n)
  blood_type_p[idxB] <- map[max.col(t(pP_B))]
  blood_type_p[idxW] <- map[max.col(t(pP_W))]

  blood_type_d <- rep(NA_character_, n)
  i <- 1
  while (i <= n){
    if (race[i] == "B"){
      draw <- rmultinom(1, 1, c(0.51, 0.26, 0.19, 0.04))
    } else {
      draw <- rmultinom(1, 1, c(0.45, 0.40, 0.11, 0.04))
    }
    blood_type_d[i] <- map[which(draw == 1)]
    if ((!bt_matching(blood_type_d[i], blood_type_p[i])) | (runif(1) < pc[i])){
      i <- i + 1
    }
  }

  bt_ok <- outer(blood_type_d, blood_type_p, Vectorize(bt_matching))
  diag(bt_ok) <- FALSE

  accept <- matrix(runif(n*n), n, n) > matrix(rep(pc, each = n), n, n)
  diag(accept) <- FALSE

  keep <- bt_ok & accept
  Edg <- as.data.frame(which(keep, arr.ind = TRUE))
  colnames(Edg) <- c("X1","X2")

  cycles <- get_cycles(n, Edg, 3)
  number_of_cycles <- length(cycles)
  X <- matrix(0, nrow = number_of_cycles, ncol = n)
  for (i in seq_len(number_of_cycles)){
    X[i, cycles[[i]]] <- 1
  }
  return(list(X, race, sens_level, cycles))
}

set.seed(1)

KK <- 100
no_fairness_summary <- matrix(nrow = KK, ncol = 8)
group_fairness_summary <- array(dim = c(KK, 2, 8))
individual_fairness_summary <- array(dim = c(KK, 2, 8))
race_fairness_summary <- array(dim = c(KK, 2, 8))

n <- 50

for (ii in seq_len(KK)){
  print(ii)
  sim <- data_sim(n)

  vp <- runif(n, 0, 0.3)
  ep <- matrix(runif(n^2, 0, 0.3), n, n)
  RS <- relevant_subset(sim[[4]])
  wt <- vapply(RS, expect_cal, numeric(1), cycle_list = sim[[4]], vp = vp, ep = ep)
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
  no_fairness_summary[ii, 8] <- compute_mad(X, no_fairness_result)

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

