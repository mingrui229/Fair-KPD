compute_mad <- function(X, w){
  # w: selection over cycles (binary x or fractional w)
  d <- as.numeric(t(X) %*% w)          # per-patient selection probability
  mean(abs(d - mean(d)))
}

individual_fairness_minMAD <- function(X, wt, base_obj, alpha_keep = 1.00){
  # Minimize mean absolute deviation of patient selection probabilities
  # subject to preserving alpha_keep * base_obj utility.
  require(gurobi); require(Matrix)
  M <- nrow(X); N <- ncol(X)
  X <- Matrix::Matrix(X, sparse = TRUE)

  # vars: w (M), d (N), z (N), dbar (1)
  nvar <- M + N + N + 1
  idx_w    <- 1:M
  idx_d    <- (M+1):(M+N)
  idx_z    <- (M+N+1):(M+2*N)
  idx_dbar <- M+2*N+1

  # Objective: min (1/N) * sum z
  obj <- numeric(nvar); obj[idx_z] <- 1 / N

  A_list <- list(); rhs <- c(); sense <- c()

  # (1) capacity: X^T w <= 1
  A_list[[length(A_list)+1]] <- cbind(t(X), Matrix::Matrix(0, nrow=N, ncol=N+N+1, sparse=TRUE))
  rhs   <- c(rhs, rep(1, N))
  sense <- c(sense, rep('<=', N))

  # (2) d - X^T w = 0
  A2 <- cbind(-t(X), Matrix::Diagonal(N), Matrix::Matrix(0, nrow=N, ncol=N), Matrix::Matrix(0, nrow=N, ncol=1))
  A_list[[length(A_list)+1]] <- A2
  rhs   <- c(rhs, rep(0, N))
  sense <- c(sense, rep('=', N))

  # (3) N*dbar - sum_v d_v = 0
  a3 <- Matrix::Matrix(0, nrow=1, ncol=nvar, sparse=TRUE)
  a3[1, idx_d]    <- -1
  a3[1, idx_dbar] <-  N
  A_list[[length(A_list)+1]] <- a3
  rhs   <- c(rhs, 0)
  sense <- c(sense, '=')

  # (4) z - d + dbar >= 0
  A4 <- Matrix::Matrix(0, nrow=N, ncol=nvar, sparse=TRUE)
  A4[cbind(1:N, idx_z)]    <-  1
  A4[cbind(1:N, idx_d)]    <- -1
  A4[cbind(1:N, idx_dbar)] <-  1
  A_list[[length(A_list)+1]] <- A4
  rhs   <- c(rhs, rep(0, N))
  sense <- c(sense, rep('>=', N))

  # (5) z + d - dbar >= 0
  A5 <- Matrix::Matrix(0, nrow=N, ncol=nvar, sparse=TRUE)
  A5[cbind(1:N, idx_z)]    <-  1
  A5[cbind(1:N, idx_d)]    <-  1
  A5[cbind(1:N, idx_dbar)] <- -1
  A_list[[length(A_list)+1]] <- A5
  rhs   <- c(rhs, rep(0, N))
  sense <- c(sense, rep('>=', N))

  # (6) utility floor on total utility: sum(w * wt) >= alpha_keep * base_obj
  a6 <- Matrix::Matrix(0, nrow=1, ncol=nvar, sparse=TRUE)
  a6[1, idx_w] <- wt
  A_list[[length(A_list)+1]] <- a6
  rhs   <- c(rhs, alpha_keep * base_obj)
  sense <- c(sense, '>=')

  model <- list(
    obj = obj,
    modelsense = 'min',
    A   = do.call(rbind, A_list),
    rhs = rhs,
    sense = sense,
    lb = rep(0, nvar),
    ub = c(rep(1, M), rep(Inf, N + N + 1))
  )
  res <- gurobi(model, params = list(OutputFlag = 0))
  if (is.null(res$x)) return(numeric(0))
  as.numeric(res$x[idx_w])   # return w
}
