individual_fairness1 <- function(X, wt){
  require(gurobi)
  M <- dim(X)[1]
  N <- dim(X)[2]
  sol <- matrix(nrow = 0, ncol = M)
  sol2 <- matrix(nrow = N, ncol = 0)
  for (i in c(1: N)){
    model <- list()
    model$obj        <- wt
    model$modelsense <- 'max'
    model$A          <- rbind(t(X), 1-sol)
    model$sense      <- c(rep('<=', N), rep(">=", dim(sol)[1]))
    model$sense[i]   <- "="
    model$rhs        <- c(rep(1, N), rep(1, dim(sol)[1]))
    model$vtype      <- 'B'
    result <- gurobi(model)
    sol <- rbind(sol, as.numeric(result$x > 0.99))
    if (length(which(result$x > 0.99)) > 0){
      v <- rep(0, N)
      for (j in which(result$x > 0.99)){
        v[which(X[j, ] == 1)] <- 1
      }
      sol2 <- cbind(sol2, v)
    }
    
  }
  return(list(sol, sol2, wt, N))
}

individual_fairness2 <- function(sol, sol2, wt, N, para){
  k <- dim(sol)[1]
  model2 <- list()
  model2$obj <- sol %*% wt
  model2$modelsense <- 'max'
  model2$A <- rbind(diag(1, k), rep(1, k))
  model2$rhs <- c(rep(0, k), 1)
  model2$sense <- c(rep('>=', k), '<=')
  qc1 <- list()
  qc1$Qc <- 1/N * (t(sol2) %*% (diag(N) - matrix(1/N, nrow = N, ncol = N)) %*% sol2)
  qc1$rhs <- para
  qc1$sense <- c('<=')
  qc1$name <- 'fairness'
  model2$quadcon <- list(qc1)
  model2$vtype <- rep('C', k)
  result <- gurobi(model2)
  return(list(sol, result$x))
}