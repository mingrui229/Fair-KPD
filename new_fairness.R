race_fairness1 <- function(X, wt, dh_cycles, dm_cycles, dl_cycles){
  require(gurobi)
  M <- dim(X)[1]
  N <- dim(X)[2]
  sense_matrix <- rbind(c(rep('<=', N), c(">=", ">=", ">=")),
                        c(rep('<=', N), c(">=", ">=", "<=")),
                        c(rep('<=', N), c(">=", "<=", ">=")),
                        c(rep('<=', N), c(">=", "<=", "<=")),
                        c(rep('<=', N), c("<=", ">=", ">=")),
                        c(rep('<=', N), c("<=", ">=", "<=")),
                        c(rep('<=', N), c("<=", "<=", ">=")),
                        c(rep('<=', N), c("<=", "<=", "<=")))
  sol <- matrix(nrow = 8, ncol = M)
  for (i in c(1: 8)){
    model <- list()
    model$obj        <- wt
    model$modelsense <- 'max'
    model$A          <- rbind(t(X), dh_cycles, dm_cycles, dl_cycles)
    model$sense      <- sense_matrix[i, ]
    model$rhs        <- c(rep(1, N), c(0, 0, 0))
    ind <- apply(model$A, 1, function(x) any(is.na(x)))
    model$vtype      <- 'B'
    result <- gurobi(model)
    sol[i, ] <- as.numeric(result$x > 0.99)
  }
  return(sol)
}

race_fairness2 <- function(sol, wt, dh_cycles, dm_cycles, dl_cycles, l1, l2, l3){
  k <- dim(sol)[1]
  model2 <- list()
  model2$obj <- sol %*% wt
  model2$modelsense <- 'max'
  model2$A <- rbind(diag(1, k), rep(1, k), c(sol %*% dh_cycles), c(sol %*% dh_cycles), 
                    c(sol %*% dm_cycles), c(sol %*% dm_cycles),
                    c(sol %*% dl_cycles), c(sol %*% dl_cycles))
  ind <- apply(model2$A, 1, function(x) any(is.na(x)))
  model2$rhs <- c(rep(0, k), 1, l1, -l1, l2, -l2, l3, -l3)
  model2$sense <- c(rep('>=', k), '<=', "<=", ">=", "<=", ">=", "<=", ">=")
  model2$vtype <- rep('C', k)
  result <- gurobi(model2)
  return(list(sol, result$x))
}