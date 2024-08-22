no_fairness <- function(X, wt){
  require(gurobi)
  M <- dim(X)[1]
  N <- dim(X)[2]
  model <- list()
  model$obj        <- wt
  model$modelsense <- 'max'
  model$A          <- t(X)
  model$rhs        <- rep(1, N)
  model$sense      <- rep('<=', N)
  model$vtype      <- 'B'
  result <- gurobi(model)
  return(as.numeric(result$x > 0.99))
}