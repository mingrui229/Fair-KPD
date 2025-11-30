no_fairness <- function(X, wt){
  require(gurobi)
  M <- nrow(X); N <- ncol(X)
  model <- list()
  model$obj        <- wt
  model$modelsense <- 'max'
  model$A          <- t(X)
  model$rhs        <- rep(1, N)
  model$sense      <- rep('<=', N)
  model$vtype      <- 'B'
  result <- gurobi(model, params = list(OutputFlag = 0))
  return(as.numeric(result$x > 0.99))
}
