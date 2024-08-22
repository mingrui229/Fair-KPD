group_fairness <- function(X, wt, hn, para){
  require(gurobi)
  M <- dim(X)[1]
  N <- dim(X)[2]
  model <- list()
  model$obj        <- wt
  model$modelsense <- 'max'
  model$A          <- rbind(t(X), hn)
  model$sense      <- c(rep('<=', N), ">=")
  model$rhs        <- c(rep(1, N), para)
  model$vtype      <- 'B'
  result <- gurobi(model)
  return(as.numeric(result$x > 0.99))
}