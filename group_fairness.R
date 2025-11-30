group_fairness <- function(X, wt, hn, para){
  require(gurobi)
  M <- nrow(X); N <- ncol(X)
  model <- list()
  model$obj        <- wt
  model$modelsense <- 'max'
  model$A          <- rbind(t(X), hn)       # hn is a 1 x M row
  model$sense      <- c(rep('<=', N), ">=")
  model$rhs        <- c(rep(1, N), para)    # absolute number of hard-to-match cycles
  model$vtype      <- 'B'
  result <- gurobi(model, params = list(OutputFlag = 0))
  return(as.numeric(result$x > 0.99))
}
