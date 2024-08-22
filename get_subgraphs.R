### expectation calculation
expect_cal <- function(ss, cycle_list, vp, ep){
  fun0 <- function(x1, x2, m){
    if (x1 == 1)
      return(x2 - 1)
    if (x1 > 1){
      s <- 0
      for (j in c(1: (x1-1))){
        s <- s + m - j
      }
      return(s + x2 - x1)
    }
  }
  
  fun1 <- function(x, i){
    y <- rep(NA, i)
    for (j in c(1: i)){
      y[j] <- as.numeric(x >= 2^(i-j))
      if (x >= 2^(i-j))
        x <- x - 2^(i-j)
    }
    return(i + 1 - which(y == 1))
  }
  
  m <- length(ss)
  candidate <- list()
  i <- 1
  for (cycle in cycle_list){
    if (all(cycle %in% ss)){
      candidate[[i]] <- cycle
      i <- i + 1
    }
  }
  i <- i - 1
  raw_exp <- sapply(candidate, length)
  ord <- order(raw_exp, decreasing = T)
  
  raw_exp <- raw_exp[ord]
  candidate <- candidate[ord]
  
  dict1 <- array(0, dim = c(2^i-1, i))
  for (j in c(1: (2^i-1))){
    dict1[j, fun1(j, i)] <- 1
  }
  dict2 <- apply(dict1, 1, sum)
  dict3 <- apply(dict1, 1, function(x) max(which(x==1)))
  
  failure_table <- matrix(0, nrow = i, ncol = m + m * (m-1) / 2)
  prob_table <- rep(0, m + m * (m-1) / 2)
  for (k in c(1: m)){
    prob_table[k] <- vp[ss[k]]
  }
  jj <- 1
  for (k1 in c(1: (m-1)))
    for (k2 in c((k1+1): m)){
      prob_table[m+jj] <- ep[ss[k1], ss[k2]]
      jj <- jj + 1
    }
  
  for (j in c(1: i)){
    vj <- which(ss %in% candidate[[j]])
    failure_table[j, vj] <- 1
    for (k1 in c(1: (length(vj)-1)))
      for (k2 in c((k1+1): length(vj))){
        failure_table[j, m+fun0(vj[k1], vj[k2], m)] <- 1
      }
  }
  
  pc <- rep(NA, 2^i-1)
  for (j in c(1: (2^i-1))){
    if (length(fun1(j, i)) == 1){
      pc[j] <- prod(1-prob_table[which(failure_table[fun1(j, i), ] == 1)])
    }else{
      pc[j] <- prod(1-prob_table[unique(which(failure_table[fun1(j, i), ] == 1, arr.ind = T)[, 2])])
    }
  }
  pc2 <- rep(0, i)
  pc2[1] <- pc[1]
  
  if (i > 1){
    for (j in c(2: i)){
      for (k in c(1: j)){
        pc2[j] <- pc2[j] - (-1)^k * sum(pc[which((dict2 == k) & (dict1[, j] == 1) & (dict3 <= j))])
      }
    }
  }
  return(sum(pc2 * raw_exp))
}


### relevant subset
relevant_subset <- function(cycle_list){
  cycle_list <- sapply(cycle_list, sort, simplify = F)
  cl <- sapply(cycle_list, length)
  c3 <- matrix(unlist(cycle_list[which(cl == 3)]), byrow = T, ncol = 3)
  c2 <- matrix(unlist(cycle_list[which(cl == 2)]), byrow = T, ncol = 2)
  d3 <- nrow(c3)
  d2 <- nrow(c2)
  
  Omega1 <- matrix(nrow = 0, ncol = 4)
  for (i in c(1: d3)){
    for (j in c(1: d3)){
      if (length(unique(c(c3[i, ], c3[j, ]))) == 4){
        Omega1 <- rbind(Omega1, sort(unique(c(c3[i, ], c3[j, ]))))
      }
    }
  }
  Omega1 <- Omega1[!duplicated(Omega1), ]
  print(dim(Omega1))
  
  Omega2 <- matrix(nrow = 0, ncol = 3)
  for (i in c(1: d2)){
    for (j in c(1: d2)){
      if (length(unique(c(c2[i, ], c2[j, ]))) == 3){
        Omega2 <- rbind(Omega2, sort(unique(c(c2[i, ], c2[j, ]))))
      }
    }
  }
  Omega2 <- rbind(Omega2, c3)
  Omega2 <- Omega2[!duplicated(Omega2), ]
  print(dim(Omega2))
  
  Omega3 <- matrix(nrow = 0, ncol = 4)
  for (i in c(1: nrow(Omega2))){
    for (j in c(1: d2)){
      if (length(unique(c(Omega2[i, ], c2[j, ]))) == 4){
        Omega3 <- rbind(Omega3, sort(unique(c(Omega2[i, ], c2[j, ]))))
      }
    }
  }
  Omega3 <- rbind(Omega3, Omega1)
  Omega3 <- Omega3[!duplicated(Omega3), ]
  l1 <- unname(as.list(as.data.frame(t(c2))))
  l2 <- unname(as.list(as.data.frame(t(Omega2))))
  l3 <- unname(as.list(as.data.frame(t(Omega3))))
  return(c(l1, l2, l3))
}