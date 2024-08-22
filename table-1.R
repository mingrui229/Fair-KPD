### calculate MSE

mse <- matrix(NA, nrow = 4, ncol = KK)
for (i in c(1: 4))
  for (j in c(1: KK)){
    mse[i,j] <- mean((apply(est[[j]][[i]], 2, mean) - truth[[j]][1: (i*20)])^2)
  }

apply(mse, 1, mean)

### calculate coverage and width
cover <- matrix(NA, nrow = 4, ncol = KK)
width <- matrix(NA, nrow = 4, ncol = KK)
for (i in c(1: 4))
  for (j in c(1: KK)){
    ind <- as.numeric(apply(est[[j]][[i]], 2, min) == 0)
    upper <- (ind * apply(est[[j]][[i]], 2, quantile, 0.95) +
                (1-ind) * apply(est[[j]][[i]], 2, max))
    lower <- (ind * 0 + (1-ind) * apply(est[[j]][[i]], 2, quantile, 0.05))
    cover[i,j] <- mean((upper>=truth[[j]][1: (i*20)]) * ((lower<=truth[[j]][1: (i*20)])))
    width[i,j] <- mean(upper - lower)
  }

apply(cover, 1, mean)
apply(width, 1, mean)

