race_fairness <- function(X, wt,
                          hwn, hbn, mwn, mbn, lwn, lbn,
                          nhw, nhb, nmw, nmb, nlw, nlb,
                          ell,
                          seed_init      = 5000,
                          max_cols       = 50000,
                          cg_max_iter    = 10000,
                          add_topK       = 20000,       # number of best columns to add per pricing
                          rc_tol         = 1e-8,       # pricing rc threshold
                          early_gap      = 1e-6,       # stop when UB − incumbent ≤ this
                          do_milp        = TRUE,       # if FALSE: use rounding heuristic
                          polish_loops   = 100,         # MILP↔pricing alternations
                          mip_gap        = 5e-4,       # relative MILP MIPGap
                          mip_time_limit = 60,       # seconds per MILP
                          threads        = max(1L, parallel::detectCores(logical = TRUE) - 1L),
                          time_limit_sec = 400,       # global wall-clock for the whole call
                          # pass extra Gurobi params if desired
                          gurobi_params  = list(OutputFlag = 0)) {

  if (!requireNamespace("Matrix", quietly = TRUE)) stop("Package 'Matrix' is required.")
  if (!requireNamespace("gurobi", quietly = TRUE)) stop("Package 'gurobi' is required.")

  tic0 <- proc.time()[3]
  elapsed <- function() proc.time()[3] - tic0
  time_left <- function() max(0, time_limit_sec - elapsed())

  M <- nrow(X); N <- ncol(X)

  # fairness coefficients per cycle (avg W − avg B within stratum)
  mk_phi <- function(num_w, num_b, n_w, n_b) {
    if (n_w > 0 && n_b > 0) as.numeric(num_w) / n_w - as.numeric(num_b) / n_b
    else rep(0, length(num_w))
  }
  phi_H <- mk_phi(hwn, hbn, nhw, nhb)
  phi_M <- mk_phi(mwn, mbn, nmw, nmb)
  phi_L <- mk_phi(lwn, lbn, nlw, nlb)

  use_H <- (nhw > 0 && nhb > 0 && is.finite(ell[1]))
  use_M <- (nmw > 0 && nmb > 0 && is.finite(ell[2]))
  use_L <- (nlw > 0 && nlb > 0 && is.finite(ell[3]))

  # seed columns: best utilities + a random sprinkle to diversify
  ord <- order(wt, decreasing = TRUE)
  J <- head(ord, min(seed_init, M))
  if (M > seed_init) {
    J <- sort(unique(c(J, sample.int(M, min(seed_init, M)))))
  }
  cap <- function(S) if (length(S) > max_cols) S[seq_len(max_cols)] else S

  # LP RMP (current columns J)
  build_lp <- function(Jset) {
    A_rows <- list(); sense <- character(); rhs <- numeric()
    A_rows[[length(A_rows)+1L]] <- Matrix::Matrix(t(X[Jset, , drop = FALSE]), sparse = TRUE)
    sense <- c(sense, rep('<=', N)); rhs <- c(rhs, rep(1, N))

    add_fair <- function(phi, bnd) {
      A_rows[[length(A_rows)+1L]] <<- Matrix::Matrix(matrix(phi[Jset], nrow = 1L), sparse = TRUE)
      sense <<- c(sense, '<='); rhs <<- c(rhs, bnd)
      A_rows[[length(A_rows)+1L]] <<- Matrix::Matrix(matrix(-phi[Jset], nrow = 1L), sparse = TRUE)
      sense <<- c(sense, '<='); rhs <<- c(rhs, bnd)
    }
    if (use_H) add_fair(phi_H, ell[1])
    if (use_M) add_fair(phi_M, ell[2])
    if (use_L) add_fair(phi_L, ell[3])

    A <- if (length(A_rows) == 1L) A_rows[[1L]] else do.call(rbind, A_rows)
    list(
      obj        = wt[Jset],
      modelsense = 'max',
      A          = A,
      sense      = sense,
      rhs        = rhs,
      lb         = rep(0, length(Jset)),
      ub         = rep(1, length(Jset))
    )
  }

  # extract duals respecting constraint order: N vertex rows, then +/- fairness rows
  get_duals <- function(res) {
    pi <- res$pi; idx <- 0L
    lambda <- pi[(idx + 1L):(idx + N)]; idx <- idx + N
    grab <- function(on) {
      if (!on) return(c(0, 0))
      c(pi[idx + 1L], pi[idx + 2L]); idx <<- idx + 2L
    }
    dH <- grab(use_H); dM <- grab(use_M); dL <- grab(use_L)
    list(lambda = as.numeric(lambda),
         aH_pos = dH[1], aH_neg = dH[2],
         aM_pos = dM[1], aM_neg = dM[2],
         aL_pos = dL[1], aL_neg = dL[2])
  }

  # reduced costs for all not-in-J
  price_all <- function(Jset, duals) {
    inJ <- logical(M); inJ[Jset] <- TRUE
    notJ <- which(!inJ)
    if (!length(notJ)) return(list(idx = integer(0), rc = numeric(0)))

    vterm <- as.numeric(X[notJ, , drop = FALSE] %*% duals$lambda)
    fH <- if (use_H) (duals$aH_pos * phi_H[notJ] + duals$aH_neg * (-phi_H[notJ])) else 0
    fM <- if (use_M) (duals$aM_pos * phi_M[notJ] + duals$aM_neg * (-phi_M[notJ])) else 0
    fL <- if (use_L) (duals$aL_pos * phi_L[notJ] + duals$aL_neg * (-phi_L[notJ])) else 0
    rc  <- wt[notJ] - vterm - fH - fM - fL
    list(idx = notJ, rc = as.numeric(rc))
  }

  solve_lp <- function(Jset, timelim = Inf) {
    model_lp <- build_lp(Jset)
    par_lp <- utils::modifyList(list(
      OutputFlag = 0, Method = 1,   # dual simplex tends to be fast with repeated solves
      Threads = threads
    ), gurobi_params)
    if (is.finite(timelim)) par_lp$TimeLimit <- max(0, timelim)
    gurobi::gurobi(model_lp, params = par_lp)
  }

  # greedy rounding + repair (fast fallback)
  greedy_round <- function(xfrac, Jset) {
    # score cycles by fractional value * utility
    s <- xfrac * wt[Jset]
    ord <- order(s, decreasing = TRUE)
    sel <- integer(0); used <- rep(FALSE, N)
    # prefetch rows
    XJ <- X[Jset, , drop = FALSE]
    for (idx in ord) {
      row <- XJ[idx, ]
      if (all(!used[row@i + 1L])) {   # Matrix sparse pattern
        sel <- c(sel, idx)
        if (length(row@i)) used[row@i + 1L] <- TRUE
      }
    }
    x <- numeric(M); x[Jset[sel]] <- 1
    x
  }

  # ---------- Phase 1: LP column generation ----------
  incumbent <- -Inf
  iter <- 0L
  repeat {
    if (elapsed() >= time_limit_sec) break
    iter <- iter + 1L
    J <- cap(J)
    res_lp <- tryCatch(solve_lp(J, timelim = time_left()), error = function(e) NULL)
    if (is.null(res_lp) || is.null(res_lp$x) || is.null(res_lp$pi)) return(numeric(0))

    UB <- as.numeric(res_lp$objval)
    if (!is.null(res_lp$x)) {
      # trivial rounding for a quick incumbent bound
      x_frac <- as.numeric(res_lp$x)
      inc_try <- sum((x_frac > 0.999) * wt[J])
      if (inc_try > incumbent) incumbent <- inc_try
    }
    if (UB - incumbent <= early_gap) break

    duals  <- get_duals(res_lp)
    priced <- price_all(J, duals)
    if (!length(priced$idx)) break

    # add only the best K improving columns
    add_idx <- which(priced$rc > rc_tol)
    if (!length(add_idx) || iter >= cg_max_iter) break
    pick <- add_idx[order(priced$rc[add_idx], decreasing = TRUE)]
    pick <- head(pick, min(add_topK, length(pick)))
    J <- sort(unique(c(J, priced$idx[pick])))
  }

  # If time almost gone or MILP disabled, do greedy rounding and return
  if (!do_milp || time_left() < 2.0) {
    # one more LP (cheap) to get fractions, then greedy
    res_lp <- tryCatch(solve_lp(J, timelim = time_left()), error = function(e) NULL)
    if (is.null(res_lp) || is.null(res_lp$x)) return(numeric(0))
    x_int <- greedy_round(as.numeric(res_lp$x), J)
    return(x_int)
  }

  # ---------- Phase 2: MILP polishing loop with time budget ----------
  build_milp <- function(Jset) {
    A_rows <- list(); sense <- character(); rhs <- numeric()
    A_rows[[length(A_rows)+1L]] <- Matrix::Matrix(t(X[Jset, , drop = FALSE]), sparse = TRUE)
    sense <- c(sense, rep('<=', N)); rhs <- c(rhs, rep(1, N))
    add_fair_J <- function(phi, bnd) {
      A_rows[[length(A_rows)+1L]] <<- Matrix::Matrix(matrix(phi[Jset], nrow = 1L), sparse = TRUE)
      sense <<- c(sense, '<='); rhs <<- c(rhs, bnd)
      A_rows[[length(A_rows)+1L]] <<- Matrix::Matrix(matrix(-phi[Jset], nrow = 1L), sparse = TRUE)
      sense <<- c(sense, '<='); rhs <<- c(rhs, bnd)
    }
    if (use_H) add_fair_J(phi_H, ell[1])
    if (use_M) add_fair_J(phi_M, ell[2])
    if (use_L) add_fair_J(phi_L, ell[3])
    A_f <- if (length(A_rows) == 1L) A_rows[[1L]] else do.call(rbind, A_rows)
    list(obj        = wt[Jset],
         modelsense = 'max',
         A          = A_f,
         sense      = sense,
         rhs        = rhs,
         vtype      = 'B')
  }

  best_x <- numeric(M); best_val <- -Inf
  loops <- 0L
  repeat {
    if (elapsed() >= time_limit_sec || loops >= polish_loops) break
    loops <- loops + 1L

    model_ilp <- build_milp(J)
    par_ilp <- utils::modifyList(list(
      OutputFlag = 0, Threads = threads,
      MIPGap = mip_gap,
      TimeLimit = min(mip_time_limit, time_left())
    ), gurobi_params)

    res_ilp <- tryCatch(gurobi::gurobi(model_ilp, params = par_ilp), error = function(e) NULL)
    if (is.null(res_ilp) || is.null(res_ilp$x)) break

    xJ <- as.numeric(res_ilp$x > 0.99)
    val <- sum(xJ * wt[J])
    if (val > best_val) {
      best_val <- val
      best_x <- numeric(M); best_x[J] <- xJ
    }

    # price again on LP duals; stop if no improving columns
    res_lp <- tryCatch(solve_lp(J, timelim = time_left()), error = function(e) NULL)
    if (is.null(res_lp) || is.null(res_lp$pi)) break

    duals  <- get_duals(res_lp)
    priced <- price_all(J, duals)
    add_idx <- if (length(priced$idx)) which(priced$rc > rc_tol) else integer(0)
    if (!length(add_idx)) break

    pick <- add_idx[order(priced$rc[add_idx], decreasing = TRUE)]
    pick <- head(pick, min(add_topK, length(pick)))
    J <- sort(unique(c(J, priced$idx[pick])))
    J <- cap(J)
  }

  if (best_val > -Inf) return(best_x)

  # Fallback: greedy rounding from last LP
  res_lp <- tryCatch(solve_lp(J, timelim = time_left()), error = function(e) NULL)
  if (is.null(res_lp) || is.null(res_lp$x)) return(numeric(0))
  greedy_round(as.numeric(res_lp$x), J)
}
