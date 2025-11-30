pair_index <- function(a, b, m) {
  before_a <- (a - 1L) * m - (a - 1L) * a %/% 2L
  before_a + (b - a)
}

popcount <- function(x) sum(as.integer(intToBits(x)))
max_bit_pos <- function(x) as.integer(floor(log2(x))) + 1L

sanitize_probs <- function(v) {
  v[is.na(v)] <- 0
  v[v < 0] <- 0
  v[v > 1] <- 1
  v
}

# Assumes cycles of length <= 3
expect_cal <- function(ss, cycle_list, vp, ep) {
  m <- length(ss)
  if (m == 0L) return(0)

  candidate <- Filter(function(cyc) all(cyc %in% ss), cycle_list)
  if (!length(candidate)) return(0)
  raw_exp <- vapply(candidate, length, integer(1))
  ord     <- order(raw_exp, decreasing = TRUE)
  candidate <- candidate[ord]
  raw_exp   <- raw_exp[ord]
  i <- length(candidate)

  v_probs <- sanitize_probs(vp[ss])
  if (m >= 2L) {
    pairs <- utils::combn(m, 2L)
    e_probs <- sanitize_probs(ep[ss[pairs[1, ]], ss[pairs[2, ]]])
  } else {
    e_probs <- numeric(0)
  }
  prob_table <- c(v_probs, e_probs)
  one_minus  <- 1 - prob_table
  log_om     <- rep(0.0, length(one_minus))
  pos_idx    <- which(one_minus > 0)
  log_om[pos_idx] <- log(one_minus[pos_idx])

  cycle_features <- vector("list", i)
  for (j in seq_len(i)) {
    vj <- sort(match(candidate[[j]], ss))
    if (length(vj) >= 2L) {
      e_pairs <- utils::combn(vj, 2L)
      e_idx   <- pair_index(e_pairs[1, ], e_pairs[2, ], m)
    } else e_idx <- integer(0)
    cycle_features[[j]] <- c(vj, m + e_idx)
  }

  nsub <- bitwShiftL(1L, i) - 1L
  if (nsub <= 0L) return(0)

  P <- length(one_minus)
  cover    <- integer(P)
  in_set   <- logical(i)
  zero_cnt <- 0L
  log_sum  <- 0.0
  pc2      <- numeric(i)

  add_feats <- function(feats) {
    idx0 <- feats[cover[feats] == 0L]
    if (length(idx0)) {
      zero_cnt <<- zero_cnt + sum(one_minus[idx0] == 0)
      pos <- idx0[one_minus[idx0] > 0]
      if (length(pos)) log_sum <<- log_sum + sum(log_om[pos])
    }
    cover[feats] <<- cover[feats] + 1L
  }
  remove_feats <- function(feats) {
    idx1 <- feats[cover[feats] == 1L]
    if (length(idx1)) {
      zero_cnt <<- zero_cnt - sum(one_minus[idx1] == 0)
      pos <- idx1[one_minus[idx1] > 0]
      if (length(pos)) log_sum <<- log_sum - sum(log_om[pos])
    }
    cover[feats] <<- cover[feats] - 1L
  }
  current_prod <- function() if (zero_cnt > 0L) 0.0 else exp(log_sum)

  toggle <- function(t) {
    feats <- cycle_features[[t]]
    if (!in_set[t]) { add_feats(feats); in_set[t] <<- TRUE }
    else            { remove_feats(feats); in_set[t] <<- FALSE }
  }

  g_prev <- 0L
  for (k in seq_len(nsub)) {
    g_curr <- bitwXor(k, bitwShiftR(k, 1L))
    diff   <- bitwXor(g_prev, g_curr)
    t      <- max_bit_pos(diff)
    toggle(t)

    s    <- popcount(g_curr)
    jmax <- max_bit_pos(g_curr)
    pc2[jmax] <- pc2[jmax] + ((-1)^(s + 1L)) * current_prod()

    g_prev <- g_curr
  }

  sum(pc2 * raw_exp)
}


.key2 <- function(a, b) { if (a > b) paste0(b, ",", a) else paste0(a, ",", b) }
.key3 <- function(a, b, c) { v <- sort(c(a, b, c)); paste(v, collapse = ",") }
.key4 <- function(a, b, c, d) { v <- sort(c(a, b, c, d)); paste(v, collapse = ",") }
.parse_vec <- function(key) as.integer(strsplit(key, ",", fixed = TRUE)[[1L]])

relevant_subset <- function(cycle_list) {
  if (!length(cycle_list)) return(list())
  cycle_list <- lapply(cycle_list, function(z) sort(as.integer(z)))
  cl <- lengths(cycle_list)
  C2 <- cycle_list[cl == 2L]
  C3 <- cycle_list[cl == 3L]

  N2 <- new.env(parent = emptyenv())
  if (length(C2)) {
    for (e in C2) {
      a <- e[1]; b <- e[2]
      ka <- as.character(a); kb <- as.character(b)
      if (!exists(ka, envir = N2, inherits = FALSE)) assign(ka, integer(0), envir = N2)
      if (!exists(kb, envir = N2, inherits = FALSE)) assign(kb, integer(0), envir = N2)
      assign(ka, unique(c(get(ka, envir = N2), b)), envir = N2)
      assign(kb, unique(c(get(kb, envir = N2), a)), envir = N2)
    }
  }

  Omega2_keys <- new.env(parent = emptyenv())
  if (length(C2)) {
    for (kv in ls(N2, all.names = TRUE)) {
      v <- as.integer(kv)
      neigh <- get(kv, envir = N2); ln <- length(neigh)
      if (ln >= 2L) {
        for (ii in 1L:(ln - 1L)) for (jj in (ii + 1L):ln)
          assign(.key3(v, neigh[ii], neigh[jj]), TRUE, envir = Omega2_keys)
      }
    }
  }
  if (length(C3)) for (tri in C3) assign(.key3(tri[1], tri[2], tri[3]), TRUE, envir = Omega2_keys)

  pair_to_third <- new.env(parent = emptyenv())
  if (length(C3)) {
    for (tri in C3) {
      a <- tri[1]; b <- tri[2]; c <- tri[3]
      for (p in list(c(a, b), c(a, c), c(b, c))) {
        k <- .key2(p[1], p[2])
        if (!exists(k, envir = pair_to_third, inherits = FALSE)) assign(k, as.integer(c()), envir = pair_to_third)
        assign(k, unique(c(get(k, envir = pair_to_third), setdiff(tri, p))), envir = pair_to_third)
      }
    }
  }
  Omega1_keys <- new.env(parent = emptyenv())
  for (kp in ls(pair_to_third, all.names = TRUE)) {
    thirds <- get(kp, envir = pair_to_third); lt <- length(thirds)
    if (lt >= 2L) {
      base <- as.integer(strsplit(kp, ",", fixed = TRUE)[[1L]])
      for (ii in 1L:(lt - 1L)) for (jj in (ii + 1L):lt)
        assign(.key4(base[1], base[2], thirds[ii], thirds[jj]), TRUE, envir = Omega1_keys)
    }
  }

  Omega3_keys <- new.env(parent = emptyenv())
  if (length(ls(Omega2_keys, all.names = TRUE)) && length(C2)) {
    for (kT in ls(Omega2_keys, all.names = TRUE)) {
      Tset <- .parse_vec(kT)
      for (t in Tset) {
        kt <- as.character(t)
        if (exists(kt, envir = N2, inherits = FALSE)) {
          neigh <- get(kt, envir = N2)
          for (u in neigh) if (!(u %in% Tset)) assign(.key4(Tset[1], Tset[2], Tset[3], u), TRUE, envir = Omega3_keys)
        }
      }
    }
  }
  for (k in ls(Omega1_keys, all.names = TRUE)) assign(k, TRUE, envir = Omega3_keys)

  l1 <- if (length(C2)) lapply(C2, identity) else list()
  Omega2 <- lapply(ls(Omega2_keys, all.names = TRUE), function(k) .parse_vec(k))
  Omega3 <- lapply(ls(Omega3_keys, all.names = TRUE), function(k) .parse_vec(k))

  if (length(Omega2)) {
    o2 <- order(vapply(Omega2, function(v) paste(sprintf("%08d", v), collapse = "-"), ""))
    Omega2 <- Omega2[o2]
  }
  if (length(Omega3)) {
    o3 <- order(vapply(Omega3, function(v) paste(sprintf("%08d", v), collapse = "-"), ""))
    Omega3 <- Omega3[o3]
  }
  c(l1, Omega2, Omega3)
}
