## ---------------------------------------------
## Fast cycle enumeration (K <= 3)
## ---------------------------------------------

get_cycles <- function(nodes, edgelist, max_cycle_size = 3) {
  # expects edgelist columns X1, X2 (1-based ints), no self-loops
  stopifnot(all(c("X1", "X2") %in% names(edgelist)))
  i <- as.integer(edgelist$X1)
  j <- as.integer(edgelist$X2)
  keep <- i != j
  i <- i[keep]; j <- j[keep]

  # Sparse boolean adjacency for O(1) membership tests
  A <- Matrix::sparseMatrix(i = i, j = j, dims = c(nodes, nodes), x = TRUE)

  # Out-neighbor list; ensure every node has a vector (possibly length 0)
  out <- split(j, factor(i, levels = seq_len(nodes)))
  out <- lapply(out, function(v) if (length(v)) as.integer(v) else integer(0L))

  cycles <- vector("list", 64L)
  cidx <- 0L

  # Enumerate with node i as the minimum label in the cycle to avoid duplicates
  for (i_node in seq_len(nodes)) {

    # ----- 2-cycles (i < j) -----
    if (max_cycle_size >= 2L) {
      nbrs <- out[[i_node]]
      if (length(nbrs)) {
        cand <- nbrs[nbrs > i_node]
        if (length(cand)) {
          recip <- cand[ A[cand, i_node] ]
          nrec <- length(recip)
          if (nrec) {
            if (cidx + nrec > length(cycles)) length(cycles) <- max(2L * length(cycles), cidx + nrec)
            for (jj in recip) { cidx <- cidx + 1L; cycles[[cidx]] <- c(i_node, jj) }
          }
        }
      }
    }

    # ----- 3-cycles (i < k to keep i minimal in {i,j,k}) -----
    if (max_cycle_size >= 3L) {
      nbrs1 <- out[[i_node]]
      if (length(nbrs1)) {
        for (j_node in nbrs1) {
          nbrs2 <- out[[j_node]]
          if (!length(nbrs2)) next

          # enforce i_node as smallest label in the 3-cycle
          ks <- nbrs2[nbrs2 > i_node & nbrs2 != j_node]
          if (!length(ks)) next

          # close the cycle with k -> i
          closek <- ks[ A[ks, i_node] ]
          nclose <- length(closek)
          if (nclose) {
            if (cidx + nclose > length(cycles)) length(cycles) <- max(2L * length(cycles), cidx + nclose)
            for (k_node in closek) {
              cidx <- cidx + 1L
              cycles[[cidx]] <- c(i_node, j_node, k_node)
            }
          }
        }
      }
    }
  }

  if (cidx == 0L) return(list())
  cycles[seq_len(cidx)]
}
