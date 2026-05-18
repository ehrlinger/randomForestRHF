########################################################################
## window engine helpers
########################################################################
.rhf_build_window_plan <- function(start, stopv, grid) {
  K <- length(grid)
  scale.time <- max(1, abs(c(start, stopv, grid)), na.rm = TRUE)
  eps.time <- sqrt(.Machine$double.eps) * scale.time
  ## Window k is (left[k], right[k]] with:
  ##   left  = c(0, grid[-K])
  ##   right = grid
  left.idx <- findInterval(start, grid) + 1L
  left.idx <- pmin(left.idx, K + 1L)
  stop.adj <- pmax(stopv - eps.time, 0)
  right.idx <- findInterval(stop.adj, grid) + 1L
  right.idx <- pmax(0L, pmin(right.idx, K))
  valid <- left.idx <= right.idx
  diff.count <- numeric(K + 1L)
  if (any(valid)) {
    diff.count <- tabulate(left.idx[valid], nbins = K + 1L) -
      tabulate(right.idx[valid] + 1L, nbins = K + 1L)
  }
  n.risk <- as.integer(cumsum(diff.count)[seq_len(K)])
  window.left <- c(0, grid[-K])
  window.right <- grid
  window.mid <- (window.left + window.right) / 2
  window.label <- paste0("(",
                         .rhf_format_time_names(window.left),
                         ", ",
                         .rhf_format_time_names(window.right),
                         "]")
  list(
    K = K,
    left.idx = left.idx,
    right.idx = right.idx,
    n.risk = n.risk,
    window.left = window.left,
    window.right = window.right,
    window.mid = window.mid,
    window.label = window.label
  )
}
.rhf_empty_membership_series <- function(K) {
  list(
    count_all = integer(K),
    count_valid = integer(K),
    sum = numeric(K),
    sumsq = numeric(K)
  )
}
.rhf_group_sums2 <- function(group, value1, value2, nbins) {
  out1 <- numeric(nbins)
  out2 <- numeric(nbins)
  if (!length(group)) {
    return(list(sum = out1, sumsq = out2))
  }
  if (length(group) == 1L) {
    gg <- as.integer(group)[1L]
    out1[gg] <- as.numeric(value1)[1L]
    out2[gg] <- as.numeric(value2)[1L]
    return(list(sum = out1, sumsq = out2))
  }
  rs <- rowsum(cbind(value1, value2), group = group, reorder = FALSE)
  idx <- as.integer(rownames(rs))
  out1[idx] <- rs[, 1L]
  out2[idx] <- rs[, 2L]
  list(sum = out1, sumsq = out2)
}
.rhf_membership_window_series <- function(idx,
                                          y,
                                          left.idx,
                                          right.idx,
                                          K,
                                          y.has.na = FALSE) {
  if (!length(idx)) {
    return(.rhf_empty_membership_series(K))
  }
  idx <- as.integer(idx)
  idx <- idx[idx >= 1L & idx <= length(left.idx)]
  if (!length(idx)) {
    return(.rhf_empty_membership_series(K))
  }
  li <- left.idx[idx]
  ri <- right.idx[idx]
  active <- li <= ri
  if (!any(active)) {
    return(.rhf_empty_membership_series(K))
  }
  idx <- idx[active]
  li <- as.integer(li[active])
  ri1 <- as.integer(ri[active] + 1L)
  K1 <- K + 1L
  diff.count.all <- tabulate(li, nbins = K1) - tabulate(ri1, nbins = K1)
  count.all <- as.integer(cumsum(diff.count.all)[seq_len(K)])
  yv <- y[idx]
  if (isTRUE(y.has.na)) {
    ok <- !is.na(yv)
    if (!any(ok)) {
      return(list(
        count_all = count.all,
        count_valid = integer(K),
        sum = numeric(K),
        sumsq = numeric(K)
      ))
    }
    li.valid <- li[ok]
    ri1.valid <- ri1[ok]
    yv <- yv[ok]
    diff.count.valid <- tabulate(li.valid, nbins = K1) - tabulate(ri1.valid, nbins = K1)
    count.valid <- as.integer(cumsum(diff.count.valid)[seq_len(K)])
  }
  else {
    li.valid <- li
    ri1.valid <- ri1
    count.valid <- count.all
  }
  add <- .rhf_group_sums2(li.valid,
                          value1 = yv,
                          value2 = yv * yv,
                          nbins = K1)
  drop <- .rhf_group_sums2(ri1.valid,
                           value1 = yv,
                           value2 = yv * yv,
                           nbins = K1)
  sum.series <- cumsum(add$sum - drop$sum)[seq_len(K)]
  sumsq.series <- cumsum(add$sumsq - drop$sumsq)[seq_len(K)]
  list(
    count_all = count.all,
    count_valid = count.valid,
    sum = sum.series,
    sumsq = sumsq.series
  )
}
.rhf_local_welch_from_stats <- function(n1,
                                        sum1,
                                        sumsq1,
                                        n2,
                                        sum2,
                                        sumsq2) {
  K <- length(n1)
  out <- rep(NA_real_, K)
  ok <- (n1 >= 2L) & (n2 >= 2L)
  if (!any(ok)) {
    return(out)
  }
  n1.ok <- as.numeric(n1[ok])
  n2.ok <- as.numeric(n2[ok])
  sum1.ok <- as.numeric(sum1[ok])
  sum2.ok <- as.numeric(sum2[ok])
  sumsq1.ok <- as.numeric(sumsq1[ok])
  sumsq2.ok <- as.numeric(sumsq2[ok])
  m1 <- sum1.ok / n1.ok
  m2 <- sum2.ok / n2.ok
  v1 <- (sumsq1.ok - (sum1.ok * sum1.ok) / n1.ok) / pmax(1, n1.ok - 1)
  v2 <- (sumsq2.ok - (sum2.ok * sum2.ok) / n2.ok) / pmax(1, n2.ok - 1)
  tiny <- sqrt(.Machine$double.eps)
  v1[is.finite(v1) & v1 < 0 & abs(v1) < tiny] <- 0
  v2[is.finite(v2) & v2 < 0 & abs(v2) < tiny] <- 0
  se <- sqrt(v1 / n1.ok + v2 / n2.ok)
  out.ok <- rep(NA_real_, length(n1.ok))
  good <- is.finite(se) & (se > 0)
  out.ok[good] <- abs((m1[good] - m2[good]) / se[good])
  out[ok] <- out.ok
  out
}
.rhf_base_branch_id <- function(strength,
                                base.rules) {
  if (!length(base.rules)) {
    return(integer(0))
  }
  tree <- strength$treeID[base.rules]
  node <- strength$nodeID[base.rules]
  as.integer(cumsum(c(TRUE,
                      (tree[-1L] != tree[-length(tree)]) |
                      (node[-1L] != node[-length(node)]))))
}
.rhf_precompute_rule_windows <- function(strength,
                                         base.rules,
                                         oobMembership,
                                         compMembership,
                                         y,
                                         left.idx,
                                         right.idx,
                                         K,
                                         verbose = FALSE) {
  n.base <- length(base.rules)
  if (!n.base) {
    return(list(
      keep = matrix(FALSE, nrow = 0L, ncol = K),
      n.oob = matrix(integer(0), nrow = 0L, ncol = K),
      local.imp = matrix(numeric(0), nrow = 0L, ncol = K),
      n.rules = integer(K)
    ))
  }
  t.start <- proc.time()[3L]
  if (isTRUE(verbose)) {
    message("[varpro.cache.rhf] precomputing window-local rule statistics")
  }
  y.has.na <- anyNA(y)
  branch.id <- .rhf_base_branch_id(strength = strength,
                                   base.rules = base.rules)
  n.branch <- max(branch.id)
  branch.first <- match(seq_len(n.branch), branch.id)
  oob.count.all <- matrix(0L, nrow = n.branch, ncol = K)
  oob.count.valid <- matrix(0L, nrow = n.branch, ncol = K)
  oob.sum <- matrix(0, nrow = n.branch, ncol = K)
  oob.sumsq <- matrix(0, nrow = n.branch, ncol = K)
  for (bb in seq_len(n.branch)) {
    rule.index <- base.rules[branch.first[bb]]
    ser <- .rhf_membership_window_series(
      idx = oobMembership[[rule.index]],
      y = y,
      left.idx = left.idx,
      right.idx = right.idx,
      K = K,
      y.has.na = y.has.na
    )
    oob.count.all[bb, ] <- ser$count_all
    oob.count.valid[bb, ] <- ser$count_valid
    oob.sum[bb, ] <- ser$sum
    oob.sumsq[bb, ] <- ser$sumsq
  }
  keep <- matrix(FALSE, nrow = n.base, ncol = K)
  n.oob <- matrix(0L, nrow = n.base, ncol = K)
  local.imp <- matrix(NA_real_, nrow = n.base, ncol = K)
  for (ii in seq_len(n.base)) {
    bb <- branch.id[ii]
    rule.index <- base.rules[ii]
    ser.comp <- .rhf_membership_window_series(
      idx = compMembership[[rule.index]],
      y = y,
      left.idx = left.idx,
      right.idx = right.idx,
      K = K,
      y.has.na = y.has.na
    )
    n.oob[ii, ] <- oob.count.all[bb, ]
    keep[ii, ] <- (oob.count.all[bb, ] > 0L) & (ser.comp$count_all > 0L)
    local.imp[ii, ] <- .rhf_local_welch_from_stats(
      n1 = oob.count.valid[bb, ],
      sum1 = oob.sum[bb, ],
      sumsq1 = oob.sumsq[bb, ],
      n2 = ser.comp$count_valid,
      sum2 = ser.comp$sum,
      sumsq2 = ser.comp$sumsq
    )
  }
  if (isTRUE(verbose)) {
    message("[varpro.cache.rhf] rule-window statistics ready: ",
            n.base, " rules x ", K, " windows",
            ", elapsed ",
            .rhf_format_elapsed(proc.time()[3L] - t.start))
  }
  list(
    keep = keep,
    n.oob = n.oob,
    local.imp = local.imp,
    n.rules = as.integer(colSums(keep))
  )
}
