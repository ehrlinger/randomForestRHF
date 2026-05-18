########################################################################
## cache builder
########################################################################
varpro.cache.rhf <- function(o,
                             max.rules.tree = 150L,
                             max.tree = 150L,
                             y.external = NULL,
                             eps = 1e-6,
                             verbose = FALSE) {
  if (!inherits(o, "rhf")) {
    stop("This function only works for objects inheriting from class 'rhf'.")
  }
  t.start <- proc.time()[3L]
  if (isTRUE(verbose)) {
    message("[varpro.cache.rhf] building rule-membership cache")
  }
  varpro.strength.fun <- varpro.strength
  get.strengthArray.fun <- get.varpro.strengthArray
  oo <- varpro.strength.fun(object = o,
                            max.rules.tree = max.rules.tree,
                            max.tree = max.tree,
                            membership = TRUE)
  if (is.null(oo$strengthArray)) {
    stop("varpro.strength() did not return a valid 'strengthArray'.")
  }
  if (is.null(oo$oobMembership) || is.null(oo$compMembership)) {
    stop("varpro.strength() did not return membership lists.")
  }
  y.info <- .rhf_get_working_response(o,
                                      y.external = y.external,
                                      eps = eps)
  y <- y.info$y
  y.source <- y.info$y.source
  ss <- .rhf_get_start_stop(o)
  grid <- .rhf_get_grid(o)
  n.pseudo <- length(ss$start)
  if (length(y) != n.pseudo) {
    stop("Length of working response does not match the number of pseudo-individuals.")
  }
  attr(y, "family") <- "regr"
  attr(y, "y.org") <- if (!is.null(y.external)) y.external else if (!is.null(o$y.org)) o$y.org else y
  ## Mirror the RHF branch in importance.varpro(): add an importance slot and
  ## build the regression-style strength array used downstream.
  oo$strengthArray$importance <- NA_real_
  results.template <- get.strengthArray.fun(oo$strengthArray, "regr", y)
  imp.col <- colnames(results.template) == "imp"
  if (sum(imp.col) != 1L) {
    imp.col <- grepl("^imp($|[.])", colnames(results.template))
  }
  if (sum(imp.col) != 1L) {
    stop("RHF time-localized importance expects a single regression-style 'imp' column.")
  }
  if (!("n.oob" %in% colnames(results.template))) {
    stop("Expected column 'n.oob' in the regression-style strength array.")
  }
  if (!all(c("tree", "variable") %in% colnames(results.template))) {
    stop("Expected columns 'tree' and 'variable' in the regression-style strength array.")
  }
  base.rules <- which(oo$strengthArray$oobCT > 0 & oo$strengthArray$compCT > 0)
  results.base <- results.template[base.rules, , drop = FALSE]
  results.base[, imp.col] <- NA_real_
  plan <- .rhf_build_window_plan(start = ss$start,
                                 stopv = ss$stop,
                                 grid = grid)
  precomp <- .rhf_precompute_rule_windows(
    strength = oo$strengthArray,
    base.rules = base.rules,
    oobMembership = oo$oobMembership,
    compMembership = oo$compMembership,
    y = y,
    left.idx = plan$left.idx,
    right.idx = plan$right.idx,
    K = plan$K,
    verbose = verbose
  )
  out <- list(
    call = match.call(),
    results.base = results.base,
    imp.col = imp.col,
    local.imp.window = precomp$local.imp,
    keep.window = precomp$keep,
    n.oob.window = precomp$n.oob,
    n.rules.window = precomp$n.rules,
    xvar.names = o$xvar.names,
    y = y,
    y.source = y.source,
    start = ss$start,
    stop = ss$stop,
    grid = grid,
    K = plan$K,
    window.left = plan$window.left,
    window.right = plan$window.right,
    window.mid = plan$window.mid,
    window.label = plan$window.label,
    n.risk.window = plan$n.risk,
    max.rules.tree = max.rules.tree,
    max.tree = max.tree,
    n.pseudo = n.pseudo
  )
  class(out) <- "varpro.cache.rhf"
  if (isTRUE(verbose)) {
    message("[varpro.cache.rhf] done: ",
            nrow(results.base), " usable rules, ",
            n.pseudo, " pseudo-individuals",
            if (!is.null(y.source) && nzchar(y.source)) paste0(", y.source=", y.source) else "",
            ", elapsed ",
            .rhf_format_elapsed(proc.time()[3L] - t.start))
  }
  out
}
varpro.cache <- varpro.cache.rhf
########################################################################
## localized rule calculations
########################################################################
.rhf_local_results_from_cache <- function(cache,
                                          window.index,
                                          trim,
                                          sort,
                                          ...) {
  workhorse.fun <- function(...) {
    args <- list(...)
    ## hard-coded for compatibility
    args$cutoff <- 0.76
    args$plot.it <- FALSE
    args$conf <- FALSE
    args$ylab <- "Importance"
    do.call(importance.varpro.workhorse, args)
  }
  take <- which(cache$keep.window[, window.index])
  if (!length(take)) {
    return(list(table = data.frame(), n.rules = 0L))
  }
  results <- cache$results.base[take, , drop = FALSE]
  results[, cache$imp.col] <- cache$local.imp.window[take, window.index]
  results$n.oob <- cache$n.oob.window[take, window.index]
  o.tmp <- list(
    results = results,
    xvar.names = cache$xvar.names,
    family = "regr"
  )
  tab <- workhorse.fun(o = o.tmp,
                       trim = trim,
                       sort = sort,
                       local.std = TRUE,
                       ...)
  list(table = tab, n.rules = length(take))
}
.rhf_importance_long <- function(importance.matrix, window.info) {
  if (!is.matrix(importance.matrix) || !length(importance.matrix)) {
    return(data.frame())
  }
  if (ncol(importance.matrix) != nrow(window.info)) {
    stop("Internal error: window metadata do not align with the importance matrix.")
  }
  imp.vec <- as.vector(importance.matrix)
  out <- data.frame(
    variable = rep(rownames(importance.matrix), times = ncol(importance.matrix)),
    time = rep(window.info$time, each = nrow(importance.matrix)),
    time.index = rep(window.info$index, each = nrow(importance.matrix)),
    window = rep(window.info$label, each = nrow(importance.matrix)),
    start = rep(window.info$start, each = nrow(importance.matrix)),
    stop = rep(window.info$stop, each = nrow(importance.matrix)),
    midpoint = rep(window.info$midpoint, each = nrow(importance.matrix)),
    n.risk = rep(window.info$n.risk, each = nrow(importance.matrix)),
    n.rules = rep(window.info$n.rules, each = nrow(importance.matrix)),
    importance = imp.vec,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  ord.imp <- out$importance
  ord.imp[is.na(ord.imp)] <- -Inf
  ord <- order(out$time.index, -ord.imp, out$variable)
  out[ord, , drop = FALSE]
}
########################################################################
## main user routine
########################################################################
importance.rhf <- function(o,
                           cache = NULL,
                           time.index = NULL,
                           trim = 0.1,
                           sort = TRUE,
                           max.rules.tree,
                           max.tree,
                           eps = 1e-6,
                           y.external = NULL,
                           verbose = FALSE,
                           ...) {
  if (!inherits(o, "rhf")) {
    stop("This function only works for objects inheriting from class 'rhf'.")
  }
  if (is.null(cache)) {
    cache.args <- list(o = o,
                       eps = eps,
                       y.external = y.external,
                       verbose = verbose)
    if (!missing(max.rules.tree)) {
      cache.args$max.rules.tree <- max.rules.tree
    }
    if (!missing(max.tree)) {
      cache.args$max.tree <- max.tree
    }
    cache <- do.call(varpro.cache.rhf, cache.args)
  }
  else {
    .rhf_validate_cache(o, cache)
    if (isTRUE(verbose)) {
      message("[importance.rhf] using supplied cache",
              if (!is.null(cache$y.source) && nzchar(cache$y.source)) {
                paste0(" (y.source=", cache$y.source, ")")
              } else "")
    }
  }
  if (is.null(time.index)) {
    time.index <- seq_len(cache$K)
  }
  if (is.logical(time.index)) {
    if (length(time.index) != cache$K) {
      stop("Logical 'time.index' must have length equal to length(o$time.interest).")
    }
    time.index <- which(time.index)
  }
  time.index <- sort(unique(as.integer(time.index)))
  if (!length(time.index)) {
    stop("No valid time indices supplied.")
  }
  if (any(!is.finite(time.index)) || any(time.index < 1L) || any(time.index > cache$K)) {
    stop("Argument 'time.index' contains invalid window indices.")
  }
  p <- length(cache$xvar.names)
  m <- length(time.index)
  importance.matrix <- matrix(
    NA_real_,
    nrow = p,
    ncol = m,
    dimnames = list(
      cache$xvar.names,
      .rhf_format_time_names(cache$window.right[time.index])
    )
  )
  n.risk <- as.integer(cache$n.risk.window[time.index])
  n.rules <- as.integer(cache$n.rules.window[time.index])
  t.start <- proc.time()[3L]
  for (jj in seq_len(m)) {
    target.window <- time.index[jj]
    local.out <- .rhf_local_results_from_cache(
      cache = cache,
      window.index = target.window,
      trim = trim,
      sort = sort,
      ...
    )
    if (is.data.frame(local.out$table) && nrow(local.out$table)) {
      importance.matrix[rownames(local.out$table), jj] <- local.out$table$z
    }
    if (isTRUE(verbose)) {
      elapsed <- proc.time()[3L] - t.start
      eta <- if (jj < m) elapsed / jj * (m - jj) else 0
      message(
        "[importance.rhf] window ", jj, "/", m,
        " (grid index ", target.window,
        ", t=", .rhf_format_time_names(cache$window.right[target.window]), ")",
        " | remaining ", m - jj,
        " | n.risk ", n.risk[jj],
        " | n.rules ", n.rules[jj],
        " | elapsed ", .rhf_format_elapsed(elapsed),
        " | ETA ", .rhf_format_elapsed(eta)
      )
    }
  }
  window.info <- data.frame(
    index = time.index,
    time = cache$window.right[time.index],
    start = cache$window.left[time.index],
    stop = cache$window.right[time.index],
    midpoint = cache$window.mid[time.index],
    n.risk = n.risk,
    n.rules = n.rules,
    label = cache$window.label[time.index],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out <- list(
    call = match.call(),
    xvar.names = cache$xvar.names,
    importance.matrix = importance.matrix,
    importance.long = .rhf_importance_long(
      importance.matrix = importance.matrix,
      window.info = window.info
    ),
    window.info = window.info,
    y.source = cache$y.source,
    trim = trim
  )
  class(out) <- "importance.rhf"
  out
}
