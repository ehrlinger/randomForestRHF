plot.rhf <- function(x, idx = NULL, scale.hazard = FALSE, 
                     ngrid = 30, bass = 0, q = 0.99, grid = FALSE, 
                     col = NULL, lty = NULL, legend.loc = "topright", 
                     jitter.factor = 1, lwd = 4, 
                     hazard.only = TRUE, legend.show = TRUE, ...) {
  ## Preliminary
  time.interest <- x$time.interest
  if (is.null(idx)) idx <- x$ensemble.id[1]
  if (sum(idx %in% x$ensemble.id) == 0L) stop("idx does not match id values in x$id")
  idx <- match(idx, x$ensemble.id)
  n.idx <- length(idx)
  dots <- list(...)
  if (is.null(col)) col <- seq_len(n.idx)
  if (is.null(lty)) lty <- rep(1, n.idx)
  ## pull the ensemble estimators
  get_mat <- function(name_oob, name_test) {
      if (!is.null(x[[name_oob]])) return(x[[name_oob]])
      if (!is.null(x[[name_test]])) return(x[[name_test]])
      NULL
  }
  chf <- get_mat("chf.oob", "chf.test")
  haz <- get_mat("hazard.oob", "hazard.test")
  ## missing values are set to zero
  chf[is.na(chf)] <- 0
  haz[is.na(haz)] <- 0
  if (is.logical(scale.hazard)) {
    if (scale.hazard) {
      haz <- diff(c(0, time.interest)) * haz
    }
  } else if (is.numeric(scale.hazard) && length(scale.hazard) == 1 && is.finite(scale.hazard)) {
    haz <- scale.hazard * haz
  } else {
    stop("scale.hazard must be either logical or a finite numeric scalar.")
  }
  ## Compute plot ranges
  ranges <- lapply(idx, function(i) {
    h <- haz[i, ]
    pt <- h <= quantile(h, q, na.rm = TRUE) 
    h.smooth <- supsmu(time.interest[pt], h[pt], bass = bass)$y
    ## auxiliary equally spaced grid (used for overlays)
    time.grid <- unique(c(0, seq(0, max(time.interest), length = ngrid)))
    if (!hazard.only) {
      H <- chf[i, ]
      H.grid <- c(0, H)[1 + sIndex(time.interest, time.grid)]
      H.grid.smooth <- supsmu(time.grid, H.grid)$y
      delta <- max(diff(time.grid))
      h.grid.smooth <- supsmu(time.grid, c(0, diff(H.grid.smooth) / delta), bass = bass)$y
    } else {
      H <- H.grid.smooth <- NULL
      ## NEW: make ngrid meaningful in hazard-only mode - smooth hazard on a uniform grid
      h.on.grid <- stats::approx(x = time.interest[pt], y = h[pt],
                          xout = time.grid, method = "linear", rule = 2, ties = mean)$y
      h.grid.smooth <- supsmu(time.grid, h.on.grid, bass = bass)$y
    }
    list(H = H,
         h.smooth = h.smooth,
         h.grid.smooth = h.grid.smooth,
         H.grid.smooth = H.grid.smooth,
         time.grid = time.grid)  ## NEW: carry the grid back for plotting
  })
  ## Extract or compute xlim/ylim
  xlim <- if (!is.null(dots$xlim)) dots$xlim else range(time.interest)
  ylim <- if (!is.null(dots$ylim)) dots$ylim else {
    range(unlist(
      lapply(ranges, function(xi) {
        if (hazard.only) {
          c(xi$h.smooth, xi$h.grid.smooth)     ## includes grid smoother in hazard-only
        } else {
          c(xi$H, xi$H.grid.smooth, xi$h.smooth, xi$h.grid.smooth)
        }
      })
    ), na.rm = TRUE)
  }
  dots$xlim <- NULL
  dots$ylim <- NULL
  ## Main plot
  ylab.default <- if (hazard.only) "Hazard" else "CHF + hazard"
  do.call("plot", c(list(x = NA, y = NA,
                         xlim = xlim, ylim = c(max(0, ylim[1]), ylim[2]),
                         xlab = "time", ylab = ylab.default), dots))
  ## Loop through cases
  invisible(lapply(seq_along(idx), function(k) {
    i <- idx[k]
    h <- haz[i, ]
    pt <- h <= quantile(h, q, na.rm = TRUE)
    h.smooth <- supsmu(time.interest[pt], h[pt], bass = bass)$y
    ## plot hazard
    t.jittered <- jitter(time.interest[pt], factor = jitter.factor)
    lines(t.jittered, h[pt], type = "h", col = adjustcolor(col[k], alpha.f = 0.5), lwd = lwd)
    lines(time.interest[pt], h.smooth, type = "s", col = col[k], lty = if (hazard.only) 1 else 3)
    if (!hazard.only) {
      H <- chf[i, ]
      rr <- ranges[[k]]
      lines(time.interest, H, type = "s", col = col[k])
      if (grid) {
        lines(rr$time.grid, rr$H.grid.smooth, type = "s", col = col[k], lty = 3)
        lines(rr$time.grid, rr$h.grid.smooth, type = "s", col = col[k], lty = 2)
      }
    } else {
      ## NEW: allow a dashed grid-based hazard overlay when hazard-only = TRUE
      if (grid) {
        rr <- ranges[[k]]
        lines(rr$time.grid, rr$h.grid.smooth, type = "s", col = col[k], lty = 2)
      }
    }
  }))
  my_rug(x)
  ## Optional legend
  if (legend.show && n.idx > 1) {
    labels <- if (!is.null(names(idx))) names(idx) else paste("Case", idx)
    legend(legend.loc, legend = labels, col = col, lty = lty, bty = "n")
  }
}
####################################################################
##
## TDC Helper Graphical Functions
##
####################################################################
## Time alignment helper
sIndex <- function(x, y) {
  sapply(seq_along(y), function(j) sum(x <= y[j]))
}
## Rug function with clipping protection
my_rug <- function(x, ticksize = 0.02, nmax = 350) {
  rug.time <- unique(x$yvar$stop[x$yvar$event == 1])
  rug.time <- sample(rug.time, size = min(length(rug.time), nmax), replace = FALSE)
  xlims <- par("usr")[1:2]
  rug.time <- rug.time[rug.time >= xlims[1] & rug.time <= xlims[2]]
  suppressWarnings(rug(rug.time, ticksize = ticksize))
}
