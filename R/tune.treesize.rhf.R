## tune.treesize: choose treesize to optimize either OOB risk or OOB iAUC.uno
tune.treesize.rhf <- function(formula, data,
                              ntree = 500,
                              nsplit = 10,
                              nodesize = NULL,
                              ## performance measure
                              perf = c("risk", "iAUC"),
                              ## extra arguments passed to auct.rhf when perf = "iAUC"
                              auct.args = NULL,
                              ## search bounds
                              lower = 2L,
                              upper = NULL,
                              C = 3,
                              ## search control
                              method = c("golden", "bisect"),
                              max.evals = 20L,
                              bracket.tol = 2L,   ## stop when width <= bracket.tol
                              ## reproducibility and output
                              seed = NULL,
                              verbose = TRUE,
                              forest = TRUE,
                              ...) {
  method <- match.arg(method)
  perf   <- match.arg(perf)
  ## ---- robust subject count (prefer explicit id, else 'id' column, else rows)
  get_id_from_formula <- function(formula, data) {
    f   <- as.formula(formula)
    lhs <- f[[2]]
    if (is.call(lhs) && identical(as.character(lhs[[1]]), "Surv")) {
      L <- as.list(lhs)[-1]
      if (length(L) == 4L) {                # Surv(id, start, stop, event)
        nm <- paste(deparse(L[[1]]), collapse = "")
        if (nm %in% names(data)) return(nm)
      }
    }
    if ("id" %in% names(data)) return("id")
    NULL
  }
  id.var <- get_id_from_formula(formula, data)
  n.subj <- if (!is.null(id.var)) length(unique(data[[id.var]])) else nrow(data)
  ## ---- bounds
  if (is.null(upper)) upper <- ceiling(C * min(30, n.subj / 5))
  lower <- as.integer(max(2L, lower))
  upper <- as.integer(max(lower + 1L, upper))
  if (!is.null(seed)) set.seed(seed)
  ## ---- caches (keyed by treesize)
  risk.cache <- new.env(parent = emptyenv())
  obj.cache  <- if (isTRUE(forest)) new.env(parent = emptyenv()) else NULL
  ## Only used when perf = "iAUC"
  iauc.cache    <- if (perf == "iAUC") new.env(parent = emptyenv()) else NULL
  iauc.se.cache <- if (perf == "iAUC") new.env(parent = emptyenv()) else NULL
  ## ---- helper to evaluate a single treesize
  eval.one <- function(ts) {
    ts  <- as.integer(ts)
    key <- as.character(ts)
    ## cached result?
    if (exists(key, envir = risk.cache, inherits = FALSE)) {
      r <- get(key, envir = risk.cache, inherits = FALSE)
      if (isTRUE(verbose)) {
        if (perf == "risk") {
          message(sprintf("treesize = %d   OOB risk (criterion) = %.6f (cached)", ts, r))
        } else {
          message(sprintf("treesize = %d   1 - iAUC.uno (criterion) = %.6f (cached)", ts, r))
        }
      }
      return(r)
    }
    ## refit RHF at this treesize
    if (!is.null(seed)) set.seed(seed)  ## make cross-size comparisons comparable
    fit <- rhf(formula = formula,
               data    = data,
               ntree   = ntree,
               nsplit  = nsplit,
               treesize = ts,
               nodesize = nodesize,
               ...)
    ## ---- performance measure
    if (perf == "risk") {
      rsk <- fit$risk.oob
      if (is.null(rsk))
        stop("OOB risk values (fit$risk.oob) are missing; cannot tune by 'risk'.")
      r <- mean(rsk[is.finite(rsk)], na.rm = TRUE)
      if (!is.finite(r)) r <- Inf
      if (isTRUE(verbose))
        message(sprintf("treesize = %d   OOB risk (criterion) = %.6f", ts, r))
    } else { ## perf == "iAUC"
      args.auct <- auct.args
      if (is.null(args.auct)) args.auct <- list()
      args.auct$object <- fit  ## enforce
      auct.obj <- try(do.call(auct.rhf, args.auct), silent = TRUE)
      if (inherits(auct.obj, "try-error")) {
        if (isTRUE(verbose)) {
          message(sprintf("treesize = %d   auct.rhf() failed; treating criterion as +Inf", ts))
        }
        r <- Inf
        if (!is.null(iauc.cache))    assign(key, NA_real_, envir = iauc.cache)
        if (!is.null(iauc.se.cache)) assign(key, NA_real_, envir = iauc.se.cache)
      } else {
        iauc <- auct.obj$iAUC.uno
        if (!is.finite(iauc)) {
          if (isTRUE(verbose)) {
            message(sprintf("treesize = %d   iAUC.uno is non-finite; treating criterion as +Inf", ts))
          }
          r <- Inf
          se <- NA_real_
        } else {
          r <- 1 - iauc
          if (isTRUE(verbose)) {
            message(sprintf("treesize = %d   iAUC.uno = %.6f   criterion (1 - iAUC.uno) = %.6f",
                            ts, iauc, r))
          }
          ## try to grab bootstrap SE if available
          se <- NA_real_
          if (!is.null(auct.obj$boot) &&
              !is.null(auct.obj$boot$iAUC.uno.se) &&
              is.finite(auct.obj$boot$iAUC.uno.se)) {
            se <- auct.obj$boot$iAUC.uno.se
          }
        }
        if (!is.null(iauc.cache))    assign(key, iauc, envir = iauc.cache)
        if (!is.null(iauc.se.cache)) assign(key, se,   envir = iauc.se.cache)
      }
    }
    ## cache results
    assign(key, r, envir = risk.cache)
    if (isTRUE(forest)) assign(key, fit, envir = obj.cache)
    r
  }
  ## collect the path
  collect.path <- function() {
    sizes <- as.integer(sort(as.integer(ls(risk.cache))))
    crit  <- vapply(
      sizes,
      function(k) get(as.character(k), envir = risk.cache, inherits = FALSE),
      numeric(1)
    )
    path <- data.frame(treesize = sizes, risk = crit)
    if (perf == "iAUC" && !is.null(iauc.cache)) {
      iauc <- vapply(
        sizes,
        function(k) {
          key <- as.character(k)
          if (exists(key, envir = iauc.cache, inherits = FALSE)) {
            get(key, envir = iauc.cache, inherits = FALSE)
          } else {
            NA_real_
          }
        },
        numeric(1)
      )
      se <- vapply(
        sizes,
        function(k) {
          key <- as.character(k)
          if (!is.null(iauc.se.cache) &&
              exists(key, envir = iauc.se.cache, inherits = FALSE)) {
            get(key, envir = iauc.se.cache, inherits = FALSE)
          } else {
            NA_real_
          }
        },
        numeric(1)
      )
      path$iAUC    <- iauc
      path$iAUC.se <- se
    }
    path
  }
  ## ---- initialize
  a <- lower; b <- upper
  evals <- 0L
  ## Always evaluate the smallest treesize at least once
  eval.one(lower); evals <- evals + 1L
  if (method == "golden") {
    ## classic discrete golden-section on integers
    phi <- (1 + sqrt(5)) / 2
    x1 <- as.integer(round(b - (b - a) / phi))
    x2 <- as.integer(round(a + (b - a) / phi))
    if (x1 == x2) x2 <- min(b, x1 + 1L)
    f1 <- eval.one(x1); evals <- evals + 1L
    f2 <- eval.one(x2); evals <- evals + 1L
    while ((b - a > bracket.tol) && evals < max.evals) {
      if (f1 > f2) {
        a <- x1
        x1 <- x2
        f1 <- f2
        x2 <- as.integer(round(a + (b - a) / phi))
        if (x2 <= x1) x2 <- min(b, x1 + 1L)
        f2 <- eval.one(x2); evals <- evals + 1L
      } else {
        b <- x2
        x2 <- x1
        f2 <- f1
        x1 <- as.integer(round(b - (b - a) / phi))
        if (x1 >= x2) x1 <- max(a, x2 - 1L)
        f1 <- eval.one(x1); evals <- evals + 1L
      }
    }
  } else { ## method == "bisect"
    while ((b - a > bracket.tol) && evals < max.evals) {
      m  <- as.integer(floor((a + b) / 2))
      fm <- eval.one(m);       evals <- evals + 1L
      fl <- eval.one(m - 1L);  evals <- evals + 1L
      fr <- eval.one(m + 1L);  evals <- evals + 1L
      if (fl >= fm && fr >= fm) { a <- max(a, m - 1L); b <- min(b, m + 1L); break }
      if (fl <  fm) { b <- m - 1L } else
      if (fr <  fm) { a <- m + 1L } else {
        a <- max(a, m - 1L); b <- min(b, m + 1L)
      }
    }
  }
  ## Evaluate remaining integers in [a, b], but only those not already cached
  left  <- max(lower, a)
  right <- min(upper, b)
  if (right >= left) {
    cand <- seq.int(left, right)
    not.eval <- cand[!(as.character(cand) %in% ls(risk.cache))]
    if (length(not.eval)) {
      vapply(not.eval, eval.one, numeric(1))
      evals <- evals + length(not.eval)
    }
  }
  ## ---- GLOBAL best over *all* evaluated sizes
  path <- collect.path()
  idx  <- which.min(path$risk)
  best.size <- path$treesize[idx]
  best.err  <- path$risk[idx]
  ## Optionally return the forest at best.size
  best.fit <- NULL
  if (isTRUE(forest)) {
    key <- as.character(best.size)
    if (!exists(key, envir = obj.cache, inherits = FALSE)) {
      if (!is.null(seed)) set.seed(seed)
      best.fit <- rhf(formula = formula, data = data,
                      ntree = ntree, nsplit = nsplit,
                      treesize = best.size, nodesize = nodesize, ...)
    } else {
      best.fit <- get(key, envir = obj.cache, inherits = FALSE)
    }
  }
  out <- list(
    best.size   = best.size,
    best.err    = best.err,
    bounds      = c(lower = lower, upper = upper),
    n.subjects  = n.subj,
    C           = C,
    method      = method,
    perf        = perf,
    path        = path
  )
  if (!is.null(best.fit)) out$forest <- best.fit
  class(out) <- "tune.treesize.rhf"
  out
}
tune.rhf <- tune.treesize.rhf
## Convenience wrapper: tune treesize by iAUC.uno from auct.rhf
tune.iAUC.rhf <- function(formula, data, auct.args = NULL, ...) {
  tune.treesize.rhf(
    formula  = formula,
    data     = data,
    perf     = "iAUC",
    auct.args = auct.args,
    ...
  )
}
tune.iAUC <- tune.iAUC.rhf
## plot results (metric vs treesize, also has bootstrap now)
plot.tune.treesize.rhf <- function(x,
                                   ylab   = NULL,
                                   main   = NULL,
                                   se.band = TRUE,
                                   se.mult = 1,
                                   ylim   = NULL,
                                   ...) {
  stopifnot(inherits(x, "tune.treesize.rhf"))
  path <- x$path
  perf <- if (!is.null(x$perf)) x$perf else "risk"
  if (perf == "iAUC" && "iAUC" %in% names(path)) {
    ## ---------- iAUC tuning plot ----------
    xx <- path$treesize
    yy <- path$iAUC
    if (is.null(ylab)) ylab <- "OOB iAUC.uno"
    if (is.null(main)) main <- "Tuning treesize by OOB iAUC"
    ## compute ylim to include band if needed
    if (is.null(ylim)) {
      y.all <- yy
      if (isTRUE(se.band) && "iAUC.se" %in% names(path)) {
        se   <- path$iAUC.se
        mult <- if (is.finite(se.mult) && se.mult > 0) se.mult else 1
        yl   <- yy - mult * se
        yu   <- yy + mult * se
        y.all <- c(y.all, yl, yu)
      }
      y.all <- y.all[is.finite(y.all)]
      if (length(y.all)) {
        ymin <- min(y.all)
        ymax <- max(y.all)
        pad  <- 0.05 * (ymax - ymin)
        ylim <- c(ymin - pad, ymax + pad)
      }
    }
    plot(xx, yy, type = "p", pch = 16,
         xlab = "treesize", ylab = ylab, main = main,
         ylim = ylim, ...)
    ok <- is.finite(xx) & is.finite(yy)
    if (sum(ok) >= 3L) {
      sm <- stats::lowess(xx[ok], yy[ok])
      lines(sm, lwd = 2)
    }
    ## gray SE ribbon, if available
    if (isTRUE(se.band) &&
        "iAUC.se" %in% names(path) &&
        any(is.finite(path$iAUC.se))) {
      se   <- path$iAUC.se
      mult <- if (is.finite(se.mult) && se.mult > 0) se.mult else 1
      yl   <- yy - mult * se
      yu   <- yy + mult * se
      okb <- is.finite(xx) & is.finite(yl) & is.finite(yu)
      if (sum(okb) >= 2L) {
        ord  <- order(xx[okb])
        xt   <- xx[okb][ord]
        yl.t <- yl[okb][ord]
        yu.t <- yu[okb][ord]
        polygon(c(xt, rev(xt)),
                c(yl.t, rev(yu.t)),
                border = NA,
                col = grDevices::adjustcolor("gray", alpha.f = 0.25))
        ## redraw points on top
        points(xx, yy, pch = 16)
      }
    }
    ## vertical line + annotation at best treesize
    abline(v = x$best.size, lty = 2)
    idx <- which(path$treesize == x$best.size)
    best.iAUC <- if (length(idx) >= 1L && is.finite(path$iAUC[idx[1L]])) {
      path$iAUC[idx[1L]]
    } else {
      1 - x$best.err
    }
    mtext(sprintf("best treesize = %d, iAUC = %.4f",
                  x$best.size, best.iAUC),
          line = 0.5)
  } else {
    ## ---------- OOB risk plot (no band) ----------
    xx <- path$treesize
    yy <- path$risk
    if (is.null(ylab)) ylab <- "OOB empirical risk"
    if (is.null(main)) main <- "Tuning treesize by OOB risk"
    if (is.null(ylim)) {
      y.all <- yy[is.finite(yy)]
      if (length(y.all)) {
        ymin <- min(y.all); ymax <- max(y.all)
        pad  <- 0.05 * (ymax - ymin)
        ylim <- c(ymin - pad, ymax + pad)
      }
    }
    plot(xx, yy, type = "p", pch = 16,
         xlab = "treesize", ylab = ylab, main = main,
         ylim = ylim, ...)
    ok <- is.finite(xx) & is.finite(yy)
    if (sum(ok) >= 3L) {
      sm <- stats::lowess(xx[ok], yy[ok])
      lines(sm, lwd = 2)
    }
    abline(v = x$best.size, lty = 2)
    mtext(sprintf("best treesize = %d, risk = %.4f",
                  x$best.size, x$best.err),
          line = 0.5)
  }
}
