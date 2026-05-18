####################################################################
##
## TDC Helper Functions
##
####################################################################
## converts a standard survival data set to counting process format
## scale is ignored but kept in place for legacy
convert.counting <- function(f, dta, scale = FALSE) {
  ## coerce formula
  f <- as.formula(f)
  ## extract names
  time.nm  <- all.vars(f)[1]
  event.nm <- all.vars(f)[2]
  ## remove missing values
  dta <- na.omit(dta)
  ## raw time
  time <- as.numeric(dta[[time.nm]])
  if (any(!is.finite(time))) {
    stop("Non-finite time values found.")
  }
  if (any(time < 0, na.rm = TRUE)) {
    stop("time values cannot be negative")
  }
  if (isTRUE(scale)) {
    warning("'scale=TRUE' is deprecated for RHF workflows: rhf() now maps time internally. Returning raw times.")
  }
  data.frame(id = seq_len(nrow(dta)),
             start = 0,
             stop = time,
             event = dta[[event.nm]],
             dta[, !(colnames(dta) %in% all.vars(f)[1:2]), drop = FALSE])
}
## clean up counting process data
cleanup.counting <- function(dta,
                             xvar.names = NULL,
                             yvar.names,
                             subj.names,
                             sorted = FALSE,
                             eps = 1e-6) {
  ## work out candidate x variables
  if (is.null(xvar.names)) {
    xvar.candidate <- setdiff(colnames(dta), c(subj.names, yvar.names))
  } else {
    missing.x <- setdiff(xvar.names, colnames(dta))
    if (length(missing.x) > 0L) {
      stop("The following xvar.names are not columns of 'dta': ",
           paste(missing.x, collapse = ", "))
    }
    xvar.candidate <- setdiff(unique(xvar.names), c(subj.names, yvar.names))
  }
  xvar.all   <- xvar.candidate
  xvar.names <- xvar.candidate
  ## coerce start/stop once, early
  start.nm <- yvar.names[1L]
  stop.nm  <- yvar.names[2L]
  for (nm in c(start.nm, stop.nm)) {
    v <- dta[[nm]]
    if (is.factor(v)) {
      v <- as.numeric(as.character(v))
    } else {
      v <- as.numeric(v)
    }
    dta[[nm]] <- v
  }
  startv <- dta[[start.nm]]
  stopv  <- dta[[stop.nm]]
  ## check if time is non-negative (ignoring NAs)
  if (any(startv < 0 | stopv < 0, na.rm = TRUE)) {
    stop("time values cannot be negative")
  }
  ## drop x variables with all missing values
  if (length(xvar.names) > 0L) {
    all.na <- vapply(
      xvar.names,
      function(z) all(is.na(dta[[z]])),
      logical(1L)
    )
    if (any(all.na)) {
      drop.x     <- xvar.names[all.na]
      dta        <- dta[, !(colnames(dta) %in% drop.x), drop = FALSE]
      xvar.names <- xvar.names[!all.na]
      warning("Dropping x variable(s) with all missing values: ",
              paste(drop.x, collapse = ", "))
    }
  }
  ## case-wise deletion of remaining missing data
  cols.to.check <- intersect(c(subj.names, yvar.names, xvar.names),
                             colnames(dta))
  if (length(cols.to.check) == 0L) {
    stop("No variables remain in data after removing predictors with all missing values.")
  }
  cc <- complete.cases(dta[, cols.to.check, drop = FALSE])
  if (!all(cc)) {
    n.rem <- sum(!cc)
    dta   <- dta[cc, , drop = FALSE]
    warning(sprintf("Removing %d row(s) with missing values.", n.rem))
  }
  ## checks after NA cleaning
  if (nrow(dta) == 0L) {
    stop("After processing (including removing missing values) there are no observations left (NULL data set).")
  }
  if (length(xvar.names) == 0L) {
    if (length(xvar.all) == 0L) {
      stop("No x variables were supplied (xvar.names is empty and no extra variables were found).")
    } else {
      stop("After removing missing values there are no x variables left.")
    }
  }
  ## one-pass stable sort by subject encounter-order, then start time.
  ## This preserves the original grouping semantics without the O(n * n_subjects)
  ## cost of lapply(unique(id), which(id == i), ...).
  if (isFALSE(sorted)) {
    id  <- dta[[subj.names]]
    ord <- order(match(id, id), dta[[start.nm]], method = "radix")
    if (!identical(ord, seq_len(nrow(dta)))) {
      dta <- dta[ord, , drop = FALSE]
    }
  }
  ## transform time to [0,1] using modified logit with tau = training max.time
  max.time <- max(dta[[stop.nm]], na.rm = TRUE)
  if (!is.finite(max.time) || max.time <= 0) {
    stop("Invalid max time in training data: max(stop) must be positive and finite.")
  }
  time.map <- list(method   = "mlogit",
                   tau      = as.double(max.time),
                   max.time = as.double(max.time))
  startv <- .forward.time(dta[[start.nm]], time.map)
  stopv  <- .forward.time(dta[[stop.nm]],  time.map)
  ## remove non-finite time values and any data points where stop<=start within epsilon tolerance
  bad <- (!is.finite(startv) | !is.finite(stopv) | (stopv <= startv + eps))
  bad[is.na(bad)] <- TRUE
  if (any(bad)) {
    keep <- !bad
    dta   <- dta[keep, , drop = FALSE]
    startv <- startv[keep]
    stopv  <- stopv[keep]
  }
  if (nrow(dta) == 0L) {
    stop("After processing there are no valid counting-process intervals left.")
  }
  dta[[start.nm]] <- startv
  dta[[stop.nm]]  <- stopv
  ## store helpful attributes
  attr(dta, "max.time")              <- max.time
  attr(dta, "time.map")              <- time.map
  attr(dta, "xvar.names")            <- xvar.names
  attr(dta, "sorted.by.subj.start")  <- TRUE
  dta
}
## helper to clean and scale new (test) counting-process data for predict.rhf
cleanup.counting.newdata <- function(newdata,
                                     xvar.names,
                                     yvar.names,
                                     subj.names,
                                     time.map,
                                     max.time,
                                     sorted = FALSE,
                                     eps = 1e-6,
                                     nonfinite.action = c("stop", "drop")) {
  nonfinite.action <- match.arg(nonfinite.action)
  if (missing(newdata) || is.null(newdata)) {
    stop("Argument 'newdata' must be a non-null data.frame.")
  }
  cn <- colnames(newdata)
  ## check subject variable present
  missing.subj <- setdiff(subj.names, cn)
  if (length(missing.subj) > 0L) {
    stop("Subject variable(s) not found in 'newdata': ",
         paste(missing.subj, collapse = ", "))
  }
  ## check predictor variables present (must match training forest)
  missing.x <- setdiff(xvar.names, cn)
  if (length(missing.x) > 0L) {
    stop("The following predictor variable(s) used to fit the forest are missing in 'newdata': ",
         paste(missing.x, collapse = ", "))
  }
  ## determine whether y is present in newdata
  y.in <- yvar.names %in% cn
  if (any(y.in) && !all(y.in)) {
    stop("Argument 'yvar.names' must refer to variables that are either all present in 'newdata' or all absent.")
  }
  yvar.present <- all(y.in)
  start.nm <- stop.nm <- NULL
  ## coerce start/stop once, early (if y present)
  if (yvar.present) {
    start.nm <- yvar.names[1L]
    stop.nm  <- yvar.names[2L]
    for (nm in c(start.nm, stop.nm)) {
      v <- newdata[[nm]]
      if (is.factor(v)) {
        v <- as.numeric(as.character(v))
      } else {
        v <- as.numeric(v)
      }
      newdata[[nm]] <- v
    }
    if (any(newdata[[start.nm]] < 0 | newdata[[stop.nm]] < 0, na.rm = TRUE)) {
      stop("time values in 'newdata' cannot be negative")
    }
    ## retained for legacy/interface sanity checks
    if (length(max.time) != 1L || !is.finite(max.time) || max.time <= 0) {
      stop("Invalid 'max.time' from training data: must be a positive finite scalar.")
    }
  }
  ## case-wise deletion of missing values in id, x, and (if present) y
  cols.to.check <- c(subj.names, xvar.names)
  if (yvar.present) {
    cols.to.check <- c(cols.to.check, yvar.names)
  }
  cols.to.check <- intersect(cols.to.check, cn)
  cc <- complete.cases(newdata[, cols.to.check, drop = FALSE])
  if (!all(cc)) {
    n.drop <- sum(!cc)
    warning(sprintf(
      "Removing %d row(s) from 'newdata' due to missing values in subject, y, or x.",
      n.drop
    ))
  }
  if (!any(cc)) {
    stop("After removing missing values from 'newdata' there are no observations left.")
  }
  dta <- newdata[cc, , drop = FALSE]
  ## one-pass stable sort by subject encounter-order, then start time.
  ## If y is absent, group by subject while preserving within-subject order.
  if (isFALSE(sorted)) {
    id <- dta[[subj.names]]
    if (yvar.present) {
      ord <- order(match(id, id), dta[[start.nm]], method = "radix")
    } else {
      ord <- order(match(id, id), seq_len(nrow(dta)), method = "radix")
    }
    if (!identical(ord, seq_len(nrow(dta)))) {
      dta <- dta[ord, , drop = FALSE]
    }
  }
  start.scaled <- stop.scaled <- NULL
  if (yvar.present) {
    ## transform start/stop outcomes to the internal time scale
    start.scaled <- .forward.time(dta[[start.nm]], time.map)
    stop.scaled  <- .forward.time(dta[[stop.nm]],  time.map)
    ## coherence check
    nonfinite <- (!is.finite(start.scaled) | !is.finite(stop.scaled))
    bad <- nonfinite | (stop.scaled <= start.scaled + eps)
    bad[is.na(bad)] <- TRUE
    if (any(nonfinite)) {
      msg <- sprintf("Found %d row(s) in 'newdata' with non-finite start/stop after scaling.",
                     sum(nonfinite))
      if (nonfinite.action == "stop") {
        stop(msg)
      } else {
        warning(msg)
      }
    }
    if (any(bad)) {
      dta <- dta[!bad, , drop = FALSE]
      start.scaled <- start.scaled[!bad]
      stop.scaled  <- stop.scaled[!bad]
    }
    if (nrow(dta) == 0L) {
      stop("After removing invalid/non-positive length intervals from 'newdata' there are no observations left.")
    }
  }
  attr(dta, "sorted.by.subj.start") <- isTRUE(yvar.present)
  ## initialise outputs only after final filtering to avoid extra copies
  subj.newdata <- dta[, subj.names]
  xvar.newdata <- dta[, xvar.names, drop = FALSE]
  yvar.newdata <- NULL
  if (yvar.present) {
    ## preserve original coercion behavior for non-time y columns
    yraw <- dta[, yvar.names, drop = FALSE]
    for (nm in yvar.names[1:2]) {
      v <- yraw[[nm]]
      if (is.factor(v)) v <- as.numeric(as.character(v))
      yraw[[nm]] <- as.numeric(v)
    }
    yvar.newdata <- as.matrix(yraw)
    storage.mode(yvar.newdata) <- "double"
    yvar.newdata[, 1L] <- start.scaled
    yvar.newdata[, 2L] <- stop.scaled
  }
  list(
    newdata      = dta,
    subj         = subj.newdata,
    xvar         = xvar.newdata,
    yvar         = yvar.newdata,
    yvar.present = yvar.present
  )
}
get.duplicated <- function(x) {
  c(apply(x, 2, function(xx) {
    1 * !all(xx == xx[1])
  }))
}
get.tdc.cov <- function(dta, subj.names, yvar.names,
                        presorted = isTRUE(attr(dta, "sorted.by.subj.start"))) {
  ## extract covariates
  x <- dta[, !(colnames(dta) %in% c(subj.names, yvar.names)), drop = FALSE]
  p <- ncol(x)
  if (p == 0L) {
    return(numeric(0L))
  }
  id <- dta[, subj.names]
  n  <- length(id)
  if (n <= 1L) {
    out <- rep(0L, p)
    names(out) <- colnames(x)
    return(out)
  }
  if (presorted) {
    ord <- seq_len(n)
    id.ord <- id
  } else {
    ord <- order(id, method = "radix")
    id.ord <- id[ord]
  }
  same <- id.ord[-1L] == id.ord[-n]
  if (!any(same, na.rm = TRUE)) {
    out <- rep(0L, p)
    names(out) <- colnames(x)
    return(out)
  }
  out <- vapply(
    x,
    function(v) {
      v <- v[ord]
      chg <- (v[-1L] != v[-n]) & same
      as.integer(any(chg, na.rm = TRUE))
    },
    integer(1L)
  )
  out
}
get.tdc.subj.time <- function(dta, subj.names, yvar.names,
                              presorted = isTRUE(attr(dta, "sorted.by.subj.start"))) {
  ## extract covariates
  x <- dta[, !(colnames(dta) %in% c(subj.names, yvar.names)), drop = FALSE]
  p <- ncol(x)
  ## preserve original behavior in trivial cases
  if (p == 0L) {
    return(rep(0L, nrow(dta)))
  }
  id <- dta[, subj.names]
  n  <- length(id)
  if (n <= 1L) {
    return(rep(0L, nrow(dta)))
  }
  if (presorted) {
    ord <- seq_len(n)
    id.ord <- id
  } else {
    ord <- order(id, method = "radix")
    id.ord <- id[ord]
  }
  same <- id.ord[-1L] == id.ord[-n]
  if (!any(same, na.rm = TRUE)) {
    return(rep(0L, nrow(dta)))
  }
  cov.tdc <- integer(p)
  names(cov.tdc) <- colnames(x)
  pair.chg <- logical(n - 1L)
  for (j in seq_len(p)) {
    v <- x[[j]][ord]
    chg <- (v[-1L] != v[-n]) & same
    cov.tdc[j] <- as.integer(any(chg, na.rm = TRUE))
    pair.chg <- pair.chg | (!is.na(chg) & chg)
  }
  if (sum(cov.tdc) == 0) {
    return(rep(0L, nrow(dta)))
  }
  pc <- as.integer(pair.chg)
  cs <- c(0L, cumsum(pc))
  rr <- rle(id.ord)
  ends <- cumsum(rr$lengths)
  starts <- ends - rr$lengths + 1L
  subj.has.chg <- (cs[ends] - cs[starts]) > 0L
  if (presorted) {
    out <- as.integer(subj.has.chg)
  } else {
    id.unq <- unique(id)
    by_id <- setNames(as.integer(subj.has.chg), as.character(rr$values))
    out <- unname(by_id[as.character(id.unq)])
  }
  out
}
#########################################################################################
##
## process survival information, set time.interest
##
#########################################################################################
timegrid.min.events <- function(data,
                                stop.col = "stop",
                                event.col = "event",
                                ntime = 50L,
                                min.events.per.gap = 10L,
                                include.first.event = FALSE,
                                merge.tail = TRUE) {
  if (!stop.col %in% names(data))  stop("stop.col not found in data")
  if (!event.col %in% names(data)) stop("event.col not found in data")
  ntime <- as.integer(ntime)
  if (is.na(ntime) || ntime < 0L) stop("ntime must be >= 0")
  min.events.per.gap <- as.integer(min.events.per.gap)
  if (is.na(min.events.per.gap) || min.events.per.gap < 1L) {
    stop("min.events.per.gap must be >= 1")
  }
  # Robust conversion for stop
  stopv <- data[[stop.col]]
  if (is.factor(stopv)) stopv <- as.numeric(as.character(stopv))
  stopv <- as.numeric(stopv)
  evv <- data[[event.col]]
  evv <- as.integer(!is.na(evv) & evv != 0L)
  ev_time <- stopv[evv == 1L]
  ev_time <- ev_time[is.finite(ev_time) & ev_time > 0]
  if (!length(ev_time)) stop("No positive event times found.")
  # Unique event times + event counts at each time
  ut <- sort(unique(ev_time))
  counts <- tabulate(match(ev_time, ut), nbins = length(ut))
  n_events <- sum(counts)
  # If ntime is 0: all unique event times
  if (is.null(ntime) || ntime == 0L) {
    grid <- ut
    attr(grid, "n_events") <- n_events
    attr(grid, "n_unique_event_times") <- length(ut)
    attr(grid, "min.events.per.gap_eff") <- NA_integer_
    return(grid)
  }
  # Effective events per gap combines both controls:
  # - never < min.events.per.gap
  # - tends to produce ~ntime grid points when possible
  m_eff <- max(min.events.per.gap, as.integer(ceiling(n_events / ntime)))
  m_eff <- max(1L, min(m_eff, n_events))
  grid_idx <- integer(0)
  acc <- 0L
  for (k in seq_along(ut)) {
    acc <- acc + counts[k]
    if (acc >= m_eff) {
      grid_idx <- c(grid_idx, k)
      acc <- 0L
    }
  }
  tmax <- ut[length(ut)]
  if (!length(grid_idx)) {
    grid <- tmax
  } else {
    grid <- ut[grid_idx]
    if (tail(grid, 1) < tmax) {
      if (merge.tail && acc > 0L && acc < m_eff) {
        grid[length(grid)] <- tmax
      } else {
        grid <- c(grid, tmax)
      }
    }
  }
  if (include.first.event) grid <- c(ut[1L], grid)
  grid <- sort(unique(grid))
  ## do not attach attributes, native code does not like that 
  #attr(grid, "n_events") <- n_events
  #attr(grid, "n_unique_event_times") <- length(ut)
  #attr(grid, "min.events.per.gap_eff") <- m_eff
  grid
}
get.grow.event.info <- function(yvar, fmly, need.deaths = TRUE, ntime, min.events.per.gap) {
  if (grepl("surv", fmly)) {
    ##-----------------------------------------------------------
    ## survival, competing risks, or time dependent covariates
    ##-----------------------------------------------------------
    if (dim(yvar)[2] == 2) {
      ##---------------------------------
      ## survival or competing risks:
      ##---------------------------------
      r.dim <- 2
      time <- yvar[, 1]
      cens <- yvar[, 2]
      start.time <- NULL
      ## censoring must be coded coherently
      if (!all(floor(cens) == abs(cens), na.rm = TRUE)) {
        stop("for survival families censoring variable must be coded as a non-negative integer (perhaps the formula is set incorrectly?)")
      }
      ## check if deaths are available (if user specified)
      if (need.deaths && (all(na.omit(cens) == 0))) {
        stop("no deaths in data!")
      }
      ## Check for event time consistency.
      ## we over-ride this now to allow for negative time (see Stute)
      ##if (!all(na.omit(time) >= 0)) {
      ##  stop("time must be  positive")
      ##}
      ## Extract the unique event types.
      event.type <- unique(na.omit(cens))
      ## Ensure they are all greater than or equal to zero.
      if (sum(event.type >= 0) != length(event.type)) {
        stop("censoring variable must be coded as NA, 0, or greater than 0.")
      }
      ## Discard the censored state, if it exists.
      event <- na.omit(cens)[na.omit(cens) > 0]
      event.type <- unique(event)
      ## Set grid of time points.
      nonMissingOutcome <- which(!is.na(cens) & !is.na(time))
      nonMissingDeathFlag <- (cens[nonMissingOutcome] != 0)
      time.interest <- sort(unique(time[nonMissingOutcome[nonMissingDeathFlag]]))
      ## trim the time points if the user has requested it
      ## we also allow the user to pass requested time points
      if (!is.null(ntime) && length(ntime) == 1 && ntime < 0) {
        ntime <- 0
      }
      if (!is.null(ntime) && !((length(ntime) == 1) && ntime == 0)) {
        if (length(ntime) == 1 && length(time.interest) > ntime) {
          time.interest <- time.interest[
            seq(1, length(time.interest), length = ntime)]
        }
        if (length(ntime) > 1) {
          time.interest <- unique(sapply(ntime, function(tt) {
            time.interest[max(1, sum(tt >= time.interest, na.rm = TRUE))]
          }))
        }
      }
    }
    ##-------------------------------
    ## time dependent covariates:
    ##-------------------------------
    else {
      r.dim <- 3
      start.time <- yvar[, 1]
      time <- yvar[, 2]
      cens <- yvar[, 3]
      ## censoring must be coded coherently
      if (!all(floor(cens) == abs(cens), na.rm = TRUE)) {
        stop("for survival families censoring variable must be coded as a non-negative integer (perhaps the formula is set incorrectly?)")
      }
      ## check if deaths are available (if user specified)
      if (need.deaths && (all(na.omit(cens) == 0))) {
        stop("no deaths in data!")
      }
      ## Check for event time consistency.
      if (!all(na.omit(time) >= 0)) {
        stop("time must be  positive")
      }
      ## Extract the unique event types.
      event.type <- unique(na.omit(cens))
      ## Ensure they are all greater than or equal to zero.
      if (sum(event.type >= 0) != length(event.type)) {
        stop("censoring variable must be coded as NA, 0, or greater than 0.")
      }
      ## Discard the censored state, if it exists.
      event <- na.omit(cens)[na.omit(cens) > 0]
      event.type <- unique(event)
      ## Set grid of time points.
      nonMissingOutcome <- which(!is.na(cens) & !is.na(time))
      nonMissingDeathFlag <- (cens[nonMissingOutcome] != 0)
      time.interest <- sort(unique(time[nonMissingOutcome[nonMissingDeathFlag]]))
      ## trim the time points if the user has requested it
      ## we also allow the user to pass requested time points
      if (!is.null(ntime) && length(ntime) == 1 && ntime < 0) {
        ntime <- 0
      }
      if (!is.null(ntime) && !((length(ntime) == 1) && ntime == 0)) {
        if (length(ntime) == 1) {
          time.interest <- timegrid.min.events(data.frame(stop=time, event=cens),
                                               ntime = ntime,
                                               min.events.per.gap = min.events.per.gap)
        }
        else {
          time.interest <- unique(sapply(ntime, function(tt) {
            time.interest[max(1, sum(tt >= time.interest, na.rm = TRUE))]
          }))
        }
      }
    }
  }
  ##---------------------
  ## other families
  ##---------------------
  else {
    if ((fmly == "regr+") | (fmly == "class+") | (fmly == "mix+")) {
      r.dim <- dim(yvar)[2]
    }
    else {
      if (fmly == "unsupv") {
        r.dim <- 0
      }
      else {
        r.dim <- 1
      }
    }
    event <- event.type <- cens <- time.interest <- cens <- time <- start.time <- NULL
  }
  return(list(event = event, event.type = event.type, cens = cens,
              time.interest = time.interest,
              time = time, start.time = start.time, r.dim = r.dim))
}
####################################################################
##
## TDC Helper Functions for hazards
## - only coherent for single or time static trees
##
####################################################################
hazard.to.chf <- function(o, max.time = 1) {
  tme.delta <- diff(c(0, o$time.interest))
  hz <- if (!is.null(o$hazard.oob)) o$hazard.oob else o$hazard
  t(apply(hz, 1, function(h) cumsum(h * tme.delta)))
}
#########################################################################################
##
## convert.standard.counting()
##
## Collapse counting-process (start/stop) data to a single-row-per-ID dataset.
##
## Original purpose:
##   - Quickly convert counting-process survival data to a standard survival
##     data set using a baseline snapshot of covariates.
##
## Extension (landmarking):
##   - Provide a landmark snapshot at a user-specified time (landmark.time)
##     using the covariate row whose [start, stop) interval contains t0^-.
##   - Optionally return the row index used for the snapshot (row_index)
##     which is useful for extracting record-level predictions (e.g., CHF)
##     from survival forests fit on the long (record-level) data.
##
## Notes:
##   - The function supports a "pseudo" Surv() with an ID argument:
##       Surv(id, start, stop, event)
##     as well as the standard counting-process Surv():
##       Surv(start, stop, event)
##     In the latter case, id.default is used.
##   - landmark.time is assumed to be on the *same scale* as the start/stop
##     columns in `data`.
##
#########################################################################################
convert.standard.counting <- function(formula, data,
                                      scale             = FALSE,
                                      rescale.from.attr = FALSE,
                                      keep.id           = FALSE,
                                      keep.row_index    = FALSE,
                                      sorted            = FALSE,
                                      id.default        = "id",
                                      eps               = 1e-8,
                                      landmark.time     = NULL,
                                      landmark.use.tminus = TRUE,
                                      return.type       = c("survival", "x"),
                                      keep.landmark.cols = FALSE) {
  return.type <- match.arg(return.type)
  ## ---- parse Surv(...) on LHS ----
  f   <- as.formula(formula)
  lhs <- f[[2]]
  if (!is.call(lhs) || !identical(as.character(lhs[[1]]), "Surv")) {
    stop("Left-hand side of formula must be a Surv(...) call.")
  }
  getnm <- function(z) paste(deparse(z), collapse = "")
  L <- as.list(lhs)[-1]  # drop 'Surv'
  if (length(L) == 4L) {
    ## Surv(id, start, stop, event)
    subj.nm  <- getnm(L[[1]])
    start.nm <- getnm(L[[2]])
    stop.nm  <- getnm(L[[3]])
    event.nm <- getnm(L[[4]])
  } else if (length(L) == 3L) {
    ## Surv(start, stop, event) -> use default id column
    subj.nm  <- id.default
    start.nm <- getnm(L[[1]])
    stop.nm  <- getnm(L[[2]])
    event.nm <- getnm(L[[3]])
  } else {
    stop("Expecting Surv(id, start, stop, event) or Surv(start, stop, event).")
  }
  ## required columns
  req  <- c(subj.nm, start.nm, stop.nm, event.nm)
  miss <- setdiff(req, names(data))
  if (length(miss)) {
    stop("Missing required columns in data: ", paste(miss, collapse = ", "))
  }
  id         <- data[[subj.nm]]
  start.time <- data[[start.nm]]
  stop.time  <- data[[stop.nm]]
  event.val  <- data[[event.nm]]
  if (any(!is.finite(start.time) | !is.finite(stop.time))) {
    stop("Non-finite start/stop values found.")
  }
  if (any(stop.time < start.time - eps, na.rm = TRUE)) {
    stop("Found rows with stop < start beyond tolerance.")
  }
  ## Optional: rescale start/stop times (and landmark.time) using attr(data, 'max.time')
  ## This is mainly useful if start/stop were stored on [0,1] and you want
  ## to work in original units.
  if (isTRUE(rescale.from.attr)) {
    tm <- attr(data, "time.map")
    if (!is.null(tm)) {
      start.time <- .inverse.time(start.time, tm)
      stop.time  <- .inverse.time(stop.time,  tm)
      if (!is.null(landmark.time)) {
        landmark.time <- .inverse.time(landmark.time, tm)
      }
    } else {
      mt <- attr(data, "max.time")
      if (!is.null(mt) && is.finite(mt) && mt > 0) {
        start.time <- start.time * mt
        stop.time  <- stop.time  * mt
        if (!is.null(landmark.time)) {
          landmark.time <- landmark.time * mt
        }
      }
    }
  }
  ## Original code silently drops NA ids because which(id == NA) returns integer(0).
  ## Preserve that behavior, but do it up front to avoid carrying empty groups around.
  valid.id <- !is.na(id)
  if (!any(valid.id)) {
    return(data.frame())
  }
  if (!all(valid.id)) {
    row.map    <- which(valid.id)
    id         <- id[valid.id]
    start.time <- start.time[valid.id]
    stop.time  <- stop.time[valid.id]
    event.val  <- event.val[valid.id]
  } else {
    row.map <- seq_along(id)
  }
  n <- length(id)
  x.cols <- setdiff(names(data), req)
  ## One-pass grouping strategy:
  ## - preserve subject encounter order via match(id, id)
  ## - if sorted=FALSE, sort within subject by (start, stop)
  ## - if sorted=TRUE, preserve original within-subject row order
  presorted <- isTRUE(attr(data, "sorted.by.subj.start"))
  grp <- match(id, id)
  if (sorted && presorted) {
    ord <- seq_len(n)
  } else if (sorted) {
    ord <- order(grp, seq_len(n), method = "radix")
  } else {
    ord <- order(grp, start.time, stop.time, method = "radix")
  }
  id.ord    <- id[ord]
  start.ord <- start.time[ord]
  stop.ord  <- stop.time[ord]
  event.ord <- event.val[ord]
  row.ord   <- row.map[ord]
  rr <- rle(id.ord)
  uid.ord <- rr$values
  ends   <- cumsum(rr$lengths)
  starts <- ends - rr$lengths + 1L
  n.id   <- length(uid.ord)
  keep.vec <- rep(TRUE, n.id)
  time.vec <- numeric(n.id)
  event.vec <- integer(n.id)
  base.row <- integer(n.id)
  if (!is.null(landmark.time)) {
    t0 <- as.numeric(landmark.time)
    if (!is.finite(t0)) {
      stop("landmark.time must be finite.")
    }
    t.eff <- if (isTRUE(landmark.use.tminus)) (t0 - eps) else t0
    if (isTRUE(keep.landmark.cols)) {
      landmark.time.vec <- rep(NA_real_, n.id)
      t.end.vec         <- rep(NA_real_, n.id)
      t.end.event.vec   <- rep(NA_real_, n.id)
    }
  } else {
    t0 <- NULL
  }
  ## Scan each subject exactly once using group boundaries.
  for (ii in seq_len(n.id)) {
    s <- starts[ii]
    e <- ends[ii]
    start.slice <- start.ord[s:e]
    stop.slice  <- stop.ord[s:e]
    ## end of follow-up for this id
    t.end <- max(stop.slice, na.rm = TRUE)
    ## last row at end time (used for event indicator)
    last.local <- tail(which(stop.slice >= t.end - eps), 1L)
    last.idx   <- s + last.local - 1L
    event.end  <- as.integer(event.ord[last.idx] > 0)
    ## default: baseline snapshot row
    base.local <- 1L
    time.i     <- t.end
    event.i    <- event.end
    if (!is.null(t0)) {
      ## exclude those not at risk at landmark
      if (t.end <= t0 + eps) {
        keep.vec[ii] <- FALSE
        next
      }
      ## find interval containing t_eff: start <= t_eff < stop
      cand <- which(start.slice <= t.eff & stop.slice > t.eff)
      if (length(cand)) {
        ## if multiple, take the one with largest start
        base.local <- cand[which.max(start.slice[cand])]
      } else {
        ## fallback: last row with stop <= t0 (if exists), else first row
        cand2 <- which(stop.slice <= t0 + eps)
        if (length(cand2)) {
          base.local <- cand2[which.max(stop.slice[cand2])]
        } else {
          base.local <- 1L
        }
      }
      ## define outcome relative to landmark (standard landmarking)
      time.i  <- t.end - t0
      event.i <- as.integer(event.end == 1L && t.end > t0 + eps)
      if (isTRUE(keep.landmark.cols)) {
        landmark.time.vec[ii] <- t0
        t.end.vec[ii]         <- t.end
        t.end.event.vec[ii]   <- if (isTRUE(event.end == 1L)) t.end else NA_real_
      }
    }
    base.row[ii]  <- row.ord[s + base.local - 1L]
    time.vec[ii]  <- time.i
    event.vec[ii] <- event.i
  }
  uid.keep   <- uid.ord[keep.vec]
  base.row   <- base.row[keep.vec]
  time.vec   <- time.vec[keep.vec]
  event.vec  <- event.vec[keep.vec]
  if (!length(time.vec)) {
    return(data.frame())
  }
  if (!is.null(t0) && isTRUE(keep.landmark.cols)) {
    landmark.time.vec <- landmark.time.vec[keep.vec]
    t.end.vec         <- t.end.vec[keep.vec]
    t.end.event.vec   <- t.end.event.vec[keep.vec]
  }
  if (isTRUE(scale)) {
    mx <- max(time.vec, na.rm = TRUE)
    if (is.finite(mx) && mx > 0) {
      time.vec <- time.vec / mx
    }
  }
  out <- NULL
  if (return.type == "survival") {
    out <- data.frame(time = time.vec, event = event.vec, check.names = FALSE)
  }
  ## bind covariates with one direct subset instead of per-id pieces + rbind
  if (length(x.cols)) {
    cov.rows <- data[base.row, x.cols, drop = FALSE]
    rownames(cov.rows) <- NULL
    if (is.null(out)) {
      out <- data.frame(cov.rows, check.names = FALSE)
    } else {
      out <- data.frame(out, cov.rows, check.names = FALSE)
    }
  } else {
    if (is.null(out)) {
      out <- data.frame(check.names = FALSE)
    }
  }
  ## optional helper columns
  if (isTRUE(keep.row_index)) {
    out <- data.frame(row_index = base.row, out, check.names = FALSE)
  }
  if (!is.null(t0) && isTRUE(keep.landmark.cols)) {
    out <- data.frame(landmark_time = landmark.time.vec,
                      t_end = t.end.vec,
                      t_end_event = t.end.event.vec,
                      out,
                      check.names = FALSE)
  }
  if (isTRUE(keep.id)) {
    id.df <- setNames(data.frame(uid.keep, check.names = FALSE), subj.nm)
    out   <- data.frame(id.df, out, check.names = FALSE)
  }
  out
}
