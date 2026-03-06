####################################################################
##
## TDC Helper Functions
##
####################################################################
## converts a standard survival data set to counting process format
convert.counting <- function(f, dta, scale = FALSE) {
  ## coherce formula
  f <- as.formula(f)
  ## extract name of time
  time.nm <- all.vars(f)[1]
  event.nm <- all.vars(f)[2]
  ## remove all NA values
  dta <- na.omit(dta)
  ## map time to [0, 1]
  time <- dta[, time.nm]
  if (scale) {
    time <- time / max(time)
  }
  ## use survSplit with cutoff at zero to process to desired format
  data.frame(id = 1:nrow(dta),
             start = 0,
             stop = time,
             event = dta[, event.nm],
             dta[, !(colnames(dta) %in% all.vars(f)[1:2]), drop = FALSE])
}
## clean up counting process data
cleanup.counting <- function(dta, xvar.names = NULL, yvar.names, subj.names, sorted = FALSE, eps = 1e-6) {
  ## work out candidate x variables
  if (is.null(xvar.names)) {
    ## default: all non-subject, non-y columns
    xvar.candidate <- setdiff(colnames(dta), c(subj.names, yvar.names))
  } else {
    ## user-specified x variables
    missing.x <- setdiff(xvar.names, colnames(dta))
    if (length(missing.x) > 0L) {
      stop("The following xvar.names are not columns of 'dta': ",
           paste(missing.x, collapse = ", "))
    }
    ## drop any subject / y columns accidentally listed as x
    xvar.candidate <- setdiff(unique(xvar.names), c(subj.names, yvar.names))
  }
  xvar.all   <- xvar.candidate  ## original set of x variables
  xvar.names <- xvar.candidate  ## mutable copy used below
  ## check if time is non-negative (ignoring NAs)
  if (any(dta[, yvar.names[1:2]] < 0, na.rm = TRUE)) {
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
    dta   <- dta[cc,, drop = FALSE]
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
  ## if sorted is requested, convert id to sequential integer
  id <- dta[, subj.names]
  if (sorted) {
    id <- as.numeric(factor(id))
  }
  ## order the data by time within id for convenience
  nms <- colnames(dta)
  dta <- data.frame(do.call(rbind, lapply(unique(id), function(i) {
    pt  <- which(id == i)
    o.r <- order(dta[pt, yvar.names[1]])
    dta[pt[o.r],, drop = FALSE]
  })))
  colnames(dta) <- nms
  ## ensure start/stop are numeric (avoid factor surprises)
  for (nm in yvar.names[1:2]) {
    v <- dta[[nm]]
    if (is.factor(v)) v <- as.numeric(as.character(v))
    dta[[nm]] <- as.numeric(v)
  }
  ## scale time to [0,1]
  max.time <- max(dta[[yvar.names[2]]])
  if (!is.finite(max.time) || max.time <= 0) {
    stop("Invalid max time in training data: max(stop) must be positive and finite.")
  }
  dta[, yvar.names[1:2]] <- dta[, yvar.names[1:2]] / max.time
  ## remove non-finite time values and any data points where stop<=start within epsilon tolerance
  startv <- dta[[yvar.names[1]]]
  stopv  <- dta[[yvar.names[2]]]
  bad <- (!is.finite(startv) | !is.finite(stopv) | (stopv <= startv + eps))
  ## treat any NA comparisons as invalid (drop)
  bad[is.na(bad)] <- TRUE
  if (any(bad)) {
    dta <- dta[!bad,, drop = FALSE]
  }
  if (nrow(dta) == 0L) {
    stop("After processing there are no valid counting-process intervals left.")
  }
  ## store helpful attributes
  attr(dta, "max.time")   <- max.time
  attr(dta, "xvar.names") <- xvar.names
  dta
}
## helper to clean and scale new (test) counting-process data for predict.rhf
cleanup.counting.newdata <- function(newdata,
                                     subj.names,
                                     yvar.names,
                                     xvar.names,
                                     max.time,
                                     eps = 1e-6,
                                     nonfinite.action = c("stop", "drop")) {
  nonfinite.action <- match.arg(nonfinite.action)
  if (missing(newdata) || is.null(newdata)) {
    stop("Argument 'newdata' must be a non-null data.frame.")
  }
  ## check subject variable present
  cn <- colnames(newdata)
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
  dta <- newdata
  ## basic check on time for newdata if y present
  if (yvar.present) {
    if (any(dta[, yvar.names[1:2]] < 0, na.rm = TRUE)) {
      stop("time values in 'newdata' cannot be negative")
    }
    if (length(max.time) != 1L || !is.finite(max.time) || max.time <= 0) {
      stop("Invalid 'max.time' from training data: must be a positive finite scalar.")
    }
  }
  ## case-wise deletion of missing values in id, x, and (if present) y
  cols.to.check <- c(subj.names, xvar.names)
  if (yvar.present) {
    cols.to.check <- c(cols.to.check, yvar.names)
  }
  cols.to.check <- intersect(cols.to.check, colnames(dta))
  cc <- complete.cases(dta[, cols.to.check, drop = FALSE])
  if (!all(cc)) {
    n.drop <- sum(!cc)
    dta    <- dta[cc, , drop = FALSE]
    warning(sprintf(
      "Removing %d row(s) from 'newdata' due to missing values in subject, y, or x.",
      n.drop
    ))
  }
  if (nrow(dta) == 0L) {
    stop("After removing missing values from 'newdata' there are no observations left.")
  }
  ## initialise outputs
  subj.newdata <- dta[, subj.names]
  xvar.newdata <- dta[, xvar.names, drop = FALSE]
  yvar.newdata <- NULL
  if (yvar.present) {
    ## extract y and coerce start/stop to numeric (avoid factor surprises)
    yraw <- dta[, yvar.names, drop = FALSE]
    for (nm in yvar.names[1:2]) {
      v <- yraw[[nm]]
      if (is.factor(v)) v <- as.numeric(as.character(v))
      yraw[[nm]] <- as.numeric(v)
    }
    yvar.newdata <- as.matrix(yraw)
    storage.mode(yvar.newdata) <- "double"
    ## scale y to [0,1] using *training* max.time
    ## (explicit arithmetic avoids any surprises from scale()/data.frame coercion)
    yvar.newdata[, 1L] <- yvar.newdata[, 1L] / max.time
    yvar.newdata[, 2L] <- yvar.newdata[, 2L] / max.time
    ## cap scaled time at 1 (equivalent to capping raw time at max.time)
    yvar.newdata[, 1:2] <- pmin(yvar.newdata[, 1:2, drop = FALSE], 1)
    ## identify invalid intervals: non-finite time OR stop<=start (within eps)
    bad <- (!is.finite(yvar.newdata[, 1L]) |
            !is.finite(yvar.newdata[, 2L]) |
            (yvar.newdata[, 2L] <= yvar.newdata[, 1L] + eps))
    ## treat any NA comparison results as invalid
    bad[is.na(bad)] <- TRUE
    if (any(bad)) {
      if (any(!is.finite(yvar.newdata[bad, 1L])) || any(!is.finite(yvar.newdata[bad, 2L]))) {
        msg <- sprintf("Found %d row(s) in 'newdata' with non-finite start/stop after scaling.",
                       sum(!is.finite(yvar.newdata[, 1L]) | !is.finite(yvar.newdata[, 2L])))
        if (nonfinite.action == "stop") stop(msg) else warning(msg)
      }
      dta          <- dta[!bad, , drop = FALSE]
      subj.newdata <- subj.newdata[!bad]
      xvar.newdata <- xvar.newdata[!bad, , drop = FALSE]
      yvar.newdata <- yvar.newdata[!bad, , drop = FALSE]
    }
    if (nrow(dta) == 0L) {
      stop("After removing invalid/non-positive length intervals from 'newdata' there are no observations left.")
    }
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
get.tdc.cov <- function(dta, subj.names, yvar.names) {
  ## extract covariates
  x <- dta[, !(colnames(dta) %in% c(subj.names, yvar.names)), drop = FALSE]
  p <- ncol(x)
  if (p == 0L) {
    return(numeric(0L))
  }
  ## for looping across the id (does not assume id is sorted)
  id <- dta[, subj.names]
  n <- length(id)
  ## trivial cases
  if (n <= 1L) {
    out <- rep(0L, p)
    names(out) <- colnames(x)
    return(out)
  }
  ## order rows by id so that within-id comparisons are contiguous
  ord <- order(id)
  id.ord <- id[ord]
  ## adjacent rows that belong to the same subject
  same <- id.ord[-1L] == id.ord[-n]
  ## if every subject appears at most once, nothing can be time-dependent
  if (!any(same, na.rm = TRUE)) {
    out <- rep(0L, p)
    names(out) <- colnames(x)
    return(out)
  }
  ## A covariate is time-dependent if it changes at least once within any subject.
  ## Detect this by looking for any within-id change between adjacent rows after ordering by id.
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
get.tdc.subj.time <- function(dta, subj.names, yvar.names) {
  ## extract covariates
  x <- dta[, !(colnames(dta) %in% c(subj.names, yvar.names)), drop = FALSE]
  p <- ncol(x)
  ## if there are no covariates, there can be no TDC
  if (p == 0L) {
    return(rep(0L, nrow(dta)))
  }
  ## for looping across the id (id should *not* be sorted)
  id <- dta[, subj.names]
  n <- length(id)
  ## trivial cases
  if (n <= 1L) {
    return(rep(0L, nrow(dta)))
  }
  ## order rows by id so that within-id comparisons are contiguous
  ord <- order(id)
  id.ord <- id[ord]
  ## adjacent rows that belong to the same subject
  same <- id.ord[-1L] == id.ord[-n]
  ## if every subject appears at most once, there can be no time-dependent covariates
  if (!any(same, na.rm = TRUE)) {
    return(rep(0L, nrow(dta)))
  }
  ## track (i) which covariates are time-dependent (any within-id change),
  ## and (ii) which subjects have any time-dependent covariate.
  cov.tdc <- integer(p)
  names(cov.tdc) <- colnames(x)
  ## per-adjacent-pair flag: TRUE if ANY covariate changes between row k and k+1
  ## within the same subject (after ordering by id)
  pair.chg <- logical(n - 1L)
  for (j in seq_len(p)) {
    v <- x[[j]][ord]
    chg <- (v[-1L] != v[-n]) & same
    ## covariate is TDC if it changes anywhere within any subject
    cov.tdc[j] <- as.integer(any(chg, na.rm = TRUE))
    ## subject-level TDC: mark boundaries where *any* covariate changes
    ## treat NA comparisons as "no evidence of change"
    pair.chg <- pair.chg | (!is.na(chg) & chg)
  }
  ## first check if there are time dependent covariates (legacy behavior)
  if (sum(cov.tdc) == 0) {
    return(rep(0L, nrow(dta)))
  }
  ## Summarize pair.chg to the subject level.
  ## Each pair index k corresponds to the boundary between ordered rows k and k+1.
  ## For a subject with rows s..e, the internal boundaries are k = s..(e-1).
  pc <- as.integer(pair.chg)
  cs <- c(0L, cumsum(pc))  ## length n
  rr <- rle(id.ord)
  ends <- cumsum(rr$lengths)
  starts <- ends - rr$lengths + 1L
  subj.has.chg <- (cs[ends] - cs[starts]) > 0L
  ## return 1 if subject i has any tdc covariate (in original encounter order)
  id.unq <- unique(id)
  by_id <- setNames(as.integer(subj.has.chg), as.character(rr$values))
  out <- unname(by_id[as.character(id.unq)])
  out
}
#########################################################################################
##
## process survival information, set time.interest
##
#########################################################################################
timegrid.min_events <- function(data,
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
          time.interest <- timegrid.min_events(data.frame(stop=time, event=cens),
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
hazard.to.chf <- function(o, max.time=1) {
  tme.delta <- diff(c(0, o$time.interest))
  if (!is.null(o$hazard.oob)) {
    t(apply(max.time * o$hazard.oob, 1, function(hz) {cumsum(hz * tme.delta)}))
  }
  else {
    t(apply(max.time * o$hazard, 1, function(hz) {cumsum(hz * tme.delta)}))
  }
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
  if (length(miss)) stop("Missing required columns in data: ", paste(miss, collapse = ", "))
  id         <- data[[subj.nm]]
  start.time <- data[[start.nm]]
  stop.time  <- data[[stop.nm]]
  event.val  <- data[[event.nm]]
  if (any(!is.finite(start.time) | !is.finite(stop.time)))
    stop("Non-finite start/stop values found.")
  if (any(stop.time < start.time - eps, na.rm = TRUE))
    stop("Found rows with stop < start beyond tolerance.")
  ## Optional: rescale start/stop times (and landmark.time) using attr(data, 'max.time')
  ## This is mainly useful if start/stop were stored on [0,1] and you want
  ## to work in original units.
  if (isTRUE(rescale.from.attr)) {
    mt <- attr(data, "max.time")
    if (!is.null(mt) && is.finite(mt) && mt > 0) {
      start.time <- start.time * mt
      stop.time  <- stop.time  * mt
      if (!is.null(landmark.time)) landmark.time <- landmark.time * mt
    }
  }
  uid    <- unique(id)                       # preserve encounter order
  x.cols <- setdiff(names(data), req)        # candidate covariates
  ## ---- per-id snapshot/time/event ----
  get_one <- function(ii) {
    rows <- which(id == ii)
    if (!length(rows)) return(NULL)
    if (!sorted) {
      rows <- rows[order(start.time[rows], stop.time[rows])]
    }
    ## end of follow-up for this id
    t_end <- max(stop.time[rows], na.rm = TRUE)
    ## last row at end time (used for event indicator)
    last_rows <- rows[stop.time[rows] >= t_end - eps]
    last_row  <- tail(last_rows, 1L)
    event_end <- as.integer(event.val[last_row] > 0)
    ## choose baseline snapshot row
    base_row <- rows[1L]
    time_i  <- t_end
    event_i <- event_end
    ## landmark mode
    if (!is.null(landmark.time)) {
      t0 <- as.numeric(landmark.time)
      if (!is.finite(t0)) stop("landmark.time must be finite.")
      ## exclude those not at risk at landmark
      if (t_end <= t0 + eps) return(NULL)
      ## pick t0^- to avoid boundary ambiguity
      t_eff <- if (isTRUE(landmark.use.tminus)) (t0 - eps) else t0
      ## find interval containing t_eff: start <= t_eff < stop
      cand <- rows[start.time[rows] <= t_eff & stop.time[rows] > t_eff]
      if (length(cand)) {
        ## if multiple, take the one with largest start
        base_row <- cand[which.max(start.time[cand])]
      } else {
        ## fallback: last row with stop <= t0 (if exists), else first row
        cand2 <- rows[stop.time[rows] <= t0 + eps]
        if (length(cand2)) {
          base_row <- cand2[which.max(stop.time[cand2])]
        } else {
          base_row <- rows[1L]
        }
      }
      ## define outcome relative to landmark (standard landmarking)
      time_i  <- t_end - t0
      event_i <- as.integer(event_end == 1L && t_end > t0 + eps)
      if (isTRUE(keep.landmark.cols)) {
        ## store some additional information
        lm_cols <- list(landmark_time = t0,
                        t_end = t_end,
                        t_end_event = if (event_end == 1L) t_end else NA_real_)
      } else {
        lm_cols <- NULL
      }
    } else {
      lm_cols <- NULL
    }
    base_cov <- if (length(x.cols)) data[base_row, x.cols, drop = FALSE] else NULL
    out <- list(time = time_i, event = event_i, cov = base_cov, row_index = base_row)
    if (!is.null(lm_cols)) out <- c(out, lm_cols)
    out
  }
  pieces <- lapply(uid, get_one)
  keep   <- vapply(pieces, Negate(is.null), logical(1))
  pieces <- pieces[keep]
  uid    <- uid[keep]
  if (!length(pieces)) {
    ## return empty frame with correct columns
    out <- data.frame()
    return(out)
  }
  ## assemble survival part
  time_vec  <- vapply(pieces, `[[`, numeric(1),  "time")
  event_vec <- vapply(pieces, `[[`, integer(1),  "event")
  if (isTRUE(scale)) {
    mx <- max(time_vec, na.rm = TRUE)
    if (mx > 0) time_vec <- time_vec / mx
  }
  out <- NULL
  if (return.type == "survival") {
    out <- data.frame(time = time_vec, event = event_vec, check.names = FALSE)
  }
  ## bind covariates
  if (length(x.cols)) {
    cov_rows <- do.call(rbind, lapply(pieces, `[[`, "cov"))
    rownames(cov_rows) <- NULL
    if (is.null(out)) {
      out <- data.frame(cov_rows, check.names = FALSE)
    } else {
      out <- data.frame(out, cov_rows, check.names = FALSE)
    }
  } else {
    if (is.null(out)) out <- data.frame(check.names = FALSE)
  }
  ## optional helper columns
  if (isTRUE(keep.row_index)) {
    row_index <- vapply(pieces, `[[`, integer(1), "row_index")
    out <- data.frame(row_index = row_index, out, check.names = FALSE)
  }
  if (!is.null(landmark.time) && isTRUE(keep.landmark.cols)) {
    landmark_time <- vapply(pieces, function(p) if (!is.null(p$landmark_time)) p$landmark_time else NA_real_, numeric(1))
    t_end         <- vapply(pieces, function(p) if (!is.null(p$t_end)) p$t_end else NA_real_, numeric(1))
    t_end_event   <- vapply(pieces, function(p) if (!is.null(p$t_end_event)) p$t_end_event else NA_real_, numeric(1))
    out <- data.frame(landmark_time = landmark_time,
                      t_end = t_end,
                      t_end_event = t_end_event,
                      out,
                      check.names = FALSE)
  }
  if (isTRUE(keep.id)) {
    id_df <- setNames(data.frame(uid, check.names = FALSE), subj.nm)
    out   <- data.frame(id_df, out, check.names = FALSE)
  }
  out
}
