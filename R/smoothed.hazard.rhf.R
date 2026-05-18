.rhf_delta <- function(time) {
  time <- as.numeric(time)
  if (length(time) < 1L) stop("time must have positive length")
  if (anyNA(time)) stop("time contains NA")
  if (any(diff(time) <= 0)) stop("time must be strictly increasing")
  c(time[1L], diff(time))
}
.rhf_chf_from_hazard <- function(hazard, time) {
  # hazard: n x q matrix
  hazard <- as.matrix(hazard)
  time <- as.numeric(time)
  q <- length(time)
  if (ncol(hazard) != q) stop("hazard ncol must match length(time)")
  dt <- .rhf_delta(time)
  hd <- sweep(hazard, 2L, dt, `*`)
  t(apply(hd, 1L, cumsum))
}
.rhf_loess_smooth_vec <- function(z, x, span, degree, family, suppress.warnings = TRUE) {
  # Smooth a single curve z(x). Returns vector length(x).
  z <- as.numeric(z)
  x <- as.numeric(x)
  ok <- is.finite(z) & is.finite(x)
  if (sum(ok) < max(6L, degree + 2L)) return(z)
  df <- data.frame(x = x[ok], z = z[ok])
  .muffle <- function(expr) {
    if (!suppress.warnings) return(expr)
    withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
  }
  fit <- try(
    .muffle(
      stats::loess(
        z ~ x,
        data = df,
        span = span,
        degree = degree,
        family = family,
        control = stats::loess.control(surface = "direct")
      )
    ),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) return(z)
  pred <- try(
    .muffle(stats::predict(fit, newdata = data.frame(x = x))),
    silent = TRUE
  )
  if (inherits(pred, "try-error")) return(z)
  pred <- as.numeric(pred)
  if (all(!is.finite(pred))) return(z)
  # Fill any NA/Inf predictions with original values
  bad <- !is.finite(pred)
  if (any(bad)) pred[bad] <- z[bad]
  pred
}
.rhf_smooth_loghazard_matrix <- function(Z, time, span, degree, family, suppress.warnings = TRUE) {
  Z <- as.matrix(Z)
  time <- as.numeric(time)
  if (ncol(Z) != length(time)) stop("Z ncol must match length(time)")
  t(apply(
    Z, 1L, .rhf_loess_smooth_vec,
    x = time, span = span, degree = degree, family = family,
    suppress.warnings = suppress.warnings
  ))
}
.rhf_map_record_ids <- function(o) {
  id_raw <- o$id
  if (is.null(id_raw)) stop("RHF object is missing $id")
  if (!is.null(o$ensemble.id)) {
    id <- match(id_raw, o$ensemble.id)
    if (anyNA(id)) stop("Unable to map o$id onto o$ensemble.id")
  } else {
    id <- id_raw
  }
  as.integer(id)
}
.rhf_get_hazard_matrix <- function(o, target = c("oob", "inbag", "test")) {
  target <- match.arg(target)
  nm <- paste0("hazard.", target)
  if (!is.null(o[[nm]])) return(as.matrix(o[[nm]]))
  stop("Object is missing ", nm)
}
.rhf_loghazard_median_tree <- function(o, target = c("oob", "inbag", "both", "test"), eps = 1e-12) {
  target <- match.arg(target)
  if (is.null(o$forest) || is.null(o$forest$t.hazard)) {
    stop("RHF object must contain forest$t.hazard. Fit with forest=TRUE and keep.forest=TRUE.")
  }
  time <- as.numeric(o$time.interest)
  if (length(time) < 1L) stop("Missing/empty o$time.interest")
  y <- o$yvar
  if (is.null(y) || ncol(y) < 2L) stop("Missing o$yvar with start/stop")
  start <- as.numeric(y[, 1L])
  stop  <- as.numeric(y[, 2L])
  id <- .rhf_map_record_ids(o)
  leafCount <- as.integer(o$forest$leafCount)
  B <- length(leafCount)
  totalLeaves <- sum(leafCount)
  if (target == "test") {
    ## pull test membership from prediction object
    tCt <- o$ttmbrCaseCt
    tId <- o$ttmbrCaseId
    if (is.null(tCt) || is.null(tId)) {
      stop("target='test' requires o$ttmbrCaseCt and o$ttmbrCaseId (returned by predict()).")
    }
    tCt <- as.integer(tCt)
    tId <- as.integer(tId)
    if (length(tCt) != totalLeaves) stop("length(ttmbrCaseCt) must equal sum(leafCount).")
    if (sum(tCt) != length(tId)) stop("sum(ttmbrCaseCt) must equal length(ttmbrCaseId).")
    ## dummy inbag: all zeros => all trees treated as oob => "use all trees"
    nSubj <- if (!is.null(o$ensemble.id)) length(o$ensemble.id) else max(id, na.rm = TRUE)
    inbag <- matrix(0L, nSubj, B)
    ## empty inbag membership lists
    timbrCaseCt <- integer(totalLeaves)
    timbrCaseId <- integer(0)
    ## put test membership into "oob membership"
    tombrCaseCt <- tCt
    tombrCaseId <- tId
    out <- .Call(
      "medianLogStitchedHazard",
      as.integer(id),
      as.numeric(start),
      as.numeric(stop),
      as.numeric(time),
      inbag,
      leafCount,
      timbrCaseCt,
      timbrCaseId,
      tombrCaseCt,
      tombrCaseId,
      o$forest$t.hazard,
      as.integer(0L),        # targetCode = oob
      as.numeric(eps)
    )
    return(list(loghazard.test = out$loghazard.oob))
  }
  ## existing training path (unchanged)
  req <- c("timbrCaseCt","timbrCaseId","tombrCaseCt","tombrCaseId")
  miss <- req[!req %in% names(o$forest)]
  if (length(miss) > 0L) stop("RHF object forest is missing: ", paste(miss, collapse = ", "))
  inbag <- o$inbag
  if (!is.matrix(inbag)) inbag <- as.matrix(inbag)
  storage.mode(inbag) <- "integer"
  timbrCaseCt <- as.integer(o$forest$timbrCaseCt)
  timbrCaseId <- as.integer(o$forest$timbrCaseId)
  tombrCaseCt <- as.integer(o$forest$tombrCaseCt)
  tombrCaseId <- as.integer(o$forest$tombrCaseId)
  targetCode <- switch(target, oob = 0L, inbag = 1L, both = 2L)
  .Call(
    "medianLogStitchedHazard",
    as.integer(id),
    as.numeric(start),
    as.numeric(stop),
    as.numeric(time),
    inbag,
    leafCount,
    timbrCaseCt,
    timbrCaseId,
    tombrCaseCt,
    tombrCaseId,
    o$forest$t.hazard,
    as.integer(targetCode),
    as.numeric(eps)
  )
}
smoothed.hazard.rhf <- function(
    o,
    method = c("median.loess", "loess"),
    oob = TRUE,
    span = 0.35,
    degree = 1,
    family = NULL,
    eps = 1e-12,
    suppress.warnings = TRUE,
    trace = FALSE )
{
  method <- match.arg(method)
  if (is.null(family)) {
    family <- if (method == "median.loess") "gaussian" else "symmetric"
  }
  family <- match.arg(family, choices = c("gaussian", "symmetric"))
  time <- as.numeric(o$time.interest)
  if (length(time) < 1L) stop("Missing/empty o$time.interest")
  # output object: keep only what downstream tools typically need
  out <- list(
    id = o$id,
    ensemble.id = o$ensemble.id,
    yvar = o$yvar,
    time.interest = time
  )
  ## set the target: oob,inbag, test
  if (inherits(o, "predict")) {
    target <- "test"
  }
  else {
    target <- ifelse(oob, "oob", "inbag")
  }
  do_oob   <- target %in% c("oob", "both")
  do_inbag <- target %in% c("inbag", "both")
  do_test  <- identical(target, "test")  
  if (method == "loess") {
    if (do_oob) {
      H <- .rhf_get_hazard_matrix(o, "oob")
      Z <- log(pmax(H, 0) + eps)
      Zs <- .rhf_smooth_loghazard_matrix(
        Z, time,
        span = span, degree = degree, family = family,
        suppress.warnings = suppress.warnings
      )
      Hs <- pmax(exp(Zs) - eps, 0)
      out$hazard.oob <- Hs
      out$chf.oob <- .rhf_chf_from_hazard(Hs, time)
      if (trace) message("smoothed oob hazard via loess")
    }
    if (do_inbag) {
      H <- .rhf_get_hazard_matrix(o, "inbag")
      Z <- log(pmax(H, 0) + eps)
      Zs <- .rhf_smooth_loghazard_matrix(
        Z, time,
        span = span, degree = degree, family = family,
        suppress.warnings = suppress.warnings
      )
      Hs <- pmax(exp(Zs) - eps, 0)
      out$hazard.inbag <- Hs
      out$chf.inbag <- .rhf_chf_from_hazard(Hs, time)
      if (trace) message("smoothed inbag hazard via loess")
    }
    if (do_test) {
      H <- .rhf_get_hazard_matrix(o, "test")
      Z <- log(pmax(H, 0) + eps)
      Zs <- .rhf_smooth_loghazard_matrix(
        Z, time,
        span = span, degree = degree, family = family,
        suppress.warnings = suppress.warnings
      )
      Hs <- pmax(exp(Zs) - eps, 0)
      out$hazard.test <- Hs
      out$chf.test <- .rhf_chf_from_hazard(Hs, time)
      if (trace) message("smoothed test hazard via loess")
    }
    class(out) <- c("smoothed.hazard.rhf", "rhf")
    return(out)
  }
  # method == 'median.loess'
  if (do_test) {
    if (trace) message("computing TEST median log-hazards across trees via C")
    med <- .rhf_loghazard_median_tree(o, target = "test", eps = eps)  # you add this mode
    Z <- med$loghazard.test
    Zs <- .rhf_smooth_loghazard_matrix(Z, time, span, degree, family,
                                       suppress.warnings = suppress.warnings)
    Hs <- pmax(exp(Zs) - eps, 0)
    out$hazard.test <- Hs
    out$chf.test    <- .rhf_chf_from_hazard(Hs, time)
    if (trace) message("smoothed test hazard via median(loghazard) + loess")
    return(out)  # optional, but keeps logic simple
  }
  else {
    # existing training path unchanged:
    if (trace) message("computing train median log-hazards across trees via C")
      # Compute only the requested target (oob or inbag) for speed
      medTarget <- if (target == "both") "both" else if (do_oob) "oob" else "inbag"
      med <- .rhf_loghazard_median_tree(o, target = medTarget, eps = eps)
      if (do_oob) {
        Z <- med$loghazard.oob
        Zs <- .rhf_smooth_loghazard_matrix(
            Z, time,
            span = span, degree = degree, family = family,
            suppress.warnings = suppress.warnings
        )
        Hs <- pmax(exp(Zs) - eps, 0)
        out$hazard.oob <- Hs
        out$chf.oob <- .rhf_chf_from_hazard(Hs, time)
        if (trace) message("smoothed oob hazard via median(loghazard) + loess")
    }
    if (do_inbag) {
        Z <- med$loghazard.inbag
        Zs <- .rhf_smooth_loghazard_matrix(
            Z, time,
            span = span, degree = degree, family = family,
            suppress.warnings = suppress.warnings
        )
        Hs <- pmax(exp(Zs) - eps, 0)
        out$hazard.inbag <- Hs
        out$chf.inbag <- .rhf_chf_from_hazard(Hs, time)
        if (trace) message("smoothed inbag hazard via median(loghazard) + loess")
    }
    class(out) <- c("smoothed.hazard.rhf", "rhf")
    return(out)
  }
}
smoothed.hazard <- smoothed.hazard.rhf
