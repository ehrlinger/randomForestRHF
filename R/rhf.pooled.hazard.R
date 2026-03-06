# Pooled (stitched) hazard estimators for Random Hazard Forests.
#
# This wrapper calls the native routine `weightedStitchedHazard`.
#
# Supported modes:
#   - weights = "unif" / "uniform": default RHF stitched hazard (uniform tree average)
#   - weights = "eb.hazard"      : EB smoothing of terminal-node hazards within each
#                                  tree, followed by uniform pooling across trees.
#
# Notes:
#   * This wrapper computes CHF *after* computing the pooled hazard:
#       chf(t_r) = sum_{k<=r} haz(t_k) * (t_k - t_{k-1}).
.rhf_get_chf_from_hazard <- function(haz, time.interest) {
  if (!is.matrix(haz)) haz <- as.matrix(haz)
  time.interest <- as.numeric(time.interest)
  q <- length(time.interest)
  if (ncol(haz) != q) stop("hazard ncol must match length(time.interest)")
  dt <- c(time.interest[1], diff(time.interest))
  hd <- sweep(haz, 2, dt, `*`)
  t(apply(hd, 1, cumsum))
}
rhf.pooled.hazard <- function(o,
                             weights = c("eb.hazard", "unif"),
                             beta.scale = 0) {
  if (is.null(o$forest) || is.null(o$forest$t.hazard)) {
    stop("RHF object must contain forest$t.hazard. Fit with forest=TRUE and keep.forest=TRUE.")
  }
  if (is.null(o$forest$timbrCaseCt) || is.null(o$forest$timbrCaseId) ||
      is.null(o$forest$tombrCaseCt) || is.null(o$forest$tombrCaseId)) {
    stop("RHF object must contain timbr/tombr terminal node membership (timbrCaseCt/Id, tombrCaseCt/Id).")
  }
  # Working grid used by RHF for terminal hazards.
  time <- as.numeric(o$time.interest)
  # Pseudo-individual representation.
  start <- as.numeric(o$yvar[, 1])
  stop  <- as.numeric(o$yvar[, 2])
  event <- as.integer(o$yvar[, 3])
  # inbag: n x B integer matrix.
  inbag <- o$inbag
  if (!is.matrix(inbag)) inbag <- as.matrix(inbag)
  storage.mode(inbag) <- "integer"
  # Map record-level ids onto 1..n (ensemble order)
  id_raw <- o$id
  if (is.null(o$ensemble.id)) {
    # Fallback: assume ids already 1..n
    id <- as.integer(id_raw)
  } else {
    id <- match(id_raw, o$ensemble.id)
    if (anyNA(id)) stop("Unable to map o$id to o$ensemble.id")
    id <- as.integer(id)
  }
  leafCount   <- as.integer(o$forest$leafCount)
  timbrCaseCt <- as.integer(o$forest$timbrCaseCt)
  timbrCaseId <- as.integer(o$forest$timbrCaseId)
  tombrCaseCt <- as.integer(o$forest$tombrCaseCt)
  tombrCaseId <- as.integer(o$forest$tombrCaseId)
  # ---- mode selection ----
  w <- tolower(as.character(weights[1]))
  if (is.na(w) || !nzchar(w)) stop("weights must be a non-empty string")
  if (w == "uniform") w <- "unif"
  if (w == "ebhazard" || w == "eb_hazard" || w == "eb-hazard") w <- "eb.hazard"
  weightMode <- switch(w,
    "unif"      = 0L,
    "eb.hazard" = 1L,
    stop("unknown weights mode: ", w, ". Supported: 'unif' and 'eb.hazard'.")
  )
  beta.scale <- as.numeric(beta.scale)
  if (length(beta.scale) != 1 || is.na(beta.scale) || !is.finite(beta.scale) || beta.scale < 0) {
    stop("beta.scale must be a single finite number >= 0")
  }
  out <- .Call(
    "weightedStitchedHazard",
    id,
    start,
    stop,
    event,
    time,
    inbag,
    leafCount,
    timbrCaseCt,
    timbrCaseId,
    tombrCaseCt,
    tombrCaseId,
    o$forest$t.hazard,
    as.integer(weightMode),
    as.numeric(beta.scale)
  )
  # Native returns q x n matrices; transpose to n x q.
  out$hazard.inbag <- t(out$hazard.inbag)
  out$hazard.oob   <- t(out$hazard.oob)
  out$time.interest <- time
  out$chf.inbag <- .rhf_get_chf_from_hazard(out$hazard.inbag, time)
  out$chf.oob   <- .rhf_get_chf_from_hazard(out$hazard.oob,   time)
  out$id <- o$id
  out$ensemble.id <- o$ensemble.id
  out$yvar <- o$yvar
  out$weights <- w
  out$beta.scale <- beta.scale
  out
}
