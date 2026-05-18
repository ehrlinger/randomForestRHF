########################################################################
## display helpers: robust summaries, formatting, and display capping
########################################################################
.rhf_nonmissing_median <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    NA_real_
  } else {
    stats::median(x)
  }
}
.rhf_nonmissing_sum <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    NA_real_
  } else {
    sum(x)
  }
}
.rhf_nonmissing_quantile <- function(x,
                                     prob = 0.90) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    NA_real_
  } else {
    as.numeric(stats::quantile(x,
                               probs = prob,
                               na.rm = TRUE,
                               names = FALSE))
  }
}
.rhf_row_summary <- function(mat,
                             rank.by = c("q90", "median", "mean", "max", "sum"),
                             abs = FALSE) {
  rank.by <- match.arg(rank.by)
  if (!is.matrix(mat)) {
    mat <- as.matrix(mat)
  }
  if (isTRUE(abs)) {
    mat <- abs(mat)
  }
  switch(
    rank.by,
    q90 = apply(mat, 1L, .rhf_nonmissing_quantile, prob = 0.90),
    median = apply(mat, 1L, .rhf_nonmissing_median),
    mean = apply(mat, 1L, .rhf_nonmissing_mean),
    max = apply(mat, 1L, .rhf_nonmissing_max),
    sum = apply(mat, 1L, .rhf_nonmissing_sum)
  )
}
.rhf_format_importance_numbers <- function(x,
                                           digits = 4L,
                                           scientific.threshold = 1e4) {
  x <- as.numeric(x)
  digits <- max(1L, as.integer(digits)[1L])
  scientific.threshold <- as.numeric(scientific.threshold)[1L]
  out <- rep(NA_character_, length(x))
  out[is.nan(x)] <- "NaN"
  out[is.infinite(x) & x > 0] <- "Inf"
  out[is.infinite(x) & x < 0] <- "-Inf"
  ok <- is.finite(x)
  if (!any(ok)) {
    return(out)
  }
  xx <- signif(x[ok], digits = digits)
  sci <- abs(xx) >= scientific.threshold |
    (abs(xx) > 0 & abs(xx) < 10^(-digits))
  yy <- rep("", length(xx))
  if (any(sci)) {
    yy[sci] <- format(xx[sci], trim = TRUE, scientific = TRUE)
  }
  if (any(!sci)) {
    yy[!sci] <- format(xx[!sci], trim = TRUE, scientific = FALSE)
  }
  out[ok] <- yy
  out
}
.rhf_cap_label <- function(prob) {
  pct <- 100 * as.numeric(prob)[1L]
  out <- formatC(pct, format = "f", digits = 1)
  out <- sub("\\.?0+$", "", out)
  paste0("q", out)
}
.rhf_parse_display_cap <- function(cap,
                                   arg = "display.cap") {
  if (is.null(cap)) {
    return(list(enabled = FALSE,
                prob = NA_real_,
                label = "none"))
  }
  if (is.character(cap)) {
    if (length(cap) != 1L) {
      stop("'", arg, "' must have length 1.")
    }
    cap.chr <- tolower(trimws(cap))
    if (cap.chr %in% c("none", "off", "false", "no")) {
      return(list(enabled = FALSE,
                  prob = NA_real_,
                  label = "none"))
    }
    cap.chr <- sub("^q", "", cap.chr)
    prob <- suppressWarnings(as.numeric(cap.chr))
  }
  else {
    prob <- as.numeric(cap)[1L]
  }
  if (!is.finite(prob)) {
    stop("'", arg, "' must be NULL, 'none', a quantile label like 'q99', or a number in (0, 1].")
  }
  if (prob > 1) {
    prob <- prob / 100
  }
  if (!is.finite(prob) || prob <= 0 || prob > 1) {
    stop("'", arg, "' must be NULL, 'none', a quantile label like 'q99', or a number in (0, 1].")
  }
  if (prob >= 1 - sqrt(.Machine$double.eps)) {
    return(list(enabled = FALSE,
                prob = prob,
                label = .rhf_cap_label(prob)))
  }
  list(enabled = TRUE,
       prob = prob,
       label = .rhf_cap_label(prob))
}
.rhf_cap_values <- function(x,
                            cap = 0.99,
                            symmetric = FALSE,
                            positive.only = FALSE,
                            arg = "display.cap") {
  info <- .rhf_parse_display_cap(cap = cap, arg = arg)
  x.vec <- as.numeric(x)
  out <- x.vec
  base.ok <- is.finite(out)
  if (isTRUE(positive.only)) {
    base.ok <- base.ok & out > 0
  }
  rng <- if (any(is.finite(out))) range(out[is.finite(out)], na.rm = TRUE) else c(NA_real_, NA_real_)
  result <- list(values = out,
                 applied = FALSE,
                 cap = NA_real_,
                 prob = info$prob,
                 label = info$label,
                 symmetric = isTRUE(symmetric),
                 positive.only = isTRUE(positive.only),
                 range = rng)
  if (!isTRUE(info$enabled) || !any(base.ok)) {
    return(result)
  }
  if (isTRUE(symmetric)) {
    cap.value <- as.numeric(stats::quantile(abs(out[base.ok]),
                                            probs = info$prob,
                                            na.rm = TRUE,
                                            names = FALSE))
    if (!is.finite(cap.value) || cap.value <= 0) {
      return(result)
    }
    ok <- is.finite(out)
    out[ok] <- pmax(pmin(out[ok], cap.value), -cap.value)
    rng <- c(-cap.value, cap.value)
  }
  else {
    cap.value <- as.numeric(stats::quantile(out[base.ok],
                                            probs = info$prob,
                                            na.rm = TRUE,
                                            names = FALSE))
    if (!is.finite(cap.value)) {
      return(result)
    }
    ok <- is.finite(out)
    out[ok] <- pmin(out[ok], cap.value)
    rng <- if (any(is.finite(out))) range(out[is.finite(out)], na.rm = TRUE) else c(NA_real_, NA_real_)
  }
  result$values <- out
  result$applied <- any(is.finite(x.vec) & is.finite(out) & (out != x.vec))
  result$cap <- cap.value
  result$range <- rng
  result
}
