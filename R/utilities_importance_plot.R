########################################################################
## plot helpers
########################################################################
.rhf_select_dotmatrix_variables <- function(mat,
                                            vars = NULL,
                                            top_n_union = 15L,
                                            sort_abs = TRUE) {
  if (!is.null(vars)) {
    vars <- intersect(as.character(vars), rownames(mat))
    if (!length(vars)) {
      stop("No requested variables found in the importance matrix.")
    }
    return(mat[vars, , drop = FALSE])
  }
  top_n_union <- as.integer(top_n_union)[1L]
  if (!is.finite(top_n_union) || top_n_union < 1L) {
    stop("top_n_union must be a positive integer.")
  }
  top.vars <- unique(unlist(lapply(seq_len(ncol(mat)), function(j) {
    v <- mat[, j]
    ord <- if (isTRUE(sort_abs)) {
      order(abs(v), decreasing = TRUE, na.last = TRUE)
    }
    else {
      order(v, decreasing = TRUE, na.last = TRUE)
    }
    rownames(mat)[head(ord, min(top_n_union, length(ord)))]
  })))
  if (!length(top.vars)) {
    stop("No variables available for the dot-matrix plot.")
  }
  mat[top.vars, , drop = FALSE]
}
.rhf_order_dotmatrix_variables <- function(mat,
                                           variable.labels = NULL,
                                           sort_by = c("q90", "sum", "max", "mean", "median", "alphabetical", "cluster", "none"),
                                           sort_abs = TRUE) {
  sort_by <- match.arg(sort_by)
  if (sort_by == "none") {
    return(rownames(mat))
  }
  if (sort_by %in% c("q90", "sum", "max", "mean", "median")) {
    score <- .rhf_row_summary(mat,
                              rank.by = sort_by,
                              abs = sort_abs)
    return(names(sort(score, decreasing = TRUE)))
  }
  if (sort_by == "alphabetical") {
    lab <- .rhf_label_lookup(rownames(mat), variable.labels, infer_prefix = TRUE)
    lab <- .rhf_make_unique_labels(lab, rownames(mat))
    return(rownames(mat)[order(lab)])
  }
  ## cluster
  mat.z <- t(scale(t(mat)))
  mat.z[!is.finite(mat.z)] <- 0
  hc <- stats::hclust(stats::dist(mat.z), method = "ward.D2")
  rownames(mat.z)[hc$order]
}
.rhf_rescale_from_range <- function(x,
                                    from,
                                    to = c(0.5, 3.0)) {
  x <- as.numeric(x)
  from <- as.numeric(from)
  out <- rep(0, length(x))
  ok <- is.finite(x)
  if (!any(ok)) {
    return(out)
  }
  if (length(from) != 2L || any(!is.finite(from))) {
    return(out)
  }
  if (from[1L] == from[2L]) {
    out[ok] <- mean(to)
    return(out)
  }
  out[ok] <- to[1L] + (x[ok] - from[1L]) / (from[2L] - from[1L]) * (to[2L] - to[1L])
  out
}
.rhf_pretty_breaks <- function(x,
                               n = 5L,
                               positive.only = FALSE,
                               symmetric = FALSE) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (positive.only) {
    x <- x[x > 0]
  }
  if (!length(x)) {
    return(numeric(0))
  }
  n <- max(2L, as.integer(n)[1L])
  if (symmetric) {
    lim <- max(abs(x), na.rm = TRUE)
    if (!is.finite(lim) || lim <= 0) {
      return(0)
    }
    br <- pretty(c(-lim, lim), n = n)
    br <- br[br >= -lim & br <= lim]
    br <- unique(br[is.finite(br)])
    if (!0 %in% br) {
      br <- sort(unique(c(br, 0)))
    }
    return(br)
  }
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1L]) || !is.finite(rng[2L])) {
    return(numeric(0))
  }
  if (rng[1L] == rng[2L]) {
    return(rng[1L])
  }
  br <- pretty(rng, n = n)
  br <- br[br >= rng[1L] & br <= rng[2L]]
  if (positive.only) {
    br <- br[br > 0]
  }
  br <- unique(br[is.finite(br)])
  if (!length(br)) {
    probs <- seq(0, 1, length.out = n)
    br <- as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
    if (positive.only) {
      br <- br[br > 0]
    }
    br <- unique(br[is.finite(br)])
  }
  br
}
.rhf_format_legend_values <- function(x,
                                      digits = 3L) {
  format(signif(as.numeric(x), digits = digits), trim = TRUE, scientific = FALSE)
}
.rhf_draw_dotmatrix_xlabels <- function(at,
                                        labels,
                                        cex = 0.9,
                                        gap.lines = 0.10,
                                        srt = 45) {
  if (!length(at) || !length(labels)) {
    return(invisible(TRUE))
  }
  usr <- graphics::par("usr")
  csi <- graphics::par("csi")
  din <- graphics::par("din")
  if (!is.finite(csi) || csi <= 0) {
    csi <- 0.2
  }
  if (!is.finite(din[2L]) || din[2L] <= 0) {
    din[2L] <- 7
  }
  ## Position labels using a fixed physical gap beneath the x-axis rather than
  ## a gap that scales with the number of variables in the plot.
  y.axis.ndc <- graphics::grconvertY(usr[3L], from = "user", to = "ndc")
  gap.in <- max(0, gap.lines) * csi * cex
  y.text.ndc <- y.axis.ndc - gap.in / din[2L]
  y.user <- graphics::grconvertY(y.text.ndc, from = "ndc", to = "user")
  graphics::text(at,
                 rep(y.user, length(at)),
                 labels = labels,
                 srt = srt,
                 adj = c(1, 1),
                 xpd = NA,
                 cex = cex)
  invisible(TRUE)
}
.rhf_dotmatrix_default_mar <- function(var.labels,
                                       x.labels,
                                       var.cex = 0.9,
                                       axis.cex = 0.9,
                                       time.label.srt = 45,
                                       legend = TRUE) {
  csi <- graphics::par("csi")
  if (!is.finite(csi) || csi <= 0) {
    csi <- 0.2
  }
  left.in <- if (length(var.labels)) {
    max(graphics::strwidth(var.labels, units = "inches", cex = var.cex), na.rm = TRUE)
  } else {
    0
  }
  lab.w.in <- if (length(x.labels)) {
    max(graphics::strwidth(x.labels, units = "inches", cex = axis.cex), na.rm = TRUE)
  } else {
    0
  }
  lab.h.in <- if (length(x.labels)) {
    max(graphics::strheight(x.labels, units = "inches", cex = axis.cex), na.rm = TRUE)
  } else {
    0
  }
  theta <- abs(as.numeric(time.label.srt)[1L]) * pi / 180
  rot.ext.in <- lab.w.in * sin(theta) + lab.h.in * cos(theta)
  ## Give the variable labels some extra room beyond their measured width so
  ## long names do not crowd the plotting region.
  left.mar <- 1.25 + left.in / csi + 1.15
  left.mar <- min(30, max(10.0, left.mar))
  ## Keep enough room for rotated time labels so they are not clipped, while
  ## controlling label proximity to the axis separately at draw time.
  bottom.mar <- 0.45 + rot.ext.in / csi + 0.65
  bottom.mar <- min(6.5, max(2.8, bottom.mar))
  top.mar <- 2.8
  right.mar <- if (isTRUE(legend)) 0.5 else 0.4
  c(bottom.mar, left.mar, top.mar, right.mar)
}
.rhf_draw_dotmatrix_legend <- function(size.breaks,
                                       size.range,
                                       size.title,
                                       cex.range,
                                       alpha,
                                       color_by,
                                       color.range = NULL,
                                       color.title = NULL,
                                       point_color = "steelblue4",
                                       value_colors = c("grey85", "steelblue4"),
                                       sign_colors = c("firebrick3", "grey90", "steelblue4"),
                                       cex.text = 0.85,
                                       cex.title = 0.9) {
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 0.70), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  has.color <- color_by %in% c("value", "sign") &&
    length(color.range) == 2L && all(is.finite(color.range))
  size.breaks <- as.numeric(size.breaks)
  size.breaks <- sort(unique(size.breaks[is.finite(size.breaks)]), decreasing = TRUE)
  ## Compact legend geometry with titles placed close to the legend content.
  x.title <- 0.06
  x.dot <- 0.22
  x.size.lab <- 0.40
  if (length(size.breaks)) {
    size.y.top <- if (has.color) 0.79 else 0.74
    size.y.bottom <- if (has.color) 0.64 else 0.28
    size.y.seq <- if (length(size.breaks) == 1L) {
      mean(c(size.y.top, size.y.bottom))
    } else {
      seq(size.y.top, size.y.bottom, length.out = length(size.breaks))
    }
    ## adjust top panel
    size.y.seq <-  1.1 * size.y.seq
    size.y.top <- 1.1 * (size.y.top + 0.040)
    graphics::text(x.title,
                   size.y.top,
                   labels = size.title,
                   adj = c(0, 0.5),
                   font = 2,
                   cex = cex.title)
    cex.val <- .rhf_rescale_from_range(size.breaks,
                                       from = size.range,
                                       to = cex.range)
    graphics::points(rep(x.dot, length(size.breaks)),
                     size.y.seq,
                     pch = 16,
                     cex = cex.val,
                     col = grDevices::adjustcolor(point_color, alpha.f = alpha))
    graphics::text(rep(x.size.lab, length(size.breaks)),
                   size.y.seq,
                   labels = .rhf_format_legend_values(size.breaks),
                   adj = c(0, 0.5),
                   cex = cex.text)
  }
  else {
    graphics::text(x.title,
                   0.72,
                   labels = size.title,
                   adj = c(0, 0.5),
                   font = 2,
                   cex = cex.title)
    graphics::text(x.title,
                   0.64,
                   labels = "No positive values",
                   adj = c(0, 0.5),
                   cex = cex.text)
  }
  if (has.color) {
    if (is.null(color.title) || !nzchar(color.title)) {
      color.title <- "Importance"
    }
    x0 <- 0.18
    x1 <- 0.34
    y0 <- 0.16 + .275
    y1 <- 0.39 + .275
    #graphics::text(x.title,
    #               y1 + 0.035,
    #               labels = color.title,
    #               adj = c(0, 0.5),
    #               font = 2,
    #               cex = cex.title)
    n.bar <- 64L
    y.seq <- seq(y0, y1, length.out = n.bar + 1L)
    pal <- if (color_by == "value") {
      grDevices::colorRampPalette(value_colors)(n.bar)
    } else {
      grDevices::colorRampPalette(sign_colors)(n.bar)
    }
    for (i in seq_len(n.bar)) {
      graphics::rect(x0, y.seq[i], x1, y.seq[i + 1L],
                     col = pal[i], border = pal[i])
    }
    graphics::rect(x0, y0, x1, y1, border = "grey40")
    br <- if (color_by == "sign") {
      .rhf_pretty_breaks(color.range, n = 10L, symmetric = TRUE)
    } else {
      .rhf_pretty_breaks(color.range, n = 10L,
                         positive.only = FALSE,
                         symmetric = FALSE)
    }
    if (!length(br)) {
      br <- color.range
    }
    if (color_by == "sign") {
      lim <- max(abs(color.range), na.rm = TRUE)
      ypos <- if (is.finite(lim) && lim > 0) {
        y0 + (br + lim) / (2 * lim) * (y1 - y0)
      } else {
        rep(mean(c(y0, y1)), length(br))
      }
    } else {
      rng <- range(color.range, na.rm = TRUE)
      ypos <- if (is.finite(rng[1L]) && is.finite(rng[2L]) && rng[1L] != rng[2L]) {
        y0 + (br - rng[1L]) / diff(rng) * (y1 - y0)
      } else {
        rep(mean(c(y0, y1)), length(br))
      }
    }
    graphics::segments(x1, ypos, x1 + 0.035, ypos, col = "grey35")
    graphics::text(x1 + 0.055,
                   ypos,
                   labels = .rhf_format_legend_values(br),
                   adj = c(0, 0.5),
                   cex = cex.text)
  }
  invisible(TRUE)
}
.rhf_map_palette <- function(x, colors, symmetric = FALSE) {
  x <- as.numeric(x)
  pal <- grDevices::colorRampPalette(colors)(64L)
  idx <- rep(1L, length(x))
  ok <- is.finite(x)
  if (!any(ok)) {
    return(pal[idx])
  }
  if (symmetric) {
    lim <- max(abs(x[ok]), na.rm = TRUE)
    if (!is.finite(lim) || lim <= 0) {
      idx[ok] <- ceiling(length(pal) / 2)
    } else {
      idx[ok] <- 1L + floor((x[ok] + lim) / (2 * lim) * (length(pal) - 1L))
    }
  } else {
    rng <- range(x[ok], na.rm = TRUE)
    if (!is.finite(rng[1L]) || !is.finite(rng[2L]) || rng[1L] == rng[2L]) {
      idx[ok] <- length(pal)
    } else {
      idx[ok] <- 1L + floor((x[ok] - rng[1L]) / (rng[2L] - rng[1L]) * (length(pal) - 1L))
    }
  }
  idx <- pmax(1L, pmin(length(pal), idx))
  pal[idx]
}
.rhf_open_plot_device <- function(out.file, width, height) {
  ext <- tolower(sub("^.*\\.", "", out.file))
  if (!nzchar(ext) || identical(ext, out.file) || ext == "pdf") {
    grDevices::pdf(file = out.file, width = width, height = height)
  }
  else if (ext == "png") {
    grDevices::png(filename = out.file, width = width, height = height,
                   units = "in", res = 300)
  }
  else if (ext %in% c("jpg", "jpeg")) {
    grDevices::jpeg(filename = out.file, width = width, height = height,
                    units = "in", res = 300, quality = 100)
  }
  else if (ext %in% c("tif", "tiff")) {
    grDevices::tiff(filename = out.file, width = width, height = height,
                    units = "in", res = 300, compression = "lzw")
  }
  else {
    stop("Unsupported plot file extension in 'out.file': ", out.file)
  }
  invisible(TRUE)
}
########################################################################
## label helpers for plotting
########################################################################
.rhf_unique_in_order <- function(x) {
  x <- as.character(x)
  x[!duplicated(x)]
}
.rhf_clean_dummy_suffix <- function(s) {
  s <- as.character(s)
  if (!nzchar(s)) {
    return(s)
  }
  s <- gsub("\\.+", " ", s)
  s <- gsub("_+", " ", s)
  s <- trimws(gsub("\\s+", " ", s))
  if (nzchar(s) && identical(s, toupper(s)) && nchar(s) >= 4) {
    s <- tools::toTitleCase(tolower(s))
  }
  s <- sub(" ([A-Z]{2,6})$", " (\\1)", s)
  s
}
.rhf_infer_prefix_label <- function(code, map) {
  code <- as.character(code)
  if (is.null(map) || is.null(names(map))) {
    return(NA_character_)
  }
  keys <- names(map)
  if (!length(keys)) {
    return(NA_character_)
  }
  hit <- keys[startsWith(code, keys)]
  if (!length(hit)) {
    return(NA_character_)
  }
  hit <- hit[order(nchar(hit), decreasing = TRUE)]
  for (k in hit) {
    suf <- substr(code, nchar(k) + 1L, nchar(code))
    if (!nzchar(suf)) {
      next
    }
    first.chr <- substr(suf, 1L, 1L)
    if (first.chr %in% c("_")) {
      next
    }
    if (!grepl("^[A-Za-z0-9]", suf)) {
      next
    }
    base.lab <- as.character(map[[k]])
    if (!nzchar(base.lab) || is.na(base.lab)) {
      next
    }
    suf.clean <- .rhf_clean_dummy_suffix(suf)
    if (!nzchar(suf.clean)) {
      return(base.lab)
    }
    return(paste0(base.lab, ": ", suf.clean))
  }
  NA_character_
}
.rhf_label_lookup <- function(x, map, infer_prefix = TRUE) {
  x <- as.character(x)
  if (is.null(map)) {
    return(x)
  }
  if (is.data.frame(map)) {
    if (ncol(map) < 2L) {
      stop("label map data.frame must have at least 2 columns")
    }
    nms <- names(map)
    if (all(c("variable", "label") %in% nms)) {
      map <- stats::setNames(as.character(map$label), as.character(map$variable))
    }
    else if (all(c("outcome", "label") %in% nms)) {
      map <- stats::setNames(as.character(map$label), as.character(map$outcome))
    }
    else {
      map <- stats::setNames(as.character(map[[2L]]), as.character(map[[1L]]))
    }
  }
  if (is.null(names(map))) {
    return(x)
  }
  if (anyDuplicated(names(map))) {
    map <- map[!duplicated(names(map), fromLast = TRUE)]
  }
  idx <- match(x, names(map))
  out <- as.character(map[idx])
  miss <- is.na(out) | out == ""
  if (any(miss) && isTRUE(infer_prefix)) {
    out2 <- vapply(x[miss], .rhf_infer_prefix_label, character(1), map = map)
    fill.ok <- !is.na(out2) & nzchar(out2)
    out[which(miss)[fill.ok]] <- out2[fill.ok]
    miss <- is.na(out) | out == ""
  }
  out[miss] <- x[miss]
  out
}
.rhf_make_unique_labels <- function(labels, codes) {
  dup <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  labels[dup] <- paste0(labels[dup], " [", codes[dup], "]")
  labels
}
