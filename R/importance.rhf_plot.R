dotmatrix.importance.rhf <- function(x,
       vars = NULL,
       top_n_union = 15L,
       variable.labels = NULL,
       time.labels = NULL,
       sort_by = c("q90", "sum", "max", "mean", "median", "alphabetical", "cluster", "none"),
       sort_abs = TRUE,
       transform = c("none", "log10"),
       color_by = c("value", "sign", "single", "none"),
       point_color = "steelblue4",
       value_colors = c("grey85", "steelblue4"),
       sign_colors = c("firebrick3", "grey90", "steelblue4"),
       cex.range = c(0.6, 3.2),
       size.cap = 0.99,
       color.cap = 0.99,
       alpha = 0.9,
       show.grid = TRUE,
       grid.col = "grey92",
       legend = TRUE,
       display.note = TRUE,
       xlab = "",
       ylab = "",
       main = "RHF time-localized VarPro importance",
       axis.cex = 0.7,
       var.cex = 0.7,
       time.label.srt = 45,
       save_plot = FALSE,
       out.file = "rhf_time_varpro_dotmatrix.pdf",
       width = 11,
       height = NULL,
       mar = NULL,
       legend.width = 0.7,
       ...) {
  if (!inherits(x, "importance.rhf")) {
    stop("This function only works for objects of class 'importance.rhf'.")
  }
  sort_by <- match.arg(sort_by)
  transform <- match.arg(transform)
  color_by <- match.arg(color_by)
  cex.range <- as.numeric(cex.range)
  if (length(cex.range) != 2L || any(!is.finite(cex.range)) || any(cex.range < 0)) {
    stop("cex.range must be a length-2 nonnegative numeric vector.")
  }
  mat <- x$importance.matrix
  if (!is.matrix(mat) || !length(mat)) {
    stop("No importance values available to plot.")
  }
  mat <- .rhf_select_dotmatrix_variables(mat = mat,
                                         vars = vars,
                                         top_n_union = top_n_union,
                                         sort_abs = sort_abs)
  var.order <- .rhf_order_dotmatrix_variables(mat = mat,
                                              variable.labels = variable.labels,
                                              sort_by = sort_by,
                                              sort_abs = sort_abs)
  var.order <- .rhf_unique_in_order(var.order)
  mat <- mat[var.order, , drop = FALSE]
  var.codes <- rownames(mat)
  time.codes <- colnames(mat)
  time.values <- x$window.info$time
  var.labels <- .rhf_label_lookup(var.codes, variable.labels, infer_prefix = TRUE)
  var.labels <- .rhf_make_unique_labels(var.labels, var.codes)
  if (is.null(time.labels)) {
    x.labels <- .rhf_format_time_labels(time.values)
  }
  else {
    x.labels <- .rhf_label_lookup(time.codes, time.labels, infer_prefix = FALSE)
  }
  if (color_by == "sign") {
    size.metric <- abs(mat)
    color.metric <- mat
    size.title <- if (transform == "log10") "log10(|Importance| + 1)" else "|Importance|"
    color.title <- "Importance"
  }
  else {
    size.metric <- pmax(mat, 0)
    color.metric <- if (color_by == "value") size.metric else NULL
    size.title <- if (transform == "log10") "log10(Importance + 1)" else "Importance"
    color.title <- size.title
  }
  if (transform == "log10") {
    size.metric <- log10(size.metric + 1)
    if (color_by == "value") {
      color.metric <- size.metric
    }
  }
  size.cap.info <- .rhf_cap_values(size.metric,
                                   cap = size.cap,
                                   symmetric = FALSE,
                                   positive.only = TRUE,
                                   arg = "size.cap")
  size.metric.display <- matrix(size.cap.info$values,
                                nrow = nrow(mat),
                                ncol = ncol(mat),
                                dimnames = dimnames(mat))
  if (color_by == "value") {
    color.cap.info <- .rhf_cap_values(color.metric,
                                      cap = color.cap,
                                      symmetric = FALSE,
                                      positive.only = FALSE,
                                      arg = "color.cap")
    color.metric.display <- matrix(color.cap.info$values,
                                   nrow = nrow(mat),
                                   ncol = ncol(mat),
                                   dimnames = dimnames(mat))
  }
  else if (color_by == "sign") {
    color.cap.info <- .rhf_cap_values(color.metric,
                                      cap = color.cap,
                                      symmetric = TRUE,
                                      positive.only = FALSE,
                                      arg = "color.cap")
    color.metric.display <- matrix(color.cap.info$values,
                                   nrow = nrow(mat),
                                   ncol = ncol(mat),
                                   dimnames = dimnames(mat))
  }
  else {
    color.cap.info <- list(applied = FALSE,
                           label = "none",
                           cap = NA_real_,
                           range = c(NA_real_, NA_real_))
    color.metric.display <- color.metric
  }
  draw <- is.finite(size.metric.display) & (size.metric.display > 0)
  cex.mat <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  size.range <- if (any(draw)) range(size.metric.display[draw], na.rm = TRUE) else c(0, 1)
  if (any(draw)) {
    cex.mat[draw] <- .rhf_rescale_from_range(size.metric.display[draw],
                                             from = size.range,
                                             to = cex.range)
  }
  if (color_by == "none") {
    col.mat <- matrix("black", nrow = nrow(mat), ncol = ncol(mat))
  }
  else if (color_by == "single") {
    col.mat <- matrix(point_color, nrow = nrow(mat), ncol = ncol(mat))
  }
  else if (color_by == "value") {
    col.mat <- matrix(.rhf_map_palette(color.metric.display,
                                       value_colors,
                                       symmetric = FALSE),
                      nrow = nrow(mat), ncol = ncol(mat))
  }
  else {
    col.mat <- matrix(.rhf_map_palette(color.metric.display,
                                       sign_colors,
                                       symmetric = TRUE),
                      nrow = nrow(mat), ncol = ncol(mat))
  }
  col.mat[draw] <- grDevices::adjustcolor(col.mat[draw], alpha.f = alpha)
  display.note.text <- NULL
  if (isTRUE(display.note)) {
    if (isTRUE(size.cap.info$applied) &&
        isTRUE(color.cap.info$applied) &&
        identical(size.cap.info$label, color.cap.info$label)) {
      display.note.text <- paste0("plot size/color capped at ", size.cap.info$label)
    }
    else {
      note.parts <- character(0)
      if (isTRUE(size.cap.info$applied)) {
        note.parts <- c(note.parts, paste0("size capped at ", size.cap.info$label))
      }
      if (isTRUE(color.cap.info$applied)) {
        note.parts <- c(note.parts, paste0("color capped at ", color.cap.info$label))
      }
      if (length(note.parts)) {
        display.note.text <- paste0("plot ", paste(note.parts, collapse = "; "))
      }
    }
  }
  if (is.null(height)) {
    height <- max(5.5, 0.22 * nrow(mat) + 1.8)
  }
  if (isTRUE(save_plot)) {
    .rhf_open_plot_device(out.file = out.file, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add = TRUE)
  if (is.null(mar)) {
    mar <- .rhf_dotmatrix_default_mar(var.labels = var.labels,
                                      x.labels = x.labels,
                                      var.cex = var.cex,
                                      axis.cex = axis.cex,
                                      time.label.srt = time.label.srt,
                                      legend = legend)
  }
  if (isTRUE(legend)) {
    graphics::layout(matrix(c(1, 2), nrow = 1L), widths = c(5.6, legend.width))
    on.exit(graphics::layout(1), add = TRUE)
  }
  graphics::par(mar = mar, mgp = c(0.9, 0.12, 0), xpd = NA)
  x.pos <- seq_len(ncol(mat))
  y.pos <- rev(seq_len(nrow(mat)))
  graphics::plot(NA,
                 xlim = c(0.5, ncol(mat) + 0.5),
                 ylim = c(0.5, nrow(mat) + 0.5),
                 xlab = xlab,
                 ylab = ylab,
                 xaxt = "n",
                 yaxt = "n",
                 bty = "n",
                 xaxs = "i",
                 yaxs = "i",
                 main = main)
  if (isTRUE(show.grid)) {
    guide.height <- min(0.16, max(0.07, 0.010 * nrow(mat)))
    graphics::segments(x0 = x.pos,
                       y0 = 0.5,
                       x1 = x.pos,
                       y1 = pmin(nrow(mat) + 0.5, 0.5 + guide.height),
                       col = grid.col,
                       lty = 3)
    graphics::abline(h = seq_len(nrow(mat)), col = grid.col, lty = 3)
    graphics::abline(v = seq_len(ncol(mat)), col = grid.col, lty = 3)
  }
  ## rewrite title over dashed lines
  graphics::title(main = main)
  if (!is.null(display.note.text) && nzchar(display.note.text)) {
    graphics::mtext(display.note.text, side = 3, line = 0.30, adj = 1, cex = 0.75)
  }
  xx <- rep(x.pos, each = nrow(mat))
  yy <- rep(y.pos, times = ncol(mat))
  keep <- as.vector(draw)
  graphics::points(xx[keep],
                   yy[keep],
                   pch = 16,
                   cex = as.vector(cex.mat)[keep],
                   col = as.vector(col.mat)[keep],
                   ...)
  graphics::axis(2, at = y.pos, labels = var.labels, las = 1, tick = FALSE, cex.axis = var.cex)
  .rhf_draw_dotmatrix_xlabels(at = x.pos,
                              labels = x.labels,
                              cex = axis.cex,
                              gap.lines = 0.10,
                              srt = time.label.srt)
  graphics::box()
  if (isTRUE(legend)) {
    graphics::par(mar = c(mar[1L], 0.10, mar[3L], 0.10), xpd = NA)
    size.breaks <- if (any(draw)) {
      .rhf_pretty_breaks(size.metric.display[draw],
                         n = 10L,
                         positive.only = TRUE,
                         symmetric = FALSE)
    }
    else {
      numeric(0)
    }
    color.range <- NULL
    if (color_by == "value" && any(draw)) {
      color.range <- range(color.metric.display[draw], na.rm = TRUE)
    }
    if (color_by == "sign") {
      finite.color <- color.metric.display[is.finite(color.metric.display)]
      if (length(finite.color)) {
        lim <- max(abs(finite.color), na.rm = TRUE)
        color.range <- c(-lim, lim)
      }
    }
    .rhf_draw_dotmatrix_legend(size.breaks = size.breaks,
                               size.range = size.range,
                               size.title = size.title,
                               cex.range = cex.range,
                               alpha = alpha,
                               color_by = color_by,
                               color.range = color.range,
                               color.title = color.title,
                               point_color = point_color,
                               value_colors = value_colors,
                               sign_colors = sign_colors)
  }
  invisible(list(matrix = mat,
                 variables = var.codes,
                 times = time.values,
                 size.metric = size.metric,
                 size.metric.display = size.metric.display,
                 color.metric = color.metric,
                 color.metric.display = color.metric.display,
                 size.cap = size.cap.info,
                 color.cap = color.cap.info))
}
dotmatrix.importance <- dotmatrix.importance.rhf
barplot.importance.rhf <- function(x,
       vars = NULL,
       top_n_union = 15L,
       variable.labels = NULL,
       time.labels = NULL,
       sort_by = c("q90", "sum", "max", "mean", "median", "alphabetical", "cluster", "none"),
       sort_abs = TRUE,
       transform = c("none", "log10"),
       color_by = c("value", "sign", "single", "none"),
       point_color = NULL,
       bar_color = "steelblue4",
       value_colors = c("grey85", "steelblue4"),
       sign_colors = c("firebrick3", "grey90", "steelblue4"),
       cex.range = NULL,
       bar.width = 0.65,
       bar.max.height = NULL,
       size.cap = 0.99,
       color.cap = 0.99,
       alpha = 0.9,
       show.grid = TRUE,
       grid.col = "grey92",
       zero.line = TRUE,
       zero.col = "grey65",
       legend = TRUE,
       display.note = TRUE,
       xlab = "",
       ylab = "",
       main = "RHF time-localized VarPro importance",
       axis.cex = 0.7,
       var.cex = 0.7,
       time.label.srt = 45,
       save_plot = FALSE,
       out.file = "rhf_time_varpro_barplot.pdf",
       width = 11,
       height = NULL,
       mar = NULL,
       legend.width = 0.7,
       border = NA,
       ...) {
  if (!inherits(x, "importance.rhf")) {
    stop("This function only works for objects of class 'importance.rhf'.")
  }
  sort_by <- match.arg(sort_by)
  transform <- match.arg(transform)
  color_by <- match.arg(color_by)
  ## Accept the dot-matrix name as a convenience alias, so code that changes
  ## only type = "barplot" can keep using point_color without failing.
  if (!is.null(point_color)) {
    bar_color <- point_color
  }
  ## cex.range is accepted for dot-matrix API compatibility; bar height is
  ## controlled by bar.max.height instead.
  invisible(cex.range)
  bar.width <- as.numeric(bar.width)[1L]
  if (!is.finite(bar.width) || bar.width <= 0 || bar.width > 1) {
    stop("bar.width must be a numeric value in (0, 1].")
  }
  if (is.null(bar.max.height)) {
    bar.max.height <- if (color_by == "sign") 0.42 else 0.85
  }
  bar.max.height <- as.numeric(bar.max.height)[1L]
  if (!is.finite(bar.max.height) || bar.max.height <= 0) {
    stop("bar.max.height must be a positive numeric value.")
  }
  if (color_by == "sign" && bar.max.height >= 0.5) {
    stop("bar.max.height must be less than 0.5 when color_by = 'sign'.")
  }
  if (color_by != "sign" && bar.max.height >= 1) {
    stop("bar.max.height must be less than 1 when color_by is not 'sign'.")
  }
  mat <- x$importance.matrix
  if (!is.matrix(mat) || !length(mat)) {
    stop("No importance values available to plot.")
  }
  mat <- .rhf_select_dotmatrix_variables(mat = mat,
                                         vars = vars,
                                         top_n_union = top_n_union,
                                         sort_abs = sort_abs)
  var.order <- .rhf_order_dotmatrix_variables(mat = mat,
                                              variable.labels = variable.labels,
                                              sort_by = sort_by,
                                              sort_abs = sort_abs)
  var.order <- .rhf_unique_in_order(var.order)
  mat <- mat[var.order, , drop = FALSE]
  var.codes <- rownames(mat)
  time.codes <- colnames(mat)
  time.values <- x$window.info$time
  var.labels <- .rhf_label_lookup(var.codes, variable.labels, infer_prefix = TRUE)
  var.labels <- .rhf_make_unique_labels(var.labels, var.codes)
  if (is.null(time.labels)) {
    x.labels <- .rhf_format_time_labels(time.values)
  }
  else {
    x.labels <- .rhf_label_lookup(time.codes, time.labels, infer_prefix = FALSE)
  }
  if (color_by == "sign") {
    height.metric <- abs(mat)
    color.metric <- mat
    height.title <- if (transform == "log10") "log10(|Importance| + 1)" else "|Importance|"
    color.title <- "Importance"
  }
  else {
    height.metric <- pmax(mat, 0)
    color.metric <- if (color_by == "value") height.metric else NULL
    height.title <- if (transform == "log10") "log10(Importance + 1)" else "Importance"
    color.title <- height.title
  }
  if (transform == "log10") {
    height.metric <- log10(height.metric + 1)
    if (color_by == "value") {
      color.metric <- height.metric
    }
  }
  height.cap.info <- .rhf_cap_values(height.metric,
                                     cap = size.cap,
                                     symmetric = FALSE,
                                     positive.only = TRUE,
                                     arg = "size.cap")
  height.metric.display <- matrix(height.cap.info$values,
                                  nrow = nrow(mat),
                                  ncol = ncol(mat),
                                  dimnames = dimnames(mat))
  if (color_by == "value") {
    color.cap.info <- .rhf_cap_values(color.metric,
                                      cap = color.cap,
                                      symmetric = FALSE,
                                      positive.only = FALSE,
                                      arg = "color.cap")
    color.metric.display <- matrix(color.cap.info$values,
                                   nrow = nrow(mat),
                                   ncol = ncol(mat),
                                   dimnames = dimnames(mat))
  }
  else if (color_by == "sign") {
    color.cap.info <- .rhf_cap_values(color.metric,
                                      cap = color.cap,
                                      symmetric = TRUE,
                                      positive.only = FALSE,
                                      arg = "color.cap")
    color.metric.display <- matrix(color.cap.info$values,
                                   nrow = nrow(mat),
                                   ncol = ncol(mat),
                                   dimnames = dimnames(mat))
  }
  else {
    color.cap.info <- list(applied = FALSE,
                           label = "none",
                           cap = NA_real_,
                           range = c(NA_real_, NA_real_))
    color.metric.display <- color.metric
  }
  draw <- is.finite(height.metric.display) & (height.metric.display > 0)
  bar.height.mat <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  height.range <- if (any(draw)) range(height.metric.display[draw], na.rm = TRUE) else c(0, 1)
  if (any(draw)) {
    bar.height.mat[draw] <- .rhf_rescale_from_range(height.metric.display[draw],
                                                    from = height.range,
                                                    to = c(0, bar.max.height))
  }
  if (color_by == "none") {
    col.mat <- matrix("black", nrow = nrow(mat), ncol = ncol(mat))
  }
  else if (color_by == "single") {
    col.mat <- matrix(bar_color, nrow = nrow(mat), ncol = ncol(mat))
  }
  else if (color_by == "value") {
    col.mat <- matrix(.rhf_map_palette(color.metric.display,
                                       value_colors,
                                       symmetric = FALSE),
                      nrow = nrow(mat), ncol = ncol(mat))
  }
  else {
    col.mat <- matrix(.rhf_map_palette(color.metric.display,
                                       sign_colors,
                                       symmetric = TRUE),
                      nrow = nrow(mat), ncol = ncol(mat))
  }
  col.mat[draw] <- grDevices::adjustcolor(col.mat[draw], alpha.f = alpha)
  display.note.text <- NULL
  if (isTRUE(display.note)) {
    if (isTRUE(height.cap.info$applied) &&
        isTRUE(color.cap.info$applied) &&
        identical(height.cap.info$label, color.cap.info$label)) {
      display.note.text <- paste0("plot height/color capped at ", height.cap.info$label)
    }
    else {
      note.parts <- character(0)
      if (isTRUE(height.cap.info$applied)) {
        note.parts <- c(note.parts, paste0("bar height capped at ", height.cap.info$label))
      }
      if (isTRUE(color.cap.info$applied)) {
        note.parts <- c(note.parts, paste0("color capped at ", color.cap.info$label))
      }
      if (length(note.parts)) {
        display.note.text <- paste0("plot ", paste(note.parts, collapse = "; "))
      }
    }
  }
  if (is.null(height)) {
    height <- max(5.5, 0.22 * nrow(mat) + 1.8)
  }
  if (isTRUE(save_plot)) {
    .rhf_open_plot_device(out.file = out.file, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add = TRUE)
  if (is.null(mar)) {
    mar <- .rhf_dotmatrix_default_mar(var.labels = var.labels,
                                      x.labels = x.labels,
                                      var.cex = var.cex,
                                      axis.cex = axis.cex,
                                      time.label.srt = time.label.srt,
                                      legend = legend)
  }
  if (isTRUE(legend)) {
    graphics::layout(matrix(c(1, 2), nrow = 1L), widths = c(5.6, legend.width))
    on.exit(graphics::layout(1), add = TRUE)
  }
  graphics::par(mar = mar, mgp = c(0.9, 0.12, 0), xpd = NA)
  x.pos <- seq_len(ncol(mat))
  y.pos <- rev(seq_len(nrow(mat)))
  graphics::plot(NA,
                 xlim = c(0.5, ncol(mat) + 0.5),
                 ylim = c(0.5, nrow(mat) + 0.5),
                 xlab = xlab,
                 ylab = ylab,
                 xaxt = "n",
                 yaxt = "n",
                 bty = "n",
                 xaxs = "i",
                 yaxs = "i",
                 main = main)
  if (isTRUE(show.grid)) {
    guide.height <- min(0.16, max(0.07, 0.010 * nrow(mat)))
    graphics::segments(x0 = x.pos,
                       y0 = 0.5,
                       x1 = x.pos,
                       y1 = pmin(nrow(mat) + 0.5, 0.5 + guide.height),
                       col = grid.col,
                       lty = 3)
    graphics::abline(h = seq_len(nrow(mat)), col = grid.col, lty = 3)
    graphics::abline(v = seq_len(ncol(mat)), col = grid.col, lty = 3)
  }
  if (color_by == "sign" && isTRUE(zero.line)) {
    graphics::abline(h = y.pos, col = zero.col, lty = 1)
  }
  ## rewrite title over guide lines
  graphics::title(main = main)
  if (!is.null(display.note.text) && nzchar(display.note.text)) {
    graphics::mtext(display.note.text, side = 3, line = 0.30, adj = 1, cex = 0.75)
  }
  xx <- rep(x.pos, each = nrow(mat))
  yy <- rep(y.pos, times = ncol(mat))
  hh <- as.vector(bar.height.mat)
  keep <- as.vector(draw)
  xleft <- xx - bar.width / 2
  xright <- xx + bar.width / 2
  if (color_by == "sign") {
    dir <- sign(as.vector(color.metric.display))
    dir[!is.finite(dir)] <- 1
    ybottom <- ifelse(dir >= 0, yy, yy - hh)
    ytop <- ifelse(dir >= 0, yy + hh, yy)
  }
  else {
    ybottom <- yy - bar.max.height / 2
    ytop <- ybottom + hh
  }
  graphics::rect(xleft[keep],
                 ybottom[keep],
                 xright[keep],
                 ytop[keep],
                 col = as.vector(col.mat)[keep],
                 border = border,
                 ...)
  graphics::axis(2, at = y.pos, labels = var.labels, las = 1, tick = FALSE, cex.axis = var.cex)
  .rhf_draw_dotmatrix_xlabels(at = x.pos,
                              labels = x.labels,
                              cex = axis.cex,
                              gap.lines = 0.10,
                              srt = time.label.srt)
  graphics::box()
  if (isTRUE(legend)) {
    graphics::par(mar = c(mar[1L], 0.10, mar[3L], 0.10), xpd = NA)
    height.breaks <- if (any(draw)) {
      .rhf_pretty_breaks(height.metric.display[draw],
                         n = 10L,
                         positive.only = TRUE,
                         symmetric = FALSE)
    }
    else {
      numeric(0)
    }
    color.range <- NULL
    if (color_by == "value" && any(draw)) {
      color.range <- range(color.metric.display[draw], na.rm = TRUE)
    }
    if (color_by == "sign") {
      finite.color <- color.metric.display[is.finite(color.metric.display)]
      if (length(finite.color)) {
        lim <- max(abs(finite.color), na.rm = TRUE)
        color.range <- c(-lim, lim)
      }
    }
    legend.bar.color <- if (color_by == "none") "black" else bar_color
    .rhf_draw_barmatrix_legend(height.breaks = height.breaks,
                               height.range = height.range,
                               height.title = height.title,
                               bar.max.height = bar.max.height,
                               alpha = alpha,
                               color_by = color_by,
                               color.range = color.range,
                               color.title = color.title,
                               bar_color = legend.bar.color,
                               value_colors = value_colors,
                               sign_colors = sign_colors)
  }
  invisible(list(matrix = mat,
                 variables = var.codes,
                 times = time.values,
                 height.metric = height.metric,
                 height.metric.display = height.metric.display,
                 size.metric = height.metric,
                 size.metric.display = height.metric.display,
                 color.metric = color.metric,
                 color.metric.display = color.metric.display,
                 bar.height = bar.height.mat,
                 height.cap = height.cap.info,
                 size.cap = height.cap.info,
                 color.cap = color.cap.info))
}
barplot.importance <- barplot.importance.rhf
########################################################################
## plotting method
########################################################################
plot.importance.rhf <- function(x,
                 type = c("dotmatrix", "barplot", "lines"),
                 vars = NULL,
                 top = 10L,
                 rank.by = c("q90", "median", "mean", "max"),
                 curve = c("step", "line", "lowess"),
                 smooth.f = 2/3,
                 display.cap = 0.99,
                 display.note = TRUE,
                 xlab = NULL,
                 ylab = NULL,
                 lty = 1,
                 lwd = 2.0,
                 ...) {
  type <- match.arg(type)
  if (type == "dotmatrix") {
    if (is.null(xlab)) {
      xlab <- ""
    }
    if (is.null(ylab)) {
      ylab <- ""
    }
    return(dotmatrix.importance.rhf(x = x, vars = vars, xlab = xlab, ylab = ylab, ...))
  }
  if (type == "barplot") {
    if (is.null(xlab)) {
      xlab <- ""
    }
    if (is.null(ylab)) {
      ylab <- ""
    }
    return(barplot.importance.rhf(x = x, vars = vars, xlab = xlab, ylab = ylab, ...))
  }
  rank.by <- match.arg(rank.by)
  curve <- match.arg(curve)
  top <- as.integer(top)[1L]
  if (!is.finite(top) || top < 1L) {
    stop("'top' must be a positive integer.")
  }
  mat <- x$importance.matrix
  if (!is.matrix(mat) || !length(mat)) {
    stop("No importance values available to plot.")
  }
  if (is.null(vars)) {
    score <- .rhf_row_summary(mat, rank.by = rank.by)
    ord <- order(score, decreasing = TRUE, na.last = TRUE)
    vars <- rownames(mat)[head(ord, top)]
  }
  vars <- intersect(as.character(vars), rownames(mat))
  if (!length(vars)) {
    stop("No requested variables found in the importance object.")
  }
  zz.raw <- mat[vars, , drop = FALSE]
  xx <- x$window.info$time
  if (is.null(xlab)) {
    xlab <- "Time"
  }
  if (is.null(ylab)) {
    ylab <- "Localized Importance"
  }
  cap.info <- .rhf_cap_values(zz.raw,
                              cap = display.cap,
                              symmetric = any(zz.raw[is.finite(zz.raw)] < 0),
                              positive.only = FALSE,
                              arg = "display.cap")
  zz <- matrix(cap.info$values,
               nrow = nrow(zz.raw),
               ncol = ncol(zz.raw),
               dimnames = dimnames(zz.raw))
  ylim <- range(zz, na.rm = TRUE)
  if (!all(is.finite(ylim))) {
    ylim <- c(0, 1)
  }
  cols <- seq_len(nrow(zz))
  lty <- rep_len(lty, nrow(zz))
  lwd <- rep_len(lwd, nrow(zz))
  if (curve %in% c("step", "line")) {
    graphics::matplot(xx,
                      t(zz),
                      type = if (curve == "step") "s" else "l",
                      lty = lty,
                      lwd = lwd,
                      col = cols,
                      xlab = xlab,
                      ylab = ylab,
                      ylim = ylim,
                      ...)
  }
  else {
    graphics::plot(NA,
                   xlim = range(xx, na.rm = TRUE),
                   ylim = ylim,
                   xlab = xlab,
                   ylab = ylab,
                   type = "n",
                   ...)
    for (i in seq_len(nrow(zz))) {
      ok <- is.finite(xx) & is.finite(zz[i, ])
      if (sum(ok) < 2L) {
        next
      }
      sm <- stats::lowess(xx[ok], zz[i, ok], f = smooth.f, iter = 0)
      graphics::lines(sm$x, sm$y, col = cols[i], lty = lty[i], lwd = lwd[i])
    }
  }
  if (isTRUE(display.note) && isTRUE(cap.info$applied)) {
    graphics::mtext(paste0("plot capped at ", cap.info$label),
                    side = 3,
                    line = 0.30,
                    adj = 1,
                    cex = 0.75)
  }
  graphics::legend("topright",
                   legend = vars,
                   col = cols,
                   lty = lty,
                   lwd = lwd,
                   bty = "n")
  invisible(zz)
}
