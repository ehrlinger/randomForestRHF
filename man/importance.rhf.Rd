\name{importance.rhf}
\alias{importance.rhf}
\alias{varpro.cache.rhf}
\alias{varpro.cache}
\alias{print.importance.rhf}
\alias{as.data.frame.importance.rhf}
\alias{plot.importance.rhf}
\alias{dotmatrix.importance.rhf}
\alias{dotmatrix.importance}
\alias{barplot.importance.rhf}
\alias{barplot.importance}
\title{Time-localized VarPro importance for random hazard forests}

\description{
Computes time-localized variable importance for a random hazard forest
(RHF) by using VarPro (variable priority) importance and restricting
rule and near-miss memberships to pseudo-individuals whose start-stop
intervals overlap selected windows from the master time grid
\code{time.interest}. The working response used for VarPro importance is
now taken directly from the RHF object as the logarithm of the upstream
integrated hazard exposure, giving a fast localized view of how variable
importance evolves over time.
}

\usage{
importance.rhf(o,
 cache = NULL,
 time.index = NULL,
 trim = 0.1,
 sort = TRUE,
 max.rules.tree,
 max.tree,
 eps = 1e-6,
 y.external = NULL,
 verbose = FALSE,
 ...)

varpro.cache.rhf(o,
 max.rules.tree = 150L,
 max.tree = 150L,
 y.external = NULL,
 eps = 1e-6,
 verbose = FALSE)

\method{print}{importance.rhf}(x,
 top = 10L,
 rank.by = c("q90", "median", "mean", "max"),
 digits = 4L,
 scientific.threshold = 1e4,
 ...)

\method{as.data.frame}{importance.rhf}(x,
  row.names = NULL,
  optional = FALSE,
  format = c("long", "variable_by_time", "time_by_variable"),
  ...)

dotmatrix.importance.rhf(x,
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
 ...)

barplot.importance.rhf(x,
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
 ...)

\method{plot}{importance.rhf}(x,
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
 lwd = 2,
 ...)
}

\arguments{

  \item{o}{A RHF object with class \code{"rhf"}.}

  \item{cache}{Optional cache object returned by \code{varpro.cache()}.
  If \code{NULL}, the cache is built internally. Supplying a cache is
  useful when repeated calls are made.}

  \item{time.index}{Optional vector identifying which windows of the time
  grid \code{o$time.interest} are to be analyzed. This may be an integer index
  vector or a logical vector of length \code{length(o$time.interest)}. If
  omitted, all windows are used.}

  \item{trim}{Tuning parameter passed to the underlying VarPro
  importance workhorse. \code{trim} controls winsorized aggregation
  across trees.}

  \item{sort}{Logical. If \code{TRUE}, variables are ordered within each
  window in decreasing importance before the long-format output is assembled.}

  \item{max.rules.tree, max.tree}{Arguments controlling rule extraction when
  the cache is built.}

  \item{y.external}{Optional externally supplied working response. When
  \code{NULL}, the working response is built internally from the RHF object's
  integrated hazard exposure values.}

  \item{eps}{Nonnegative value added before taking the logarithm of the
  integrated hazard exposure when \code{y.external} is not supplied.}

  \item{verbose}{Logical. If \code{TRUE}, reports cache construction and
  per-window progress.}

  \item{x}{An object of class \code{"importance.rhf"}.}

  \item{top, rank.by}{Arguments used by \code{print()} and by
  \code{plot(type = "lines")}. Printing now ranks variables robustly over
  time by default using \code{rank.by = "q90"}. The line plot also uses
  \code{rank.by} when \code{vars} is omitted.}

  \item{digits, scientific.threshold}{Formatting controls for
  \code{print()}, used to keep very large importance values readable.}

  \item{row.names, optional}{Included for compatibility with
  \code{as.data.frame()}.}

  \item{format}{Output format for \code{as.data.frame()}. \code{"long"}
  returns the long-format table, \code{"variable_by_time"} returns a data
  frame whose rows are variables and columns are times, and
  \code{"time_by_variable"} returns the transpose with window metadata.}

  \item{type, vars, top_n_union}{Arguments controlling which
  variables are displayed and which plot is produced. \code{type = "dotmatrix"}
  gives the time-by-variable dot-matrix display, \code{type = "barplot"} gives
  the companion time-by-variable bar-matrix display, and \code{type = "lines"}
  gives a line, step, or smoothed view for selected variables. When \code{vars}
  is omitted, the line plot chooses \code{top} variables using \code{rank.by},
  while the dot-matrix and bar-matrix plots use the union of the top
  \code{top_n_union} variables across time.}

  \item{curve, smooth.f, lty, lwd, display.cap, display.note}{Arguments for
  \code{type = "lines"}. \code{curve} chooses between step, ordinary line,
  and lowess-smoothed displays; \code{smooth.f} is passed to
  \code{stats::lowess()} when needed; \code{lty} and \code{lwd} control line
  type and line width. \code{display.cap} applies display-only quantile
  capping to stabilize the vertical scale in the presence of extreme spikes,
  and \code{display.note} toggles the on-plot note when capping is applied.
  The same \code{display.note} flag is also used by the dot-matrix and
  bar-matrix plots.}

  \item{variable.labels, time.labels, sort_by, sort_abs}{Arguments
  controlling variable labeling and ordering in the dot-matrix and bar-matrix
  plots. Variable labels may be supplied as a named vector or a two-column data
  frame. Variables may be ordered by robust aggregate importance,
  alphabetically, hierarchical clustering, or left in their existing order.}

  \item{transform, color_by, point_color, bar_color, value_colors,
  sign_colors, cex.range, bar.width, bar.max.height, size.cap, color.cap,
  alpha, legend}{Arguments controlling dot size or bar height, color encoding,
  display-only quantile capping, transparency, and the optional right-side
  legend in the dot-matrix and bar-matrix plots. For the bar-matrix plot,
  \code{bar.width} controls the width of each bar within a time cell and
  \code{bar.max.height} controls the largest bar height within a variable row.
  When \code{color_by = "sign"}, bars are drawn upward or downward from the
  row center according to the sign of the importance value. In the bar-matrix
  plot, \code{point_color} is accepted as a compatibility alias for
  \code{bar_color}, and \code{cex.range} is accepted but ignored.}

  \item{xlab, ylab, main, axis.cex, var.cex, time.label.srt, show.grid,
  grid.col, zero.line, zero.col, border, mar, legend.width, width, height,
  save_plot, out.file}{Display, layout, and export options for the plotting
  helpers. By default the dot-matrix and bar-matrix plots use blank axis
  labels, draw light guide lines, compute margins automatically, and can
  optionally be written to file with \code{save_plot = TRUE}. \code{zero.line}
  and \code{zero.col} control row-center reference lines for signed bar plots;
  \code{border} is passed to the rectangles drawn by the bar-matrix plot.}

  \item{...}{Additional arguments passed to internal calculations or plotting
  routines.}
}

\details{

This routine implements a \emph{fast localization} strategy for RHF
VarPro importance. The master time grid is taken from
\code{time.interest}. For a window corresponding to a selected grid
index, the method keeps only those pseudo-individuals whose start-stop
interval overlaps that window, while reusing the same sampled rules and
near-miss sets already obtained from the RHF fit.

For RHF objects the underlying VarPro importance calculation follows a
regression-style approach in which the working response is the logarithm
of the integrated hazard exposure, and local rule importance is computed
by comparing this working response in a rule versus its near-miss
set. Time localization is achieved by restricting those memberships
within each window rather than rebuilding the entire rule structure
repeatedly.

The helper \code{varpro.cache()} stores the minimum information needed
for repeated localized importance calculations: a regression-style rule
template, window metadata, the working response source, and precomputed
window-local rule statistics. During cache construction, raw OOB and
complementary memberships are converted into compact per-window rule
summaries, so the later window sweep does not need to rescan membership
vectors.

The returned importance matrix has variables in rows and selected time
windows in columns. Column names correspond to the right endpoints of
the selected windows. The long-format table contains the same values
together with window metadata such as start, stop, midpoint, number at
risk, and number of active rules.

Printing and plotting share a robust strategy.  Summaries default to a
robust over-time ranking based on the 90th percentile, and the plotting
helpers apply optional quantile capping for display only. This prevents
rare extreme spikes from flattening curves while preserving the original
importance matrix for downstream analyses.

}

\value{

\code{varpro.cache()} returns an object containing cached rule
memberships, the working response used for importance, start-stop
information for pseudo-individuals, time-window metadata, and the rule
extraction settings.

\code{importance.rhf()} returns a list including:

\itemize{
  \item \code{importance.matrix}: matrix of localized importance values with
  variables in rows and selected time windows in columns.
  \item \code{importance.long}: long-format data frame containing variable,
  time, window metadata, and localized importance.
  \item \code{window.info}: data frame describing the analyzed windows,
  including start, stop, midpoint, \code{n.risk}, and \code{n.rules}.
  \item \code{y.source}: source of the working response. This is
  \code{"int.haz.oob"}, \code{"int.haz.test"}, or \code{"y.external"}.
  \item \code{trim}: tuning value used in importance aggregation.
}

\code{print()} returns its input invisibly after displaying a short summary
that includes robust over-time summaries for the leading variables.

\code{as.data.frame()} returns one of the supported data-frame views.

\code{dotmatrix.importance.rhf()} produces a base-R dot-matrix plot and
returns plotting metadata invisibly.

\code{barplot.importance.rhf()} produces a base-R bar-matrix plot and returns
plotting metadata invisibly.

\code{plot()} returns invisibly the result of the underlying plotting helper.
}


\seealso{
  \code{\link{rhf}}
}

\examples{
\donttest{
################################################################
##
## simulation model
##
################################################################

## draw simulation (can be modified)
n <- 400
p <- 10
simid <- 2
d <- hazard.simulation(type = simid, n = n, p = p, nrecords = 4)$dta

## fit a RHF model with weighted mtry (use for high-dimension)
f <- "Surv(id, start, stop, event) ~ ."
o <- rhf(f, d, ntree = 50, nsplit = 5, xvar.wt = xvar.wt.rhf(f, d))
print(o)

## time-localized RHF importance across the full time grid
imp.t <- importance.rhf(o)
print(imp.t)

## extract the variable-by-time matrix
print(head(imp.t$importance.matrix))

oldpar <- par(mfrow=c(1,1))

## dot-matrix importance plot (default)
plot(imp.t)

## companion bar-matrix importance plot
plot(imp.t, type = "barplot")

## step-style importance line plot for the top variables
## (ranked by the 90th percentile over time and display-capped at q99)
plot(imp.t, type = "lines", top = 10)

## smoothed importance plot for all variables with display capping
plot(imp.t, type = "lines", curve = "lowess", smooth.f = 0.5,
     display.cap = 0.95)

## dot-matrix plot with robust ordering and display capping
plot(imp.t, sort_by = "q90", size.cap = 0.99, color.cap = 0.99)

## bar-matrix plot using signed colors, if signed importance values are present
plot(imp.t, type = "barplot", color_by = "sign")

par(oldpar)


## reuse a cache for repeated calls on subsets of the time grid
cache <- varpro.cache(o)
imp.t.sub <- importance.rhf(
  o,
  cache = cache,
  time.index = seq(1, length(o$time.interest), by = 5),
  verbose = TRUE
)

## long-format export
print(head(as.data.frame(imp.t.sub)))
}
}

\keyword{rhf importance}
