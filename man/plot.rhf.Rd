\name{plot.rhf}
\alias{plot.rhf}
\title{Plot smoothed hazard and cumulative hazard plots from RHF analysis}

\description{
  Plot case specific hazard and cumulative hazard (CHF) from a fitted
  random hazard forest (RHF) object. Hazards are smoothed with
  \code{stats::supsmu}. Optional scaling places hazards on the same
  scale as the CHF for easier comparison.
}

\usage{\method{plot}{rhf}(x, idx = NULL, scale.hazard = FALSE, 
         ngrid = 30, bass = 0, q = 0.99, grid = FALSE,
         col = NULL, lty = NULL, legend.loc = "topright",
         jitter.factor = 1, lwd = 4,
         hazard.only = TRUE, legend.show = TRUE, ...)
}

\arguments{

  \item{x}{An \code{rhf} fit object returned by \code{\link{rhf}}.}

  \item{idx}{Subject identifiers selecting which cases to plot. Values must
  match those in \code{unique(x$id)}; by default the first value is used.}

  \item{scale.hazard}{Logical or numeric. If \code{TRUE}, hazards are
  multiplied by the local time step so they approximate CHF increments.
  If numeric, hazards are multiplied by the given constant.}

  \item{ngrid}{Number of equally spaced points used when \code{grid = TRUE}.}

  \item{bass}{Smoothing parameter passed to \code{stats::supsmu}.}

  \item{q}{Quantile used to trim extreme hazard values before smoothing.}

  \item{grid}{If \code{TRUE}, overlays smoothed CHF and its derivative.}

  \item{col}{Colors for selected cases. Recycled as needed.}

  \item{lty}{Line types for hazard and CHF curves.}

  \item{legend.loc}{Legend location. Passed to \code{\link[graphics]{legend}}.}

  \item{jitter.factor}{Amount of horizontal jitter applied to hazard spikes.}

  \item{lwd}{Line width for hazard spikes.}

  \item{hazard.only}{If \code{TRUE}, only the hazard is drawn.}

  \item{legend.show}{If \code{TRUE}, show a legend when plotting multiple cases.}
  
  \item{...}{Additional graphics parameters passed to \code{\link[graphics]{plot}}.}

}

\details{
  
The function displays out of bag hazard and CHF estimates on the grid
\code{x$time.interest}. Hazards are smoothed using
\code{\link[stats]{supsmu}} after trimming large values for stability.
Scaling the hazard allows direct visual comparison with CHF.
Plots are drawn to the current device; no model re-estimation is
performed.

}

\value{
  Invisibly returns \code{NULL}. The function is used for its plotting side effects.
}

\author{
  Hemant Ishwaran and Udaya B. Kogalur
}

\references{

  Ishwaran H. and Kogalur U.B. (2007).
  Random survival forests for R.
  \emph{R News}, 7(2): 25--31.

  Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008).
  Random survival forests.
  \emph{Annals of Applied Statistics}, 2: 841--860.

  Lee D.K., Chen N., and Ishwaran H. (2021).
  Boosted nonparametric hazards with time dependent covariates.
  \emph{Annals of Statistics}, 49: 2101--2128.

  Ishwaran H., Lee D.K. and Hsich E.M. (2025).
  Random hazard forests.
}

\seealso{
  \code{\link{hazard.simulation}},
  \code{\link{predict.rhf}},
  \code{\link{rhf}},
  \code{\link{smoothed.hazard.rhf}}
}

\examples{
\donttest{

## ------------------------------------------------------------
##  time static peakVO2
## ------------------------------------------------------------
data(peakVO2, package = "randomForestSRC")
d <- convert.counting(Surv(ttodead, died)~., peakVO2)
f <- "Surv(id, start, stop, event) ~ ."

set.seed(1)
o <- rhf(f, d)

oldpar <- par(mfrow=c(1,1))

## plot selected cases
ids <- o$ensemble.id[1:3]
plot(o, idx = ids)

## hazard only
plot(o, idx = ids)

## scaled hazard with CHF
plot(o, idx = ids, scale.hazard = TRUE, hazard.only = FALSE)

## auxiliary grid with smoother control
plot(o, idx = ids, grid = TRUE, ngrid = 60, bass = 2, hazard.only = FALSE)

## multiple cases, no legend
plot(o, idx = o$ensemble.id[1:10], lwd = 0, legend.show = FALSE)

## lowess median smoothed hazard
s <- smoothed.hazard(o)  ## default method="median.loess"
plot(s, idx = o$ensemble.id[1:10])
plot(s, idx = o$ensemble.id[1:10], lwd = 0)


par(oldpar)

## ------------------------------------------------------------
##  complex simulated time dependent covariate hazards
## ------------------------------------------------------------
f <- "Surv(id, start, stop, event) ~ ."
sim2 <- hazard.simulation(2)$dta
o2 <- rhf(f, sim2)

oldpar <- par(mfrow=c(1,1))
plot(o2, 1)
par(oldpar)

## ------------------------------------------------------------
##  same complex simulation, but using smoothed hazard
## ------------------------------------------------------------
f <- "Surv(id, start, stop, event) ~ ."
sim2 <- hazard.simulation(2)$dta
o2 <- rhf(f, sim2)

s2.loess <- smoothed.hazard(o2, method="loess")
s2.med.loess <- smoothed.hazard(o2, method="median.loess")

oldpar <- par(mfrow=c(1,1))
plot(o2, 1)
plot(s2.loess, 1)
plot(s2.med.loess, 1)
plot(s2.med.loess, 1:10, lwd = 0)
par(oldpar)


}

}

\keyword{plot}
\keyword{rhf}
