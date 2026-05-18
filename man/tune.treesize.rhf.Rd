\name{tune.treesize.rhf}
\alias{tune.treesize.rhf}
\alias{tune.rhf}
\alias{tune.iAUC.rhf}
\alias{tune.iAUC}
\alias{plot.tune.treesize.rhf}
\title{Tune tree size for Random Hazard Forests}

\description{

  Tune the \code{treesize} for a Random Hazard Forest using either
  out-of-bag (OOB) empirical risk or an OOB integrated AUC computed by
  \code{\link{auct.rhf}}.

}

\usage{
tune.treesize.rhf(
  formula,
  data,
  ntree = 500,
  nsplit = 10,
  nodesize = NULL,
  perf = c("risk", "iAUC"),
  auct.args = NULL,
  lower = 2L,
  upper = NULL,
  C = 3,
  method = c("golden", "bisect"),
  max.evals = 20L,
  bracket.tol = 2L,
  seed = NULL,
  verbose = TRUE,
  forest = TRUE,
  ...
)


tune.iAUC.rhf(
  formula,
  data,
  auct.args = NULL,
  ...
)

\method{plot}{tune.treesize.rhf}(x, ylab = NULL, main = NULL,
    se.band = TRUE, se.mult = 1, ylim = NULL, \dots)
}


\arguments{
  
  \item{formula}{Survival formula for \code{\link{rhf}}, typically
    \code{Surv(id, start, stop, event) ~ .}.}

  \item{data}{Counting-process data frame containing the variables in
    \code{formula}.}

  \item{ntree}{Number of trees grown at each candidate \code{treesize}.}

  \item{nsplit}{Number of random split points tried per variable.}

  \item{nodesize}{Minimum number of events in a terminal node; passed to
    \code{\link{rhf}}.}

  \item{perf}{Tuning criterion. \code{"risk"} minimizes mean OOB risk;
    \code{"iAUC"} maximizes integrated AUC obtained using
    \code{\link{auct.rhf}}.}

  \item{auct.args}{Optional list of arguments passed to
    \code{\link{auct.rhf}} when \code{perf = "iAUC"}. }

  \item{lower}{Smallest \code{treesize} considered.}

  \item{upper}{Largest \code{treesize} considered.} 

  \item{C}{Multiplier used when computing the default upper bound if
    \code{upper} is \code{NULL}.}

  \item{method}{Discrete search strategy: \code{"golden"} (golden-section
    search) or \code{"bisect"} (bisection-style search).}

  \item{max.evals}{Maximum number of distinct \code{treesize} values
    evaluated by the search.}

  \item{bracket.tol}{Tolerance for the bracketing interval width used by
    the search algorithms.}

  \item{seed}{Optional random seed for reproducible comparisons across
    tree sizes.}

  \item{verbose}{Logical; if \code{TRUE}, print progress messages when
    evaluating new \code{treesize} values.}

  \item{forest}{Logical; if \code{TRUE}, return the RHF object at the
    best \code{treesize}.}

  \item{\dots}{Additional arguments passed to \code{\link{rhf}}.}

  \item{x, ylab, main, se.band, se.mult, ylim}{Arguments to
     customize \code{plot}.}
   
}

\details{

The alias \code{tune.rhf} is provided for convenience.

  
Rpeatedly fits a hazard forest with different values of \code{treesize}
and selects the value that optimizes the chosen criterion
\code{perf}. When \code{perf = "risk"}, the criterion is the mean OOB
risk. When \code{perf = "iAUC"}, \code{\link{auct.rhf}} is applied to
each fit and the criterion is \code{1 - iAUC.uno}.  \code{tune.iAUC} is
a wrapper around \code{tune.rhf} with \code{perf = "iAUC"}, using
default \code{auct.rhf} settings when \code{auct.args = NULL}.

}

\value{
An object of class \code{"tune.treesize.rhf"} with components
\itemize{
  \item \code{best.size}: optimal \code{treesize};
  \item \code{best.err}: minimal criterion value (mean OOB risk if
    \code{perf = "risk"}, or \code{1 - iAUC.uno} if \code{perf = "iAUC"});
  \item \code{path}: data frame with evaluated \code{treesize} values and
    corresponding criterion values (and an \code{iAUC} column when
    \code{perf = "iAUC"});
  \item \code{forest}: RHF fit at \code{best.size} when
    \code{forest = TRUE}.}
}

\seealso{
\code{\link{rhf}}, \code{\link{auct.rhf}}
}

\examples{
\donttest{
## ------------------------------------------------------------
## Example: tuning treesize by OOB empirical risk
## ------------------------------------------------------------

set.seed(7)
d <- hazard.simulation(1)$dta
f <- "Surv(id, start, stop, event) ~ ."

## tune.rhf is an alias for tune.treesize.rhf
tuneRisk <- tune.rhf(f, d)  ## same as tune.treesize.rhf(f, d)

oldpar <- par(mfrow = c(1, 1))
plot(tuneRisk)
par(oldpar)

tuneRisk$best.size    # treesize minimizing mean OOB risk
tuneRisk$best.err

## Access the tuned forest and refit AUC diagnostics if desired
best.forest <- tuneRisk$forest
a.risk <- auct.rhf(best.forest)
print(a.risk$iAUC.uno)


## ------------------------------------------------------------
## Example: tuning treesize by iAUC
## ------------------------------------------------------------

set.seed(7)
tuneiAUC <- tune.iAUC(f, d)

oldpar <- par(mfrow = c(1, 1))
plot(tuneiAUC)
par(oldpar)

print(tuneiAUC$best.size)
print(1 - tuneiAUC$best.err)

best.forest.iauc <- tuneiAUC$forest
a.iauc <- auct.rhf(best.forest.iauc)
print(1 - a.iauc$iAUC.uno)


## ------------------------------------------------------------
## Example: tuning treesize by iAUC with bootstrap s.e.
## ------------------------------------------------------------

set.seed(7)
tuneiAUC.boot <- tune.iAUC(f, d, auct.args=list(bootstrap.rep=25))
oldpar <- par(mfrow = c(1, 1))
plot(tuneiAUC.boot)        
par(oldpar)

## ------------------------------------------------------------
## Example: tuning by hazard-based incident AUC
## ------------------------------------------------------------

set.seed(7)
tuneHazInc <- tune.iAUC(
  f, d,
  auct.args = list(
    marker       = "hazard",
    method       = "incident"
  )
)

oldpar <- par(mfrow = c(1, 1))
plot(tuneHazInc)
par(oldpar)

print(tuneHazInc$best.size)
print(tuneHazInc$best.err)

## Confirm the tuned incident AUC on the selected forest
best.forest.haz <- tuneHazInc$forest
aHazInc <- auct.rhf(
  best.forest.haz,
  marker  = "hazard",
  method  = "incident"
)
print(aHazInc)
print(1 - aHazInc$iAUC.uno)

}}

\keyword{survival}
\keyword{tuning}
