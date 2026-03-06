\name{rhf.tune.treesize}
\alias{rhf.tune.treesize}
\alias{rhf.tune}
\alias{rhf.tune.iAUC}
\alias{plot.rhf.tune.treesize}
\alias{plot.rhf.tune}
\title{Tune tree size for Random Hazard Forests}

\description{

  Tune the \code{treesize} for a Random Hazard Forest using either
  out-of-bag (OOB) empirical risk or an OOB integrated AUC computed by
  \code{\link{rhf.auct}}.

}

\usage{
rhf.tune.treesize(
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


rhf.tune.iAUC(
  formula,
  data,
  auct.args = NULL,
  ...
)

\method{plot}{rhf.tune.treesize}(x, ylab = NULL, main = NULL,
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
    \code{\link{rhf.auct}}.}

  \item{auct.args}{Optional list of arguments passed to
    \code{\link{rhf.auct}} when \code{perf = "iAUC"}. }

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
  
Rpeatedly fits a hazard forest with different values of
\code{treesize} and selects the value that optimizes the chosen
criterion \code{perf}. When \code{perf = "risk"}, the criterion is the
mean OOB risk. When \code{perf = "iAUC"}, \code{\link{rhf.auct}} is
applied to each fit and the criterion is \code{1 - iAUC.uno}. The alias
\code{rhf.tune} is provided for convenience. \code{rhf.tune.iAUC} is a
wrapper around \code{rhf.tune.treesize} with \code{perf = "iAUC"}, using
default \code{rhf.auct} settings when \code{auct.args = NULL}.

}

\value{
An object of class \code{"rhf.tune.treesize"} with components
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
\code{\link{rhf}}, \code{\link{rhf.auct}}
}

\examples{
\donttest{
## ------------------------------------------------------------
## Example: tuning treesize by OOB empirical risk
## ------------------------------------------------------------

set.seed(7)
d <- hazard.simulation(1)$dta
f <- "Surv(id, start, stop, event) ~ ."

## rhf.tune is an alias for rhf.tune.treesize
tune.risk <- rhf.tune(f, d)  ## same as rhf.tune.treesize(f, d)

oldpar <- par(mfrow = c(1, 1))
plot(tune.risk)
par(oldpar)

tune.risk$best.size    # treesize minimizing mean OOB risk
tune.risk$best.err

## Access the tuned forest and refit AUC diagnostics if desired
best.forest <- tune.risk$forest
a.risk <- rhf.auct(best.forest)
print(a.risk$iAUC.uno)


## ------------------------------------------------------------
## Example: tuning treesize by iAUC
## ------------------------------------------------------------

set.seed(7)
tune.iauc <- rhf.tune.iAUC(f, d)

plot(tune.iauc)        
print(tune.iauc$best.size)
print(1 - tune.iauc$best.err)

best.forest.iauc <- tune.iauc$forest
a.iauc <- rhf.auct(best.forest.iauc)
print(1 - a.iauc$iAUC.uno)


## ------------------------------------------------------------
## Example: tuning treesize by iAUC with bootstrap s.e.
## ------------------------------------------------------------

set.seed(7)
tune.iauc.boot <- rhf.tune.iAUC(f, d, auct.args=list(bootstrap.rep=25))
plot(tune.iauc.boot)        

## ------------------------------------------------------------
## Example: tuning by hazard-based incident AUC
## ------------------------------------------------------------

set.seed(7)
tune.haz.inc <- rhf.tune.iAUC(
  f, d,
  auct.args = list(
    marker       = "hazard",
    method       = "incident"
  )
)

plot(tune.haz.inc)
print(tune.haz.inc$best.size)
print(tune.haz.inc$best.err)

## Confirm the tuned incident AUC on the selected forest
best.forest.haz <- tune.haz.inc$forest
a.haz.inc <- rhf.auct(
  best.forest.haz,
  marker  = "hazard",
  method  = "incident"
)
print(a.haz.inc)
print(1 - a.haz.inc$iAUC.uno)

}}

\keyword{survival}
\keyword{tuning}
