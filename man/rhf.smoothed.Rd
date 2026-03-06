\name{rhf.smoothed.hazard}
\alias{rhf.smoothed.hazard}
\alias{rhf.smooth.hazard}
\title{Smoothed hazard and cumulative hazard estimates from a random hazard forest}

\description{

Compute smoothed case-specific hazard curves (and corresponding
cumulative hazards) from a Random Hazard Forest (RHF) object. The
returned object is a lightweight list designed to be directly usable by
\code{\link{plot.rhf}}.

}

\usage{
rhf.smoothed.hazard(
  o,
  method = c("median.loess", "loess"),
  oob = TRUE,
  span = 0.35,
  degree = 1,
  family = NULL,
  eps = 1e-12,
  suppress.warnings = TRUE,
  trace = FALSE
)
}

\arguments{

  \item{o}{A fitted RHF object or predict object.}


  \item{method}{
    Smoothing strategy.
    See \bold{Details} for definitions of \code{"loess"} and \code{"median.loess"}.
  }

  \item{oob}{By default out-of-bag (OOB) values are returned for RHF
    objects, but inbag values can be requested by setting this option
    to \code{FALSE}.  Does not apply to RHF predict objects.}

  \item{span}{Loess span passed to \code{\link[stats]{loess}}.}

  \item{degree}{Loess degree passed to \code{\link[stats]{loess}}.}

  \item{family}{
    Loess family passed to \code{\link[stats]{loess}}.
    Must be \code{"gaussian"} or \code{"symmetric"}.
    If \code{NULL}, defaults to \code{"gaussian"} for \code{method="median.loess"}
    and \code{"symmetric"} for \code{method="loess"}.
  }

  \item{eps}{
    Small positive constant added before log-transforming hazards: \code{log(h + eps)}.
  }

  \item{suppress.warnings}{
    If \code{TRUE}, warnings arising from \code{loess()} fitting/prediction are suppressed.
  }

  \item{trace}{If \code{TRUE}, print progress messages.}
}

\details{

Hazard curves are computed/smoothed on the grid \code{o$time.interest}.
Smoothing is performed on the log-hazard scale: \code{log(hazard +
eps)}, and then back-transformed to the hazard scale.


\describe{ 


  \item{\code{method="loess"}}{
    Applies loess smoothing to an already-constructed hazard matrix
    stored in \code{o} (e.g. \code{o$hazard.oob} and/or
    \code{o$hazard.inbag}), independently for each subject.  }

  \item{\code{method="median.loess"}}{
    For each subject and time bin, computes the median across trees of
    the terminal-node (leaf) log-hazard \code{log(h + eps)} (using a
    native C routine), then applies loess smoothing to the resulting
    median log-hazard curve and back-transforms.  This method requires
    the fitted object to retain tree-specific hazard and membership
    structures (e.g., \code{o$forest$t.hazard} and related arrays).  }

}

}


\value{

A list containing:
  
\itemize{
  \item \code{id}, \code{ensemble.id}, \code{yvar}, \code{time.interest} copied from \code{o}.
  \item Depending on \code{target}:
  \itemize{
    \item \code{hazard.oob}, \code{chf.oob} (out-of-bag), matrices of dimension
    \code{nSubj x length(time.interest)}.
    \item \code{hazard.inbag}, \code{chf.inbag} (in-bag), matrices of dimension
    \code{nSubj x length(time.interest)}.
    \item \code{hazard.test}, \code{chf.test} (test), matrices of dimension
    \code{nSubjTest x length(time.interest)}.
  }
}
}


\seealso{
\code{\link{rhf}}, \code{\link{predict.rhf}}, \code{\link{plot.rhf}}
}

\examples{
\donttest{

## ------------------------------------------------------------
## canonical example using synthetic data
## includes both train/test scenarios
## ------------------------------------------------------------

simID <- 1
trn <- hazard.simulation(simID)$dta
tst <- hazard.simulation(simID)$dta
f <- "Surv(id, start, stop, event) ~ ."

## training/testing
o <- rhf(f, trn)
p <- predict(o, tst)

## default: median(log leaf hazard) + loess
so <- rhf.smoothed.hazard(o)
sp <- rhf.smoothed.hazard(p)

## the returned object can be passed directly to plot.rhf()
plot.rhf(so)
plot.rhf(sp)


## ------------------------------------------------------------
##  peak vo2
## ------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")
d <- convert.counting(Surv(ttodead, died)~., peakVO2)
f <- "Surv(id, start, stop, event) ~ ."

## training
o <- rhf(f, d)

## median loess
s0 <- rhf.smoothed.hazard(o) 
ids <- o$ensemble.id[1:3]
plot.rhf(s0, idx = ids)

## loess smoothing 
s1 <- rhf.smoothed.hazard(o, method = "loess")
plot.rhf(s1, idx = ids)

}
}

\keyword{survival}
\keyword{models}
