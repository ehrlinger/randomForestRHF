\name{rhf}
\alias{rhf}
\title{Random Hazard Forests}
\description{

  Random Hazard Forest (RHF) is a tree-ensemble survival method that
  estimates case-specific hazards and cumulative hazard functions on a
  working time grid and supports time-dependent covariates using
  counting-process (start/stop) format.

}

\usage{
rhf(formula,
      data,
      ntree = 500,
      nsplit = 10,
      treesize = NULL,
      nodesize = NULL,
      block.size = 10,
      bootstrap = c("by.root", "none", "by.user"),
      samptype = c("swor", "swr"),
      samp = NULL,
      case.wt = NULL,
      membership = TRUE,
      sampsize = if (samptype == "swor") function(x){x * .632} else function(x){x},
      xvar.wt = NULL,
      ntime = 50,
      min.events.per.gap = 10,
      seed = NULL,
      do.trace = FALSE,
      ...)
}

\arguments{

  \item{formula}{Model formula specifying the response and predictors.}

  \item{data}{Data frame containing the variables referenced in \code{formula}.}

  \item{ntree}{Number of trees to grow in the ensemble.}

  \item{nsplit}{Non-negative integer specifying the number of random
  split points to consider for each variable. When set to 0, all
  possible split points are evaluated (deterministic splitting), which
  may be slower. The default is 10.}

  \item{treesize}{Specifies the desired tree size, defined as the number
  of terminal nodes. Can be provided as a function of sample size
  \code{n}. If unspecified, an internal default is used.}

  \item{nodesize}{Minimum number of cases required in a terminal
    node. If not specified, an internal default is used.}
  
  \item{block.size}{Controls the granularity of cumulative error rate
  reporting. Setting this to an integer between 1 and \code{ntree}
  yields the cumulative error every \code{block.size} trees.}

  \item{bootstrap}{Bootstrap strategy for generating inbag samples. The
  default is \code{"by.root"}, which bootstraps by sampling with or
  without replacement (default is without; see \code{samptype}). If set
  to \code{"none"}, no bootstrapping is performed (OOB predictions and
  errors are then unavailable). The option \code{"by.user"} uses a
  user-defined bootstrap specified by \code{samp}.}

  \item{samptype}{Sampling type used when \code{bootstrap =
  "by.root"}. Options are \code{"swor"} (sampling without replacement,
  default) and \code{"swr"} (sampling with replacement).}

  \item{samp}{User-specified bootstrap when \code{bootstrap =
  "by.user"}. Should be an \code{n} by \code{ntree} array, where each
  entry gives the number of times a case appears in the inbag sample for
  a given tree.}

  \item{case.wt}{Non-negative vector of case weights (not required to
  sum to 1). Higher weights increase the probability of an observation
  being selected during bootstrapping or subsampling. Using real-valued
  weights is preferred over integer weights.}

  \item{membership}{Logical flag indicating whether to return terminal
    node membership and inbag information.}
  
  \item{sampsize}{Specifies the size of the bootstrap sample when
  \code{bootstrap = "by.root"}. For sampling without replacement, it
  represents the requested subsample size (default is 0.632 times the
  sample size). For sampling with replacement, it equals the sample
  size. Can be supplied as a numeric value or a function.}

  \item{xvar.wt}{Non-negative vector of variable selection probabilities
  for splitting. Values do not need to sum to 1. Defaults to uniform
  selection probability.}


  \item{ntime}{Controls the working time grid used for all
   ensemble calculations. Can be:
   \describe{

     \item{\code{ntime >= 1} (integer):}{Request approximately \code{ntime} grid
        points chosen from the observed event times. When used with
       \code{min.events.per.gap}, grid points are selected adaptively so that each
       interval contains at least \code{min.events.per.gap} events (when possible);
       therefore the resulting grid may contain fewer than \code{ntime} points when
       events are sparse (especially in the tail).}

     \item{numeric vector:}{A user-supplied set of time points. Each value is
       aligned (snapped) to the nearest observed event time and duplicates are
       removed.}

     \item{\code{0} or \code{NULL}:}{Use all observed (unique) event times.}

 }}
 
 \item{min.events.per.gap}{Minimum number of observed events required in
   each time interval when \code{ntime} is specified as an integer.
   Together with ntime, this provides an automatic event-balanced grid
   that allocates more resolution where events are dense and avoids 
   sparse tail intervals.}

  \item{seed}{Negative integer setting the random seed for reproducibility.}

  \item{do.trace}{Time in seconds between progress updates printed to the console.}

  \item{...}{Additional arguments (currently unused).}

}

\details{
\code{rhf()} grows an ensemble of hazard trees for survival
outcomes specified in counting-process notation
\code{Surv(id, start, stop, event)}. Predictors may include both time-static
variables and time-dependent covariates (TDCs). The fitted object contains
case-specific ensemble estimates of the hazard and cumulative hazard on the
working time grid \code{time.interest}.

Random Hazard Forests (RHF) extends Random Survival Forests (RSF) in two
key ways: (1) it directly estimates the hazard function, and (2) it
accommodates time-dependent covariates.

Tree construction uses best-first splitting (BFS) guided by empirical
risk reduction. The risk is based on a smooth convex surrogate for the
nonparametric log-likelihood functional, as described in Lee, Chen, and
Ishwaran (2021).

Splits can occur on both static and time-dependent covariates (TDCs),
but not on time itself. Time splitting is unnecessary, as terminal node
hazard estimators already incorporate time appropriately. To encourage
selection of time-dependent covariates, use the \code{xvar.wt} option to
weight their importance.


The case-specific hazard estimator uses a "stitched" active-record rule
on the \code{time.interest} grid: each record is treated as active from
just after its start time up to (and including) the next record's start
time, with the final record extended to the maximum time in
\code{time.interest}.


\strong{Data Format:} Input data must follow standard counting process
format and include the following four columns (column names must match
those specified in the counting process formula; see examples):


\enumerate{

  \item \code{id}: A unique integer identifier for each
  individual. Repeated for multiple rows corresponding to that
  individual.
  

  \item \code{start}: The start time for each interval. All time values
  must be scaled to the interval [0, 1].
  

  \item \code{stop}: The stop time for each interval. Must satisfy
  \code{stop > start}.
  

  \item \code{event}: A binary event indicator (0 = censored, 1 =
  event). Only one event is allowed per individual; otherwise, the
  individual is treated as censored.
  

}

Use the helper function \code{convert.counting} to convert conventional
survival data to counting process format. See the examples section for
further illustration and guidance on data preparation.


}

\value{

  An object of class \code{"rhf"} containing the fitted forest and related
  results. Components include (among others) the working time grid
  \code{time.interest} and case-specific ensemble estimates of the hazard
  and cumulative hazard. When bootstrapping is used, these are typically
  returned as \code{hazard.oob}/\code{chf.oob} (out-of-bag) and
  \code{hazard.inbag}/\code{chf.inbag} (in-bag). Use \code{\link{predict.rhf}}
  to obtain estimates for new data.

}

\author{

  Hemant Ishwaran and Udaya B. Kogalur

}

\references{
  
  Ishwaran H. and Kogalur U.B. (2007).  Random survival forests for R,
  \emph{Rnews}, 7(2):25-31.
  
  Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S.
  (2008).  Random survival forests, \emph{Ann. App.
    Statist.}, 2:841-860.

  Lee, D.K. and Chen N. and Ishwaran H (2021). Boosted nonparametric
  hazards with time-dependent covariates. \emph{Annals of Statistics},
  49: 2101-2128.

  Ishwaran H. (2025). Multivariate Statistics: Classical Foundations and
  Modern Machine Learning.  Chapman and Hall.

  Ishwaran H., Kogalur U.B., Hsich E.M. and Lee D.K. (2026).  Random
  hazard forests.
  
}

\seealso{
  \code{\link{auct.rhf}},
  \code{\link{plot.rhf}},
  \code{\link{predict.rhf}},
  \code{\link{smoothed.hazard.rhf}},
  \code{\link{tune.treesize.rhf}}
}


\examples{

  
## ------------------------------------------------------------
##  time-static pbc: parameters set for fast CRAN run
## ------------------------------------------------------------

## load the data
data(pbc, package = "randomForestSRC")
## convert the data to counting process
d <- convert.counting(Surv(days, status) ~ ., na.omit(pbc))
## set the formula
f <- "Surv(id, start, stop, event) ~ ."

## rhf call
print((o <- rhf(f, d, ntree = 3)))

## smooth hazard estimator for specific cases
plot(o, idx=c(1,5,10))
plot(o, idx=c(1,5,10), hazard.only=TRUE)
plot(o, idx=c(1,5,10), hazard.only=TRUE, lwd=0)
plot(o, idx=1:10, lwd=0, hazard.only=TRUE, legend.show=FALSE)

\donttest{

## ------------------------------------------------------------
##  time-static pbc: compare RHF to RSF
## ------------------------------------------------------------

data(pbc, package = "randomForestSRC")
d <- convert.counting(Surv(days, status) ~ ., na.omit(pbc))

## first we run rhf
o.rhf <- rhf("Surv(id, start, stop, event) ~ .", d)
h <- o.rhf$hazard.oob
time <- o.rhf$time.interest
delta <- c(0, diff(time))
H <- apply(h, 1, function(x) {cumsum(delta * x)})
S.rhf <- exp(-H)

## next we run rsf, using the same time.interest grid
o.rsf <- randomForestSRC::rfsrc(Surv(days, status) ~ ., pbc, ntime = time)
S.rsf <- t(o.rsf$survival.oob)

## graphical parameters
bass <- 3
oldpar <- par(mfrow=c(3,2))

## plot survival results 
matplot(time,S.rhf,pch=16)
matplot(time,S.rsf,pch=16)
matplot(time,S.rhf-S.rsf,pch=16)
boxplot(S.rhf-S.rsf,ylab="S.rhf-S.rsf",xaxt="n",outline=FALSE,range=1e-10)
abline(h=0,lwd=3,col=2)

## plot chf and hazard results
matplot(time,H,ylab="CHF",pch=16)
matplot(time,t(h),ylab="hazard",pch=16,type="n",ylim=c(0,quantile(c(h),.95)))
nO <- lapply(1:nrow(h), function(i) {
  lines(supsmu(time, h[i,], bass=bass),type="s",col=gray(.4))
})

par(oldpar)

## ------------------------------------------------------------
##  TDC illustration (using built in hazard simulation function)
## ------------------------------------------------------------

d1 <- hazard.simulation(1)$dta
d2 <- hazard.simulation(2)$dta
d3 <- hazard.simulation(3)$dta
f <- "Surv(id, start, stop, event) ~ ."

o1 <- rhf(f, d1)
o2 <- rhf(f, d2)
o3 <- rhf(f, d3)

plot(o1,idx=4)
plot(o2,idx=1:3)
plot(o3,idx=2)


## ------------------------------------------------------------
##  peakVO2: demonstrates time-varying auc
## ------------------------------------------------------------

data(peakVO2, package = "randomForestSRC")
d <- convert.counting(Surv(ttodead, died)~., peakVO2)
f <- "Surv(id, start, stop, event) ~ ."

## build the forest
o1 <- rhf(f, d, nodesize=1)
o2 <- rhf(f, d, nodesize=15)

## acquire auc-t
a1.chf <- auct.rhf(o1) ## same as auct.rhf(o1, marker = "chf")
a1.haz <- auct.rhf(o1, marker = "haz")
a2.chf <- auct.rhf(o2) ## same as auct.rhf(o2, marker = "chf")
a2.haz <- auct.rhf(o2, marker = "haz")

## print auc-t
print(a1.chf)
print(a2.chf)
print(a1.haz)
print(a2.haz)

## plot auc-t
oldpar <- par(mfrow=c(2,2))
plot(a1.chf, main="nodesize 1")
plot(a1.haz, main="nodesize 1")
plot(a2.chf, main="nodesize 15")
plot(a2.haz, main="nodesize 15")
par(oldpar)

## ------------------------------------------------------------
##  more detailed example of time-varying performance 
## ------------------------------------------------------------

d <- hazard.simulation(1)$dta
f <- "Surv(id, start, stop, event) ~ ."
treesize <- c(10, 30, 100)

oldpar <- par(mfrow=c(3,2))
perf <- lapply(treesize, function(tz) {
  o <- rhf(f, d, treesize=tz, nodesize=1)
  print(o)
  rO <- list(chf = auct.rhf(o, marker="chf"),
             haz = auct.rhf(o, marker="haz"))
  plot(rO$chf, main=paste("chf marker, treesize=", tz))
  plot(rO$haz, main=paste("hazard marker, treesize=", tz))
  rO
})
par(oldpar)

## ------------------------------------------------------------
##  example illustrating tuning the tree size
##  using function 'rhf.tune.treesize'
## ------------------------------------------------------------

d <- hazard.simulation(1)$dta
f <- "Surv(id, start, stop, event) ~ ."

tune <- tune.rhf(f, d)  ## same as rhf.tune.treesize(f,d)
oldpar <- par(mfrow=c(1,1))
plot(tune)
par(oldpar)

## ------------------------------------------------------------
##  example illustrating guided feature selection
##  using built in 'xvar.wt.rhf' helper
## ------------------------------------------------------------

d <- hazard.simulation(1)$dta
f <- "Surv(id, start, stop, event) ~ ."

o <- rhf(f,d)
o.gfs <- rhf(f,d, xvar.wt = xvar.wt.rhf(f, d))

print(o)
print(o.gfs)

}
}


\keyword{rhf}
