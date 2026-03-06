\name{predict.rhf}
\alias{predict.rhf}
\title{Prediction on Test Data for Random Hazard Forests}

\description{
  Obtain predicted values on test data using a trained random hazard
  forests.
}

\usage{\method{predict}{rhf}(object, newdata,  get.tree = NULL,
     block.size = 10, membership = TRUE, seed = NULL, do.trace = FALSE,...)}

\arguments{

  \item{object}{An \code{rhf} object returned from a previous training call to \code{rhf}.}

  \item{newdata}{Test data frame. If omitted, the original training data is used and the full training forest is restored.}

  \item{get.tree}{Optional vector of integer indices specifying which trees to use for ensemble predictions. Defaults to using all trees in the forest.}

  \item{block.size}{Controls how cumulative error rate is reported. To obtain cumulative error every \code{n} trees, set this to an integer between 1 and \code{ntree}.}

  \item{membership}{Logical flag indicating whether terminal node membership and inbag information should be returned.}

  \item{seed}{Negative integer specifying the random seed for reproducibility.}

  \item{do.trace}{Number of seconds between progress updates printed to the console.}

  \item{...}{Additional optional arguments passed to internal methods.}

}

\details{

  Returns the predicted values for a random hazard forests.
  
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



}

\seealso{
  \command{\link{rhf}}
}

\examples{
\donttest{


## ------------------------------------------------------------
## canonical train/test example (synthetic data)
## ------------------------------------------------------------

simID <- 1
trn <- hazard.simulation(simID)$dta
tst <- hazard.simulation(simID)$dta
f <- "Surv(id, start, stop, event) ~ ."

## training
o <- rhf(f, trn)
print(o)

## testing
p <- predict(o, tst)
print(p)


## ------------------------------------------------------------
##  pbc: train/test example
## ------------------------------------------------------------

library("randomForestSRC")
data(pbc, package = "randomForestSRC")
pbc.raw <- na.omit(pbc)
trn <- sample(1:nrow(pbc.raw), size=nrow(pbc.raw) * .75, replace = FALSE)

d.trn <- convert.counting(Surv(days, status) ~ ., pbc.raw[trn,])
d.tst <- convert.counting(Surv(days, status) ~ ., pbc.raw[-trn,])
f <- "Surv(id, start, stop, event) ~ ."

## train/predict
o <- rhf(f, d.trn)
print(predict(o, d.tst))

## restore the forest
print(predict(o))

}
}

\keyword{predict rhf}
