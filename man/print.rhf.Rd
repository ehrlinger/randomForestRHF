\name{print.rhf}
\alias{print.rhf}
\alias{print.vimp.rhf}
\title{Print Output from a Random Hazard Forest Analysis}
\description{
  Print a compact summary of a Random Hazard Forest analysis. This is the
  default print method for objects produced by \code{grow} or \code{predict}.
}
\usage{
\method{print}{rhf}(x, digits = 3, label.width = 34, ...)
}
\arguments{
  \item{x}{An object of class \code{rhf} created by \code{grow} or \code{predict}.
           When \code{predict} is called without new data (``restore mode''), the
           original forest and its OOB risk are returned.}
  \item{digits}{Number of digits used for printed numeric summaries.}
  \item{label.width}{Width used to right-justify labels in the printed output.}
  \item{...}{Additional arguments (currently unused).}
}
\details{
  The printed summary includes basic sample information, forest size
  characteristics, event counts when available, and risk metrics
  (in-bag, OOB, or test risk depending on how the object was created).
}
\value{
  Returns the object \code{x} invisibly.
}
\author{
  Hemant Ishwaran and Udaya B. Kogalur
}
\keyword{print}
