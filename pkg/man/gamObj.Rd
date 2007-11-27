\name{gamObj}
\alias{gamObj}
\alias{gamObj.spsth}
\alias{gamObj.slockedTrain}
\title{Generic Function and Methods for Extracting a gamObject}
\description{
  Some functions of \code{STAR}, like \code{spsth} and
  \code{slockedTrain} perform gam fits internally and keep as a list
  component or within the environment of a returned function the result
  of this fit. Method \code{gamObj} extracts this gam object.
}
\usage{
gamObj(object, ...)
\method{gamObj}{spsth}(object, ...)
\method{gamObj}{slockedTrain}(object, ...)
}
\arguments{
  \item{object}{an object containing a \code{gamObject}. Currently the
    result of a call to \code{\link{spsth}} or to
    \code{\link{slockedTrain}}.
  }
  \item{\dots}{not used for now}
}
\value{
  A \code{\link[mgcv]{gamObject}}
}
\author{Christophe Pouzat  \email{christophe.pouzat@gmail.com}}
\seealso{
  \code{\link[mgcv]{gam}},
  \code{\link[mgcv]{gamObject}},
  \code{\link{spsth}},
  \code{\link{slockedTrain}}
}
\examples{
##
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{smooth}
\keyword{regression}
