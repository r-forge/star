\name{trainWAV}
\alias{trainWAV}
\title{ Generate wav Files from Spike Train(s) }
\description{
  Given one or two \code{spikeTrain} object(s), \code{trainWav} generates
  a \code{wav} file containing the result of the convolution of the spike
  train(s) with the single period of a sine function.
}
\usage{
trainWAV(leftCh, rightCh = NULL, filename, leftChFreq = 500,
         rightChFreq = 1000, rate = 10000, bits = 8, pan = 50,
         overwrite = FALSE)
}
\arguments{
  \item{leftCh}{a \code{spikeTrain} object or a numeric vector which can
  be coerced to such an object. The left channel of the resulting file
  if two channels are specified.}
  \item{rightCh}{a \code{spikeTrain} object or a numeric vector which can
  be coerced to such an object or \code{NULL} (default). The right
  channel of the resulting file if specified and not \code{NULL}.}
  \item{filename}{see \code{\link[sound]{saveSample}}.}
  \item{leftChFreq}{the frequency of the sine function convolved with
    \code{leftCh}.}
  \item{rightChFreq}{the frequency of the sine function convolved with
    \code{rightCh}.}
  \item{rate}{see \code{\link[sound]{as.Sample}}.}
  \item{bits}{see \code{\link[sound]{as.Sample}}.}
  \item{pan}{see \code{\link[sound]{stereo}}.}
  \item{overwrite}{see \code{\link[sound]{saveSample}}.}
}
\details{
  The \code{spikeTrain} object(s) of \code{leftCh} and \code{rightCh}
  are viewed as sequence of Dirac delta function and are convovled with
  a single period of a sine function of a given frequency.
}
\value{
  Nothing is returned, the function is used for its side effect: a
  \code{wav} file is created.
}
\author{Christophe Pouzat \email{christophe.pouzat@gmail.com}}
\note{
  You have to install the \code{sound} package to use these functions. 
}
\seealso{
  \code{\link{as.spikeTrain}},
  \code{\link[sound]{as.Sample}},
  \code{\link[sound]{stereo}},
  \code{\link[sound]{saveSample}}
}
\examples{
\dontrun{
## load spontaneous data of 4 putative projection neurons
## simultaneously recorded from the cockroach (Periplaneta
## americana) antennal lobe
data(CAL1S)
## convert data into spikeTrain objects
CAL1S <- lapply(CAL1S,as.spikeTrain)
## write train of neuron 1 to disk
trainWAV(CAL1S[[1]],,"n1spont.wav")
## write trains of neuron 1 and 2 to disk
trainWAV(CAL1S[[1]],CAL1S[[2]],"n1n2spont.wav")
}
}
\keyword{ts}
