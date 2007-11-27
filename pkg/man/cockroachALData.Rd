\name{cockroachAlData}
\alias{CAL1S}
\alias{CAL1V}
\alias{CAL2S}
\alias{CAL2C}
\alias{e070528spont}
\alias{e070528citronellal}
\docType{data}
\title{Spike Trains of several Cockroach Antennal Lobe Neurons
  Recorded from Three Animals}
\description{
  Four (\code{CAL1S} and \code{CAL1V}), three (\code{CAL2S} and
  \code{CAL2C}) and four (\code{e070528spont} and \code{e070528citronellal}) Cockroach (\emph{Periplaneta americana}) antennal lobe neurons
  (putative projection neurons) were recorded simultaneously
  and extracellularly during spontaneous activity and odor (vanilin and citronellal)
  responses from three different animals. The data sets contain the sorted
  spike trains of the neurons. 
}
\usage{
data(CAL1S)
data(CAL1V)
data(CAL2S)
data(CAL2C)
data(e070528spont)
data(e070528citronellal)
}
\format{
  \code{CAL1S} is a named list with 4 components
  (\code{"neuron 1"}, \code{"neuron 2"}, \code{"neuron 3"},
  \code{"neuron 4"}). Each
  component contains the spike train (ie, action potentials occurrence
  times) of one neuron recorded during 30 s of spontaneous
  activity. \emph{Times are expressed in seconds}.

  \code{CAL1V} is a named list with 4 components
  (\code{"neuron 1"}, \code{"neuron 2"}, \code{"neuron 3"},
  \code{"neuron 4"}).
  Each component is a named list
  with 20 components: \code{"stim. 1"}, ..., \code{"stim. 20"}. Each
  sub-list contains the spike train of one neuron during 1 stimulation
  (odor puff) with vanilin. Each acquisition was 10 s long. The command
  to the odor delivery  valve was on between sec 4.49 and sec 4.99.

  \code{CAL2S} is a named list with 3 components
  (\code{"neuron 1"}, \code{"neuron 2"}, \code{"neuron 3"}). Each
  component contains the spike train (ie, action potentials occurrence
  times) of one neuron recorded during 1 mn of spontaneous
  activity. \emph{Times are expressed in seconds}.

  \code{CAL2C} is a named list with 3 components
  (\code{"neuron 1"}, \code{"neuron 2"}, \code{"neuron 3"}).
  Each component is a named list
  with 20 components: \code{"stim. 1"}, ..., \code{"stim. 20"}. Each
  sub-list contains the spike train of one neuron during 1 stimulation
  (odor puff) with vanilin. Each acquisition was 14 s long. The command
  to the odor delivery  valve was on between sec 5.87 and sec 6.37.

  \code{e070528spont} is a named list of  with 4 components
  (\code{"neuron 1"}, \code{"neuron 2"}, \code{"neuron 3"},
  \code{"neuron 4"}). Each
  component is a \code{spikeTrain} object (ie, action potentials occurrence
  times) of one neuron recorded during 60 s of spontaneous
  activity. \emph{Times are expressed in seconds}.

  \code{e070528citronellal} is a named list with 4 components
  (\code{"neuron 1"}, \code{"neuron 2"}, \code{"neuron 3"},
  \code{"neuron 4"}).
  Each component is a \code{repeatedTrain} object
  with 15 \code{spikeTrain} objects: \code{"stim. 1"}, ..., \code{"stim. 15"}. Each
  \code{spikeTrain} contains the spike train of one neuron during 1 stimulation
  (odor puff) with citronellal. Each acquisition was 13 s long. The command
  to the odor delivery  valve was on between sec 6.14 and sec 6.64.
}
\details{
  The data were recorded from neighboring sites on a \emph{NeuroNexus}
  (\url{http://neuronexustech.com/}) silicon probe. Sorting was done
  with \code{SpikeOMatic} with superposition resolution which can AND
  DOES lead to artifcats on cross-correlograms. 
}
\source{
  Recording and spike sorting performed by Antoine Chaffiol
  \email{antoine.chaffiol@univ-paris5.fr} at the Cerebral Physiology
  Lab, CNRS UMR 8118:
  \url{http://www.biomedicale.univ-paris5.fr/physcerv/physiologie_cerebrale.htm}. 
}
\references{
  \url{http://www.biomedicale.univ-paris5.fr/physcerv/C_Pouzat/Doc/ChaffiolEtAl_FENS2006.pdf}
}
\examples{
## load CAL1S data
data(CAL1S)
## convert the data into spikeTrain objects
CAL1S <- lapply(CAL1S,as.spikeTrain)
## look at the train of the 1sd neuron
CAL1S[["neuron 1"]]
## fit the 6 different renewal models to the 1st neuron spike train
compModels(CAL1S[["neuron 1"]])
## look at the ISI distribution with the fitted invgauss dist for
## this 1st neuron
isiHistFit(CAL1S[["neuron 1"]],model="invgauss")

## load CAL1V data
data(CAL1V)
## convert them to repeatedTrain objects
CAL1V <- lapply(CAL1V, as.repeatedTrain)
## look at the raster of the 1st neuron
CAL1V[["neuron 1"]]

## load e070528spont data
data(e070528spont)
## look at the spike train of the 1st neuron
e070528spont[["neuron 1"]]

## load e070528citronellal data
data(e070528citronellal)
## look at the raster of the 1st neuron
plot(e070528citronellal[["neuron 1"]],stim=c(6.14,6.64))

}
\keyword{datasets}
