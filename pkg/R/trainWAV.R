trainWAV <- function(leftCh,
                     rightCh=NULL,
                     filename,
                     leftChFreq=500,
                     rightChFreq=1000,
                     rate=10000,
                     bits=8,
                     pan=50,
                     overwrite=FALSE
                     ) {

  require(sound)
  ## check that leftCh is a spikeTrain object
  ## overwise try to coerce leftCh to a spikeTrain object
  if (!is.spikeTrain(leftCh)) leftCh <- as.spikeTrain(leftCh)
  ## if rightCh is neither NULL nor missing check
  ## that it is a spikeTrain object otherwise coerce it
  if (!is.null(rightCh) && !missing(rightCh)) {
    if (!is.spikeTrain(rightCh)) rightCh <- as.spikeTrain(rightCh)
    withRight <- TRUE
  } else {
    withRight <- FALSE
  }

  ## find out the necessary length of the sound vectors
  duration <- ceiling(leftCh[length(leftCh)])-floor(leftCh[1])
  if (withRight) {
    rightDuration <- ceiling(rightCh[length(rightCh)])-floor(rightCh[1])
    if ( rightDuration > duration) duration <- rightDuration
  }

  s <- numeric(duration*rate)
  sPeriod <- 1/leftChFreq
  idealS <- sin(seq(0,2*pi,pi/(sPeriod*rate)))
  s.l <- length(idealS)
  for (i in leftCh) s[(i*rate+1):(i*rate+s.l)] <- idealS
  s <- as.Sample(matrix(s,nrow=1),rate=rate,bits=bits)
  if (withRight) {
    r <- numeric(duration*rate)
    sPeriod <- 1/rightChFreq
    idealS <- sin(seq(0,2*pi,pi/(sPeriod*rate)))
    s.l <- length(idealS)
    for (i in rightCh) r[(i*rate+1):(i*rate+s.l)] <- idealS
    r <- as.Sample(matrix(r,nrow=1),rate=rate,bits=bits)
    s <- stereo(s,r,pan=pan)
  }
  saveSample(s,filename=filename,overwrite=overwrite)
  
}

