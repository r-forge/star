slockedTrain <- function(lockedTrain,
                         bw=0.001,
                         bs="cr",
                         k=100,
                         ...)
### smooths a lockedTrain object
### uses the gam model with the Poisson family after
### binning the object.
{
  ## check that lockedTrain is a "lockedTrain" object
  if (!inherits(lockedTrain,"lockedTrain"))
    stop("lockedTrain should be a \"lockedTrain\" object.")
  ## make a histogram without generating a plot
  lTh <- hist(lockedTrain,bw=bw,plot=FALSE)
  Time <- lTh$mids
  nRef <- lTh$nRef
  testFreq <- lTh$testFreq
  bwV <- diff(lTh$breaks)
  Count <- lTh$density*nRef*bwV
  PoissonFit <- gam(Count ~ s(Time,k=k,bs=bs),family=poisson(),...)

  result <- list(gamFit=PoissonFit,
                 Time=Time,
                 nRef=nRef,
                 testFreq=testFreq,
                 bwV=bwV,
                 CCH=lTh$CCH,
                 call=match.call())
  
  class(result) <- "slockedTrain"
  result
}

gamObj.slockedTrain <- function(object, ...) {
  object$gamFit
}

summary.slockedTrain <- function(object, ...) {
  summary(gamObj(object))
}

print.slockedTrain <- function(x, ...) {
  print(gamObj(x))
}

plot.slockedTrain <- function(x,
                              xlab, ylab, main, xlim, ylim,
                              col,lwd,
                              ...) {

  if (missing(xlab)) 
    xlab <- "Time (s)"
  if (missing(ylab)) 
    ylab <- "PDF of a Test Neuron Spike (1/s)"
  if (missing(main)) {
    nameList <- deparse(x$call[["lockedTrain"]])
    if (x$CCH) main <- paste(nameList, "smoothed cross-intensity")
    else main <- paste(nameList, "smoothed auto-intensity")
  }

  gamFitP <- predict(gamObj(x),type="response",se.fit=TRUE)
  Y <- gamFitP$fit/x$nRef/x$bwV
  Yp <- (gamFitP$fit+1.96*gamFitP$se.fit)/x$nRef/x$bwV
  Ym <- (gamFitP$fit-1.96*gamFitP$se.fit)/x$nRef/x$bwV
  X <- x$Time

  if (missing(xlim)) xlim <- range(X)
  if (missing(ylim)) ylim <- c(min(c(Ym,x$testFreq)),max(c(Yp,x$testFreq)))
  if (missing(col)) col <- 2
  if (missing(lwd)) lwd <- 2
  
  plot(X,Y,type="n",
       xlab=xlab,ylab=ylab,
       xlim=xlim,ylim=ylim,
       main=main,
       ...)
  abline(h=x$testFreq)
  abline(v=0)
  lines(X,Yp,lty=2)
  lines(X,Ym,lty=2)
  lines(X,Y,col=col,lwd=lwd)
  
}
