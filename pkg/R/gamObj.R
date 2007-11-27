gamObj <- function(object,
                   ...) {

  UseMethod("gamObj")
}

gamObj.spsth <- function(object,...) {

  evalq(PoissonF, env=environment(object$lambdaFct))

}

gamObj.slockedTrain <- function(object, ...) {
  object$gamFit
}
