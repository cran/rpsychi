ind.t.test <- function(formula, data, sig.level=.05, digits=3){
  x <- summaryBy(formula, data=data, FUN=sumfun)
  output <- ind.t.test.second(m=x[,2], sd=x[,3], n=x[,4], sig.level=sig.level,digits=digits)
  return(output)
}
