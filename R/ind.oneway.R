ind.oneway <- function(formula, data, contr=NULL, sig.level=.05, digits=3){
  x <- summaryBy(formula, data=data, FUN=sumfun)
  output <- ind.oneway.second(m=x[,2], sd=x[,3], n=x[,4], contr=contr, sig.level=sig.level,digits=digits)
  return(output)
}