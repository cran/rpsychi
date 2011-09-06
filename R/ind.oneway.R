ind.oneway <- function(formula, data, contr=NULL, sig.level=.05, digits=3){
  x <- aggregate(formula, data=data, FUN=sumfun)$y
  output <- ind.oneway.second(m=x[,1], sd=x[,2], n=x[,3], contr=contr, sig.level=sig.level,digits=digits)
  return(output)
}
