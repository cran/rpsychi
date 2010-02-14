dep.oneway <- function(formula, data, block, contr=NULL, sig.level=.05, digits=3){
  xx <- summaryBy(formula, data=data, FUN=sumfun)
  n <- nlevels(factor(data[, block]))  
  indvar <- unlist(strsplit(as.character(formula), " ")[3])  
  datawide <- reshape(data, direction="wide", idvar=block, timevar=indvar)
  corr <- cor(datawide[, setdiff(names(datawide), block)])
  output <- dep.oneway.second(m=xx[,2], sd=xx[,3], n=n, corr=corr, contr=contr, sig.level=sig.level,digits=digits)
  return(output)
}
