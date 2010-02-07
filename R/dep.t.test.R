dep.t.test <- function(formula, data, block, sig.level=.05, digits=3){
  xx <- summaryBy(formula, data=data, FUN=sumfun)
  indvar <- unlist(strsplit(as.character(formula), " ")[3])
  depvar <- unlist(strsplit(as.character(formula), " ")[2])
  datawide <- reshape(data, direction="wide", idvar=block, timevar=indvar)    
  corr <- cor(datawide[, setdiff(names(datawide), block)])[1,2]
  output <- dep.t.test.second(m=xx[,2], sd=xx[,3], n=xx[1,4], corr=corr, sig.level=sig.level,digits=digits)
  return(output) 
}
