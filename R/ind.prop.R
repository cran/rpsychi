ind.prop <- function(formula, data, sig.level=.05, digits=3, lev.prop=1){
  indvar <- unlist(strsplit(as.character(formula), " ")[3])
  depvar <- unlist(strsplit(as.character(formula), " ")[2])
  tab <- table(data[, indvar], data[, depvar])
  output <- ind.prop.second(x=tab[,lev.prop], n=rowSums(tab), sig.level=sig.level, digits=digits)
  return(output) 
}
