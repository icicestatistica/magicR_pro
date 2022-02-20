pvalor <- function(p){
  if(is.numeric(p)==F) res="" else
    if(p>0.05) res = p=paste0("p=",formatC(round(p,3), format='f', digits=3) else
      if(p>0.01) res=paste0("p=",formatC(round(p,3), format='f', digits=3),"\\*") else
        if(p>=0.001) res=paste0("p=",formatC(round(p,3), format='f', digits=3),"\\*\\*") else res = "p<0.001\\*\\*\\*"
      return(res)}

pvetor <- function(vetor){
  pvalores <- c()
  for (i in 1:length(vetor))
    pvalores <- c(pvalores,pvalor(vetor[i]))
  return(pvalores)}
