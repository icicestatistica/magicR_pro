get_auxiliar <- function(dados){

  auxiliar <- data.frame(linha = 1:(dim(dados)[2]),nomes = names(dados),tipo=unlist(lapply(dados,class)))
  auxiliar[auxiliar$tipo=="character" | auxiliar$tipo=="logical",3] <- "factor"


  ntotal=dim(dados)[2]
  niveis=rep("",ntotal)
  for (i in 1:ntotal){
    if (auxiliar$tipo[i]=="factor"){
      niv <- c("c(")
      nomes=names(table(dados[,i]))
      n=length(nomes)
      if (n>1) for (j in 1:(n-1)) {niv <- c(niv,paste("'",nomes[j],"',",sep=""))}
      niveis[i] <- paste(c(niv,"'",nomes[n],"')"),collapse="",sep="")}}

  auxiliar <- data.frame(auxiliar,niveis)
  row.names(auxiliar) <- 1:length(auxiliar$linha)
  return(auxiliar)}
