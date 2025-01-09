get_auxiliar <- function(dados){

  auxiliar <- data.frame(linha = 1:(dim(dados)[2]),nomes = names(dados),tipo=unlist(sapply(lapply(dados, class),"[[",1)))
  auxiliar[auxiliar$tipo=="character" | auxiliar$tipo=="logical",3] <- "factor"
  auxiliar[auxiliar$tipo=="integer",3] <- "numeric"
  n_cate = unlist(unname(apply(dados,2,function(x) length(table(x)))))
  auxiliar[auxiliar$tipo=="numeric" & n_cate<7,3] <- "factor"
  n_cate[auxiliar$tipo=="numeric"]=NA                             

  ntotal=dim(dados)[2]
  niveis=rep("",ntotal)
  n_porcol=apply(is.na(dados)==F,2,sum)
  for (i in 1:ntotal){
    if (auxiliar$tipo[i]=="factor" | auxiliar$tipo[i]=="ordinal"){
      niv <- c("c(")
      nomes=names(table(dados[,i]))
      n=length(nomes)
      if (n>1) for (j in 1:(n-1)) {niv <- c(niv,paste('"',nomes[j],'",',sep=""))}
      niveis[i] <- paste(c(niv,'"',nomes[n],'")'),collapse="",sep="")
      }
  }

  auxiliar <- data.frame(auxiliar,niveis,"n"=n_porcol,"n_cat"=n_cate)
      a=auxiliar[auxiliar$n_cat > 20 & is.na(auxiliar$n_cat) == F,]$tipo
    if(length(a)>0) auxiliar[auxiliar$n_cat > 20 & is.na(auxiliar$n_cat) == F,]$tipo = "texto"
  row.names(auxiliar) <- 1:length(auxiliar$linha)
  return(auxiliar)}



arruma_dados_auxiliar = function(dados,auxiliar){
  dadosnovo=dados
for (i in 1:dim(auxiliar)[1]){
  if(is.na(auxiliar$n_cat[i])==F) dadosnovo[,i]=Transformar(unlist(dados[,i]),eval(parse(text=auxiliar$niveis[i])),eval(parse(text=unlist(auxiliar[i,5]))))
  
  if(auxiliar$tipo[i]=="numeric") dadosnovo[,i] = as.numeric(as.character(unlist(dadosnovo[,i])))}
  
auxnovo = get_auxiliar(dadosnovo)
auxnovo$tipo = auxiliar$tipo
  
return(list("dadosnovo"=dadosnovo,"auxnovo"=auxnovo))  
}

## Ainda falta o catsamelevels
