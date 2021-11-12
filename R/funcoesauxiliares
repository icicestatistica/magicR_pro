# Printar um certo vetor separado por ',' e espaço, com último termo separado por "e"
printvetor=function(vetor){
size=length(vetor)
print=paste(paste(vetor[1:(size-1)],collapse=", ")," e ",vetor[size],collapse=NULL)
return(print)}

# Formatação para Reais
format_real <- function(valor) {
   res=format(round(valor,2),nsmall = 2, decimal.mark = ",", big.mark = ".")
return(res)}

#gera matriz de respostas sim e não por caixa
caixadeselecao <- function(vetor){
  opcoes = unique(trim(unlist(str_split(vetor,","))))
  matrix <- matrix(c(rep("",length(opcoes)*length(vetor))),ncol=length(opcoes))
  for (i in 1:length(opcoes))
    for (j in 1:length(vetor))
      if(is.na(vetor[j])==T) matrix[j,i]=NA else
        if(str_detect(vetor[j],fixed(opcoes[i]))==T) matrix[j,i]="Sim" else matrix[j,i]="Não"

  m <- data.frame(matrix)
  colnames(m)<- opcoes
  m <- m[,names(m)!="NA"]
  return(m)}
