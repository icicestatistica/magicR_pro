printvetor <- function(vetor){
size=length(vetor)
print=paste(paste(vetor[1:(size-1)],collapse=", ")," e ",vetor[size],collapse=NULL)
return(print)}

format_real <- function(valor) {
   res=format(round(valor,2),nsmall = 2, decimal.mark = ",", big.mark = ".")
return(res)}

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

vetor_comsep <- function(vec,corte){
mat = str_split(vec," ")
res=c()
for (i in 1:length(vec)){
  if (length(mat[[i]]) <= corte) res <- c(res, paste(mat[[i]],collapse=" ",sep="")) else {
    linhas=ceiling(length(mat[[i]])/corte)
    if(length(mat[[i]])%%corte==0) ultima = corte else ultima=length(mat[[i]])%%corte
    aux=c()
    if (linhas==2) {aux <- c(aux,paste(mat[[i]][1:corte],collapse=" ",sep=""))}
    if (linhas>2){
      for (j in 0:(linhas-2)) aux <- c(aux,paste(mat[[i]][j*corte+(1:corte)],collapse=" ",sep=""))}
    aux <- c(aux, paste(mat[[i]][((linhas-1)*corte)+(1:ultima)],collapse=" ",sep=""))
  res=c(res, paste(aux, collapse=" \n "))}}
return(res)}

relatorio <- function(a){
for (i in 2:length(a)){
if(class(a[[i]])=="data.frame") print(kable(a[[i]], row.names=F)) else
  if(class(a[[i]])=="list") for (j in 1:length(a[[i]])) cat(a[[i]][[j]],sep="\n") else 
    print(a[[i]])}}

   
cont_analises <- function(nome,vetortestes){
df = data.frame("Nome"=nome,"analises"=paste("list(c('desc'=",vetortestes[1],",'catsame'=",vetortestes[2],",'t'=",vetortestes[3],
",'mw'=",vetortestes[4],",'aov1'=",vetortestes[5],",'kw'=",vetortestes[6],",'correl'=",vetortestes[7],",'cc'=",vetortestes[8],
",'t_par'=",vetortestes[9],",'wilc'=",vetortestes[10],",'aovmr'=",vetortestes[11],",'fried'=",vetortestes[12],",'mcnem'=",vetortestes[13],",'qcoch'=",vetortestes[14],"))"))
return(df)}
