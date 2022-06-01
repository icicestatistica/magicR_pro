printvetor <- function(vetor, idioma="PT"){
size=length(vetor)
conec = ifelse(idioma=="PT","' e '","' and '")
if(size==1) print=paste0("'",vetor[1],"'") else print=paste(paste("'",vetor[1:(size-1)],collapse="', ",sep=""),conec,vetor[size],"'",collapse=NULL,sep="")
return(print)}

format_real <- function(valor) {
   res=format(round(valor,2),nsmall = 2, decimal.mark = ",", big.mark = ".")
return(res)}

caixadeselecao <- function(vetor,sep){
  vetor=unlist(vetor)
  opcoes = unique(trim(unlist(str_split(vetor,sep))))
  matrix <- matrix(c(rep("",length(opcoes)*length(vetor))),ncol=length(opcoes))
  for (i in 1:length(opcoes))
    for (j in 1:length(vetor))
      if(is.na(vetor[j])==T) matrix[j,i]=NA else
        if(str_detect(vetor[j],fixed(opcoes[i]))==T) matrix[j,i]="Sim" else matrix[j,i]="Não"

  m <- data.frame(matrix)
  colnames(m)<- opcoes
  m <- m[,names(m)!="NA"]
  return(m)}
   
nivcatsame = function(niveisoriginal){
   possi = unique(unlist(str_split(paste0(eval(parse(text=auxiliar$niveis[25])),sep=", ",collapse=""),", ")))
   possi = possi[which(possi!="")]
   niveiscerto = vec_to_string(possi)
   return(niveiscerto)}

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
   
vetor_comsep_c <- function(vec,corteinicial){
  res=c()
  for (vetor in vec){
  a=T
  espacos = c(str_locate_all(vetor," ")[[1]][,1],nchar(vetor)+1)
  inicio=1
  corte=corteinicial
  fim= ifelse(espacos[1]>corte,espacos[1]-1,espacos[max(which(espacos-1<=corte))]-1)
  newvec <- c()
  while (a){
    newvec = c(newvec,str_sub(vetor,inicio,fim))
    inicio = fim + 2
      if(inicio>nchar(vetor)) a=F
    corte=inicio+corteinicial
    fim=espacos[max(which(espacos-1<=corte))]-1
    if(fim<inicio) {fim=espacos[max(which(espacos-1<=corte))+1]-1}
  }
  newvec = paste0(newvec, collapse="\n")
  res = c(res,newvec)}
return(res)}

#vetor_comsep_c(names(dados),50)

relatorio <- function(a,pularprimeiro){
if (pularprimeiro==T) comeco=2 else comeco=1
for (i in comeco:length(a)){
   if(is.null(a[[i]])==T) tantofaz=0 else {
if(class(a[[i]])=="data.frame") print(kable(a[[i]], row.names=F)) else
  if(class(a[[i]])=="list") relatorio(a[[i]],F) else
      if(class(a[[i]])=="character") cat(a[[i]], sep="\n") else
        print(a[[i]])}}}

   
cont_analises <- function(nome,vetortestes,vars){
df = data.frame("Nome"=nome,"analises"=paste("list(c('desc'=",vetortestes[1],",'catsame'=",vetortestes[2],",'t'=",vetortestes[3],
",'mw'=",vetortestes[4],",'aov1'=",vetortestes[5],",'kw'=",vetortestes[6],",'correl'=",vetortestes[7],",'cc'=",vetortestes[8],
",'t_par'=",vetortestes[9],",'wilc'=",vetortestes[10],",'aovmr'=",vetortestes[11],",'fried'=",vetortestes[12],",'mcnem'=",vetortestes[13],",'qcoch'=",vetortestes[14],"))"),Variáveis = vars)
return(df)}
   
transpordf <- function(tab){

nova=data.frame(t(tab))
nova <- cbind(names(tab),nova)
names(nova)=c(names(tab)[1],tab[,1])
nova <- nova[-1,]
row.names(nova)<-NULL

return(nova)}

vec_to_string <- function(vec){
   str = paste0("c(",paste(paste("'",vec,"'",sep=""),collapse=","),")",collapse="")
   return(str)}
