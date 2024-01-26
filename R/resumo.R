resumo_inner = function(anali){
resumo = c()
if(dim(anali)[1]==1) resumo = paste0("\n - ",anali$resumo) else {
if(length(table(anali$Nome1))>1) {nomes_res=anali$Nome1; ref=unique(anali$Nome2)} else {nomes_res = anali$Nome2 ; ref=unique(anali$Nome1)}

if(sum(anali$sig_ou_não==T)>0) {
  resumo = c(resumo,paste0("\n - ",anali[anali$sig_ou_não==T,]$resumo))
}
if(sum(anali$sig_ou_não==T)==0) {resumo = c(resumo, paste0("\n - Nenhuma das variáveis estudadas (",paste0(printvetor(nomes_res[anali$sig_ou_não==F]), collapse="",sep=""),") tiveram associações ou correlações estatisticamente significativas com ",ref,". \n"))} else {
if(sum(anali$sig_ou_não==F)>0) {
  resumo = c(resumo, paste0("\n - As demais variáveis (",paste0(printvetor(nomes_res[anali$sig_ou_não==F]), collapse="",sep=""),") não tiveram associações ou correlações estatisticamente significativas. \n"))}
}}
if(sum(anali$sig_ou_não=="-")>0) {resumo = paste0("\n - ",anali$resumo)}
return(resumo)}

resumo_geral = function(analises){

res=c()
sessoes = unique(analises$Sessão)
n_sess = length(sessoes)
for (i in 1:n_sess)
  res = c(res,paste0(paste0("\n\n**",sessoes[i],"**:\n"),paste0(resumo_inner(analises[analises$Sessão==sessoes[i],]), collapse="")))

return(paste0(res))}
