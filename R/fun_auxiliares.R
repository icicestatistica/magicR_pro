

pvetor <- function(vetor){
  pvalores <- c()
  for (i in 1:length(vetor))
    pvalores <- c(pvalores,pvalor(vetor[i]))
  return(pvalores)}


format_real <- function(valor) {
   res=format(round(valor,2),nsmall = 2, decimal.mark = ",", big.mark = ".")
return(res)}

caixadeselecao = function (vetor, sep = ",", opcoes = "auto")
{
    vetor = unlist(vetor)
    if (opcoes[1] == "auto")
        opcoes = unique(gdata::trim(unlist(stringr::str_split(vetor, sep))))
    opcoes = opcoes[is.na(opcoes) == F]
    matrix <- matrix(c(rep("", length(opcoes) * length(vetor))),
        ncol = length(opcoes))
    for (i in 1:length(opcoes))
      for (j in 1:length(vetor))
        if (is.na(vetor[j]) == T) matrix[j, i] = NA else {
            if (opcoes[i] %in% gdata::trim(stringr::str_split(vetor[j],sep)[[1]]))  matrix[j, i] = "Sim"  else matrix[j, i] = "Não"}
    m <- data.frame(matrix)
    colnames(m) <- opcoes
    m <- m[, names(m) != "NA"]
    return(m)
}

nivcatsame = function(niveisoriginal){
   possi = unique(unlist(str_split(paste0(eval(parse(text=niveisoriginal)),sep=", ",collapse=""),", ")))
   possi = possi[which(possi!="")]
   niveiscerto = vec_to_string(possi)
   return(niveiscerto)}


arrumacaixadeselecao = function(cx,nomesini,nomesagrup){

names(cx) = nomesini
novascats = names(table(nomesagrup))

cx_novo = matrix(rep("",length(novascats)*dim(cx)[1]),ncol=length(novascats))

for (i in 1:length(novascats)){
  id = which(nomesagrup==novascats[i])
  df = data.frame(cx[,id])
  cx_novo[,i]=apply(df,1,function(x) ifelse(sum(x=="Sim",na.rm=T)>0,"Sim","Não"))
}
cx_novo = data.frame(cx_novo)
names(cx_novo)=novascats

vec_novo = apply(cx_novo,1,function(x) paste0(novascats[x=="Sim"],collapse=",")) %>% unlist() %>% unname()
vec_novo[vec_novo==""]=NA
return(list("cx_novo"=cx_novo,"vec_novo"=vec_novo))}




cont_analises <- function(nome,a){
df = data.frame("Sessão"=rep(nome,dim(a$testes)[1]),a$testes)
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

turn_into_factor=function(vec,niveis_str){
  return(unname(factor(unlist(vec),levels=eval(parse(text=niveis_str)))))}

Transformar = function(vec,original,novo){
a=factor(vec,levels=original)
levels(a)=novo
return(a)}

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))}

arruma_tabela_repetidos = function(vec) {
  fica = unlist(lapply(1:length(unique(vec)),function(x) min(which(str_detect(vec, fixed(unique(vec)[x]))==T))))
  vec[-fica]=""
  return(vec)}


rodape_tabela = function(nome,testes){

ana = unique(testes$tipo)
ana = factor(ana,levels=c("numeric","ordinal","factor","catsame","t","mw","aov1","kw","cc_e","cc_q","correl_s","correl_p", "wilc","fried","mcnem","qcoch"))

indicador = table(ana)>0

cap = c("Q1-Q3 = Primeiro e terceiro quartis; DP=desvio padrão","","",
  "IC95% = Intervalo de 95% de confiança para proporção",
  "p-valores marcados pela letra 'c' indicam realização do Teste-t e tamanho de efeito d de cohen",
  "p-valores marcados pela letra 'd' indicam realização do teste de Mann-Whitney e tamanho de efeito r",
  "p-valores marcados pela letra 'e' indicam realização do teste ANOVA e tamanho de efeito eta",
  "p-valores marcados pela letra 'f' indicam realização do teste Kruskall-Wallis e tamanho de efeito eta",
  "p-valores marcados pela letra 'b' indicam realização do teste Exato de Fisher",
  "p-valores marcados pela letra 'a' indicam realização do teste Qui-quadrado, seguido do tamanho de efeito V de cramer",
  "",
  "",
  "p-valores marcados pela letra 'j' indicam realização do teste Wilcoxon",
  "p-valores marcados pela letra 'i' indicam realização do teste de Friedman",
  "p-valores marcados pela letra 'k' indicam realização do teste de McNemar",
  "p-valores marcados pela letra 'l' indicam realização do teste Q de Cochran")

  capt = paste0(cap[indicador], collapse="; ")

if (sum(indicador[-c(1:4)])>0) capt = paste0(capt, "; \\* significante a 5%; \\*\\* significante a 1%; \\*\\*\\* significante a 0.1%")

capt = paste0(nome,"; ",capt,"\n")

return(capt)}


get_niveis = function(vec) {return(vec_to_string(names(table(vec))))}

###################

preencher = function(vec,vazios=""){

if(is.na(vazios)) {vec[is.na(vec)]=""; vazios=""}

id_vaz = which(vec==vazios)
id_nvaz = which(vec!=vazios)

mudar = sapply(id_vaz, function(x) max(id_nvaz[id_nvaz<x]))

vec_novo = vec
vec_novo[id_vaz] = vec[mudar]

return(vec_novo)}
