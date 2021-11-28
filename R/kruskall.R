kruskall <- function(resp,fator,mudarordem,nome){

dad <- data.frame("continua"=resp,"categorica"=fator)
a <- kruskal.test(dad$continua, dad$categorica)
c=round(kruskal_effsize(dad, continua ~ categorica)$effsize,3)
p=paste0(pvalor(a$p.value,3),"f (eta2=",c,")")

if (kruskal.test(dad$continua, dad$categorica)$p.value > 0.05) {tabela=NULL
  texto=c(nome,": Não encontramos com o teste de Kruskall Wallis evidência de diferença entre os grupos (","X2(",a$parameter,") =",round(a$statistic,3),",p-valor=",pvalor(a$p.value,3),"). \n")}
else {

texto=c(nome,": O teste de Kruskall-Wallis mostrou que há diferença entre os grupos (","X2(",a$parameter,") =",round(a$statistic,3),",p-valor=",pvalor(a$p.value,3),"). O post-hoc de Dunn mostrou que")

dunn <- dunn.test(dad$continua, dad$categorica,method = "bonferroni",kw=F,table=F,list=F)
b <- data.frame(dunn$comparisons,dunn$P.adjusted)

ordem <- dad %>% group_by(categorica) %>% 
  get_summary_stats(continua, type = "median_iqr")

if (mudarordem=="Não") ord = c(ordem[order(ordem$median),1])$categorica else ord=mudarordem

d <- c(b$dunn.P.adjusted)
names(d) <- str_replace(b$dunn.comparisons, " - ","-")

mult = multcompLetters(d)

new <- rep("",dim(dad)[1])

for (i in 1:length(mult$Letters)){
new[dad$categorica==names(mult$Letters)[i]] <- mult$Letters[i]}

print <- c()
for (i in 1:length(mult$Letters)){
  print=c(print,"O grupo ",c(ordem[i,1])$categorica," (mediana = ",c(ordem[i,4])$median," e intervalo interquartil = ",ordem[i,5]$iqr,") ")}

texto <- c(texto,print,"\n")

tabela=data.frame("Comparações"=dunn$comparisons,"Estatística"=round(dunn$Z,2),"p-valor"=pvetor(dunn$P,3),"p-valor ajustado"=pvetor(dunn$P.adjusted,3))
}

return(list("pvalor"=p,"tabela"=tabela, "texto"=texto))}
