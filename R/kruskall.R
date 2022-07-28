kruskall <- function(resp,fator,nomeresp,nomefator,niveis='auto',dig=2,respcol=T,excluirtotal=T,cor="cyan4",ordenar=F, idioma="PT", ordinal=F,labels=T){
  
supos=T
  
if(respcol==T) ref=nomefator else ref=nomeresp
  
resp=unlist(resp)
fator=unlist(fator)
if(niveis=="auto") niveis = names(table(fator))
fator <- factor(fator,levels=niveis)

dad <- data.frame("continua"=as.numeric(resp),"categorica"=fator)
dad2= data.frame("continua"=resp,"categorica"=fator)
  
a <- kruskal.test(dad$continua, dad$categorica)
c=round(kruskal_effsize(dad, continua ~ categorica)$effsize,dig)
p=paste0(pvalor(a$p.value),"f ($\\eta^2$=",c,")")

if (kruskal.test(dad$continua, dad$categorica)$p.value > 0.05) {tabela=NULL
  texto=c("* **",ref,"**: Não encontramos com o teste de Kruskall Wallis evidência de diferença entre os grupos (","X2(",a$parameter,") =",round(a$statistic,dig),",p-valor=",pvalor(a$p.value),"). \n")}
else {

texto=c("* **",ref,"**: O teste de Kruskall-Wallis mostrou que há diferença entre os grupos (",paste("$\\chi^2$",collapse=NULL),"(",a$parameter,") =",round(a$statistic,dig),",p-valor=",pvalor(a$p.value),"). O post-hoc de Dunn mostrou que")

dunn <- dunn.test(dad$continua, dad$categorica,method = "bonferroni",kw=F,table=F,list=F)
b <- data.frame(dunn$comparisons,dunn$P.adjusted)

ordem <- dad %>% group_by(categorica) %>% 
  get_summary_stats(continua, type = "median_iqr")

ord = c(ordem[order(ordem$median),1])$categorica

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

tabela=data.frame("Comparações"=dunn$comparisons,"Estatística"=round(dunn$Z,dig),"p-valor"=pvetor(dunn$P),"p-valor ajustado"=pvetor(dunn$P.adjusted))
}
  
if(ordinal==F) res=desc_bi_cont(dad$continua,dad$categorica,F,respcol,F,dig) else 
   {if(respcol==F) res=desc_bi_cat(linha=dad2$continua,col=dad2$categorica,respcol=F) else res=desc_bi_cat(linha=dad2$categorica,col=dad2$continua,respcol=T)}


tot=dim(na.omit(dad))[1]
if(excluirtotal==T) res=res[-1,]
  
res <- cbind(rbind(c(paste("**",ref,"** (", tot,")",sep=""),rep("",dim(res)[2])),res),"p-valor"=c("",p,rep("",dim(res)[1]-1)))

if(is.null(tabela)==TRUE) texto=paste(texto,collapse="") else texto=list(paste(texto,collapse=""),tabela)
 
a1=a$parameter  ; a2=round(a$statistic,dig) ; a3=ifelse(a$p.value<0.001,"<0.001",round(a$p.value,3))
textograf <- substitute(paste("Kruskall-Wallis (",chi^2,"(",a1,") =",a2,",p=",a3,")",collapse=""),list(a1=a1,a2=a2,a3=a3))
if(ordinal==F) grafico = grafico_comp_box(dad$continua,nomeresp,dad$categorica,nomefator,cor=cor,textograf,dig,ordenar, idioma) else
grafico = grafico_catcat(dad2$categorica,nomefator,dad2$continua,nomeresp, cor=cor, textograf,idioma,labels=labels) + coord_flip()
  
return(list("sup"=supos,
            "result"=res,
            "texto"=texto,
             "grafico"=grafico))}
