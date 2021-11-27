anovac <- function(continua,categorica,nomecont,nomecat,niveis,dig,respcol,excluirtotal){

categorica <- factor(categorica,levels=niveis)

d <- data.frame(continua,categorica)
names(d) <- c("resp","fator")

fit <- aov(resp ~ fator, data=d)
eta=round(eta_squared(fit),2)

p=paste0(pvalor(summary(fit)[[1]][[1,"Pr(>F)"]],3),"e (eta=",eta,")")

if (summary(fit)[[1]][[1,"Pr(>F)"]] > 0.05) {tabela=NULL 
texto=c("* ",nome,": Não encontramos evidência estatística pela anova one-way para rejeitar a hipótese de igualdade entre as médias dos grupos. \n")}
else {
  texto=c("* ",nome,": O teste one-way anova encontrou evidências para rejeitar a igualdade entre as médias dos grupos. As médias, em ordem crescente foram:")

  
ordem <- d %>% group_by(fator) %>% 
  get_summary_stats(resp, type = "mean_sd")

if(is.na(ordem[dim(ordem)[1],1])) ordem = ordem[-dim(ordem)[1],]

ord = c(ordem[order(ordem$mean),1])$fator
  
t=TukeyHSD(fit)$fator

tabela<- data.frame("Diferença"=round(t[,1],1), 
               "IC 95%"=paste0("(",round(t[,2],1),", ",round(t[,3],1),")"),
               "P-valor"=t[,4])
difs = matrix(unlist(str_split(row.names(tabela),"-")),ncol=2,byrow=T)

ncomps=dim(tabela)[1]

r=rep("",ncomps)
for (i in 1:ncomps){
if(tabela$P.valor[i]>0.05) r[i] = "Não" else
  if(tabela$Diferença[i]>0) r[i]="Maior" else r[i]="Menor"}

resumo = factor(r, levels=c("Maior","Menor","Não"))

tex <- c()

if(prop.table(table(resumo))[3]==1) tex=c(tex,"Apesar de termos encontrado diferença pela anova, ao realizar o teste de tukey de comparação par-a-par, nenhuma das comparações de grupos teve diferença estatisticamente significativa.  \n") else
  if(sum(prop.table(table(resumo))[1:2])==1) tex=c(tex,"O teste de comparações múltiplas de tukey apontou diferenças entre todos os grupos estudados") else {
    tex=c(tex," O teste de comparações múltiplas de tukey apontou as seguintes diferenças:  \n")
      for (j in which(resumo!="Não")){
        if(r[j]=="Menor") tex=c(tex,c("+ ",difs[j,2], " é maior que ",difs[j,1],"  \n"))
        if(r[j]=="Maior") tex=c(tex,c("+ ",difs[j,1], " é maior que ",difs[j,2],"  \n"))}
    tex=c(tex, "Podemos verificar esses resultados na seguinte tabela:")}

print <- c()
for (i in ord){
  if (i==ord[length(ord)]) print=c(print," ",i," (média=",round(mean(d$resp[d$fator==i], na.rm=T),2),", DP=",round(sd(d$resp[d$fator==i], na.rm=T),2),").")
    else print=c(print," ",i," (média=",round(mean(d$resp[d$fator==i], na.rm=T),2),", DP=",round(sd(d$resp[d$fator==i], na.rm=T),2),"),")}

texto <- c(texto,print,tex,"\n")
}

return(list("pvalor"=p,"tabela"=tabela,"texto"=texto))}
