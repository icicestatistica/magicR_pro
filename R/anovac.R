anovac <- function(continua,categorica,nomecont,nomecat,niveis,dig,respcol,excluirtotal,cor,idioma){
if(respcol==T) ref=nomecat else ref=nomecont
tabela=NULL
  
continua=unlist(continua)
categorica=unlist(categorica)
categorica <- factor(categorica,levels=niveis)

d <- data.frame(continua,categorica)
names(d) <- c("resp","fator")

homog = leveneTest(resp ~ fator, data=d)

if(homog$`Pr(>F)`[1]<0.05) {
  corr=" com correção de Welch para variâncias diferentes"
  homogtext=c(" A correção de Welch foi utilizada em decorrência do teste de Levene, que com p-valor menor que 0.05 (p=",pvalor(homog$`Pr(>F)`[1]),") rejeitou a hipótese de igualdade de variâncias. ");
  whiteadj=T;
  etatext="Por causa da correção, não computa-se a estatística de tamanho de efeito $\\eta^2$."} else {
    corr="";
    homogtext=c("Não foi necessário utilizar a correção de Welch, uma vez que o teste de Levene, com p-valor maior que 0.05 (p=",pvalor(homog$`Pr(>F)`[1]),") não rejeitou a hipótese de igualdade de variâncias.");
    whiteadj=F}

a=anova_test(d,resp~fator,white.adjust = whiteadj,type=3)
res = residuals((aov(lm(data=d,resp~fator))))
pv=a$p
sig=ifelse(pv>0.05," NÃO"," ")
if(is.null(a$ges)) eta = "-" else {eta=round(a$ges,(dig+1));
tamanef = ifelse(eta<0.01,"insignificante. ",ifelse(eta<0.06,"pequeno. ",ifelse(eta<0.14,"médio. ","grande. ")))
etatext=c("O tamanho de efeito $\\eta^2$ = ",eta," indica ",100*eta,"% de variabilidade de ",nomecont," explicada por ",nomecat,", o que Cohen(1988) classificou como um efeito ",tamanef)}

#suposições

shapresid=shapiro.test(res)
dassump=c()
for (i in niveis){
  obs=d$resp[d$fator==i]
  if(length(obs)<3) dassump=c(dassump,i,"","") else {shap=shapiro.test(obs); dassump=c(dassump,i,shap$statistic,shap$p.value)}}
dassump <- data.frame(matrix(dassump,ncol=3,byrow=T))
dassump <- rbind(dassump,c("Resíduos",shapresid$statistic,shapresid$p.value))

supos= ifelse(min(as.numeric(dassump$X3[dassump$X3!=""]))>0.05,T,F)

dassumpc=dassump[-dim(dassump)[1],]
tnaocalc=dassumpc$X3==""
tcumpre=tnaocalc==F & dassumpc$X3>0.05
tnaocumpre=tnaocalc==F & tcumpre==F
supg=c(paste0(dassump$X1[dassumpc$X2!=""]," - W=",round(as.numeric(dassump$X2[dassumpc$X2!=""]),dig),", \\*p\\*=",pvetor(as.numeric(dassump$X3[dassumpc$X2!=""]))))


printsup <- function(tipo,textotipo){
  return(ifelse(sum(tipo)==1,paste0("O grupo ",printvetor(dassumpc$X1[tipo]),textotipo[1]),ifelse(sum(tipo)>1,paste0("os grupos ",printvetor(dassumpc$X1[tipo]),textotipo[2]),"")))
}

textosupres = ifelse(as.numeric(dassump[dim(dassump)[1],3])>0.05, "não foi rejeitada pelo mesmo teste ","foi rejeitada pelo mesmo teste ")
vali = ifelse(supos," Consideramos, então, o teste válido e bem aplicado, conferindo confiança aos resultados."," Com este cenário, concluímos que a ANOVA não é o melhor teste a ser aplicado. Sugerimos a realização do teste não paramétrico de Kruskall-Wallis.")

textotipo=data.frame(c(", com p-valor maior que 0.05 no teste de shapiro-wilk, cumpre a suposição de normalidade. ",", com p-valor maior que 0.05 no teste de shapiro-wilk, cumprem a suposição de normalidade. "),c(", com p-valor menor que 0.05 no teste de shapiro-wilk, NÃO cumpre a suposição de normalidade. ",", com p-valor menor que 0.05 no teste de shapiro-wilk, NÃO cumprem a suposição de normalidade. "),c(" não apresentou dados suficientes para realização do teste. "," não apresentaram dados suficientes para realização do teste. "))

anasup=paste(" Quanto às suposições dos testes, temos o seguinte cenário: ",printsup(tcumpre,textotipo[,1]),printsup(tnaocumpre,textotipo[,2]),printsup(tnaocalc,textotipo[,3])," ",paste("(",paste(supg[-length(supg)],collapse="; "),")",sep=""),". Verificamos também a normalidade dos resíduos do ajuste, que ",textosupres,paste("(",paste(supg[length(supg)],collapse="; "),").",sep=""),vali,sep="")

texto=paste(c(" * **",ref,":** No total, ",dim(na.omit(d))[1]," linhas apresentaram dados completos sobre ",nomecat," e ",nomecont,". A análise foi feita a partir de uma ANOVA de uma via",corr,", que ",sig,"rejeitou a ausência de efeito de ",nomecat," em ",nomecont," (F(",a$DFn,",",round(a$DFd,dig),") = ",round(a$F,dig),", p = ",pvalor(pv),", $\\eta^2$ = ",eta,"). ",etatext,homogtext,anasup), collapse="")

p=paste0(pvalor(pv),"e ($\\eta^2$=",eta,")")

#post hoc

if(pv<0.05) {tex=c("As médias, em ordem crescente foram:")

ordem <- d %>% group_by(fator) %>% 
  get_summary_stats(resp, type = "mean_sd")

if(is.na(ordem[dim(ordem)[1],1])) ordem = ordem[-dim(ordem)[1],]

ord = c(ordem[order(ordem$mean),1])$fator

fit <- aov(data=d,resp ~ fator)
  
t=TukeyHSD(fit)$fator

tabela<- data.frame("Comparação"=row.names(t),"Diferença"=round(t[,1],1), 
               "IC 95%"=paste0("(",round(t[,2],1),", ",round(t[,3],1),")"),
               "P-valor"=pvetor(t[,4]))
difs = matrix(unlist(str_split(row.names(t),"-")),ncol=2,byrow=T)

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
        if(r[j]=="Menor") tex=c(tex,c("  + \"",difs[j,2], "\" é maior que \"",difs[j,1],"\";"),"\n")
        if(r[j]=="Maior") tex=c(tex,c("  + \"",difs[j,1], "\" é maior que \"",difs[j,2],"\";"),"\n")}
    tex=c(tex, "\n  Podemos verificar esses resultados na seguinte tabela:")}

print <- c()
for (i in ord){
  if (i==ord[length(ord)]) print=c(print," ",i," (média=",round(mean(d$resp[d$fator==i], na.rm=T),2),", DP=",round(sd(d$resp[d$fator==i], na.rm=T),2),").")
    else print=c(print," ",i," (média=",round(mean(d$resp[d$fator==i], na.rm=T),2),", DP=",round(sd(d$resp[d$fator==i], na.rm=T),2),"),")}

texto <- c(texto,print,tex,"\n")
}

res=desc_bi_cont(d$resp,d$fator,F,respcol,F,dig)
tot=dim(na.omit(d))[1]
  
if(excluirtotal==T) res=res[-1,]
  
res <- cbind(rbind(c(paste("**",ref,"** (", tot,")",sep=""),rep("",dim(res)[2])),res),"p-valor"=c("",p,rep("",dim(res)[1]-1)))

  if(is.null(tabela)==TRUE) texto=paste(texto,collapse="") else texto=list(paste(texto,collapse=""),tabela)
    
a1=a$DFn  ; a2=round(a$DFd,dig) ; a3=ifelse(pv<0.001,"<0.001",paste("=",round(pv,3))) ; a4=round(a$F,dig)
textograf <- substitute(paste("ANOVA one-way F(",a1,",",a2,") = ",a4,", p",a3,collapse=""),list(a1=a1,a2=a2,a3=a3,a4=a4))
grafico=grafico_comp_bar(d$resp,nomecont,d$fator,nomecat,cor=cor,teste=textograf,dig=dig, idioma=idioma)

return(list("sup"=supos,
            "result"=res,
            "texto"=texto,
             "grafico"=grafico))}
