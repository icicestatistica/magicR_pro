testet <- function(continua,categorica,nomecont,nomecat,niveis,dig,respcol){
  
if(respcol==T) ref=nomecat else ref=nomecont

d <- data.frame("resp"=continua,"fator"=categorica)
names(d) <- c("resp","fator")

resp1=as.numeric(d$resp[d$fator==niveis[1]])
resp2=as.numeric(d$resp[d$fator==niveis[2]])

if(length(resp1)>3 & length(resp1)<5000) str1=shapiro.test(resp1) else str1="Não"
if(length(resp2)>3 & length(resp2)<5000) str2=shapiro.test(resp2) else str2="Não"

if(str1=="Não" || str2=="Não") sup=c(" A suposição de normalidade das amostras não pode ser verificada pelo teste de Shapiro-Wilk, uma vez que o tamanho de pelo menos uma das amostras foi menor que 3 ou maior que 5000. Por este motivo, recomendamos a utilização do teste não paramétrico de Mann-Whitney ao invés do tradicional teste-t") else{
if(str1$p.value>=0.05 & str2$p.value>=0.05) sup=c(" A suposição de normalidade das amostras foi verificada através do teste de Shapiro-Wilk, que com p-valor maior que 0.05 para as duas amostras não rejeitou a normalidade das distribuições (",niveis[1]," - W=",round(str1$statistic,2),", p-valor=",pvalor(str1$p.value),", ",niveis[2]," - W=",round(str2$statistic,dig),", p-valor=",pvalor(str2$p.value),"), atendendo a suposição do teste.")
if(str1$p.value>=0.05 & str2$p.value<0.05) sup=c(" A suposição de normalidade das amostras foi verificada através do teste de Shapiro-Wilk, que com p-valor menor que 0.05 rejeitou a normalidade da distribuição de ",niveis[2]," - W=",round(str2$statistic,dig),", p-valor=",pvalor(str2$p.value),", mas não de ",niveis[1]," - W=",round(str1$statistic,dig),", p-valor=",pvalor(str1$p.value),". Com a suposição de normalidade violada, sugerimos a realização do teste não paramétrico Mann-Whitney ao invés do teste-t.")
if(str1$p.value<0.05 & str2$p.value>=0.05) sup=c(" A suposição de normalidade das amostras foi verificada através do teste de Shapiro-Wilk, que com p-valor menor que 0.05 rejeitou a normalidade da distribuição de ",niveis[1]," - W=",round(str1$statistic,dig),", p-valor=",pvalor(str1$p.value),", mas não de ",niveis[2]," - W=",round(str2$statistic,dig),", p-valor=",pvalor(str2$p.value),". Com a suposição de normalidade violada, sugerimos a realização do teste não paramétrico Mann-Whitney ao invés do teste-t.")
if(str1$p.value<0.05 & str2$p.value<0.05) sup=c(" A suposição de normalidade das amostras foi verificada através do teste de Shapiro-Wilk, que com p-valor menor que 0.05 rejeitou a normalidade da distribuição dos dois grupos (",niveis[1]," - W=",round(str1$statistic,dig),", p-valor=",pvalor(str1$p.value),",  ",niveis[2]," - W=",round(str2$statistic,dig),", p-valor=",pvalor(str2$p.value),"). Com a suposição de normalidade violada, sugerimos a realização do teste não paramétrico Mann-Whitney ao invés do teste-t.")}


if(leveneTest(resp ~ factor(fator), data=d)$`Pr(>F)`[1]>0.05) {vareq=T; varsao="iguais"} else {vareq=F; varsao="diferentes"}

ef= round(cohens_d(resp ~ fator, var.equal=vareq,data=d)$effsize,dig)
if (ef<0) dif="menor que" else dif="maior que"

if (abs(ef)<0.2) mag=paste0(" Através da estatística d de cohen (",ef,"), verificamos a magnitude da diferença entre as médias menor que 0.2 DP, o que Cohen (1988) considerou desprezível, mesmo que seja estatisticamente significativa.") else
  if(abs(ef)<0.3) mag=paste0(" Através da estatística d de cohen (",ef,"), verificamos a magnitude da diferença entre as médias entre 0.2 e 0.3 DP's, o que Cohen (1988) considerou uma magnitude pequena.") else
    if(abs(ef)<0.8) mag=paste0(" Através da estatística d de cohen (",ef,"), verificamos a magnitude da diferença entre as médias entre 0.3 e 0.8 DP's, o que Cohen (1988) considerou uma magnitude média.") else
      mag=paste0(" Através da estatística d de cohen (",ef,"), verificamos a magnitude da diferença entre as médias maior que 0.8 DP's, o que Cohen (1988) considerou uma magnitude grande.")

teste=t.test(resp ~ fator,data=d,var.equal=vareq)

p = paste0(pvalor(teste$p.value),"c (d=",ef,")")

if (teste$p.value > 0.05) texto=c(" * **",ref,":** Realizando o teste-t bicaudal para duas amostras independentes com variâncias ",varsao," (t(",round(teste$parameter,0),
                                  ") = ",round(teste$statistic,2),"; p=",pvalor(teste$p.value),
                                  "), não encontramos evidências para rejeitar a igualdade de médias de ",nomecont," por ",nomecat,". De fato, os grupos '",niveis[1],"' (Média=",round(mean(resp1, na.rm=T),dig),", DP=",round(sd(resp1, na.rm=T),dig), ") e '",niveis[2],"' (Média=",
                                  round(mean(resp2, na.rm=T),dig),", DP=",round(sd(resp2, na.rm=T),dig),") tiveram estatísticas semelhantes. A estimativa de diferença entre as médias foi ", round(teste$estimate[1]-teste$estimate[2],dig)," e IC95%=(",round(teste$conf.int[1],dig),", ",round(teste$conf.int[2],dig),
                                 " ). Este intervalo inclui a estimativa de diferença igual a 0 (igualdade de médias).",sup,"\n") else
  texto=c(" * **",ref,":** Realizando o teste-t bicaudal para duas amostras independentes com variâncias ",varsao," (t(",
          round(teste$parameter,0),") = ",round(teste$statistic,dig),"; p=",pvalor(teste$p.value),"), rejeitamos a hipótese de igualdade de médias de ",nomecont," por ",nomecat,". Podemos ver que o grupo '",niveis[1],"' (Média=",round(mean(resp1, na.rm=T),dig),
", DP=",round(sd(resp1, na.rm=T),dig), ") apresenta média ",dif," o grupo '",niveis[2],"' (Média=",round(mean(resp2, na.rm=T),dig),", DP=",round(sd(resp2, na.rm=T),dig),"). A estimativa de diferença entre as médias foi ",
                                  round(teste$estimate[1]-teste$estimate[2],dig)," e IC95%=(",round(teste$conf.int[1],dig),", ",round(teste$conf.int[2],2),
                                 " ).",mag," Apesar disso, cabe considerar a relevância clínica ou prática da diferença. ",sup,"\n")


res = desc_bi_cont(d$resp,d$fator,F,respcol,F,dig)
res <- cbind(res,"p-valor"=c(p,rep("",dim(res)[1]-1)))

return(list("testes"=c("desc"=0,"catsame"=0,"t"=1,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
            "result"=res,
            "texto"=list(paste(texto,collapse=""))))}
