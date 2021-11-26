mann <- function(x,y,nomex,nomey,niveis,dig,respcol,excluirtotal){

if(respcol=T) ref=nomex else ref=nomey  
  
d=data.frame("x"=x,"y"=y)

y1=as.numeric(d$y[d$x==niveis[1]])
y2=as.numeric(d$y[d$x==niveis[2]])
  
a=wilcox.test(y ~ x ,data=d, paired=F)

str1=shapiro.test(y1)
str2=shapiro.test(y2)

r=rcompanion::wilcoxonR(as.numeric(d$y), g = d$x, ci = F)

if(r<0) dif="menor que" else dif="maior que"

if (abs(r)<0.1) mag=paste0(" Através da estatística r (",round(r,3),"), verificamos a magnitude da diferença detectada menor que 0.1, o que foi classificado em Fritz et al., 2011 como desprezível, mesmo que seja estatisticamente significativa.") else
  if(abs(r)<0.3) mag=paste0(" Através da estatística r (",round(r,3),"), verificamos a magnitude da diferença detectada entre 0.1 e 0.3, o que foi classificado em Fritz et al., 2011 como um efeito pequeno.") else
    if(abs(r)<0.5) mag=paste0(" Através da estatística r (",round(r,3),"), verificamos a magnitude da diferença detectada entre 0.3 e 0.5, o que foi classificado em Fritz et al., 2011 como um efeito médio.") else
      mag=paste0(" Através da estatística r (",round(r,3),"), verificamos a magnitude da diferença detectada maior que 0.5, o que foi classificado em Fritz et al., 2011 como um efeito grande.")

if(str1$p.value>=0.05 & str2$p.value>=0.05) sup=paste0(" Através do teste de Shapiro-Wilk, com p-valor maior que 0.05 para as duas amostras, não rejeitamos a normalidade das distribuições (",niveis[1]," - W=",round(str1$statistic,2),", p-valor=",pvalor(str1$p.value,3),", ",niveis[2]," - W=",round(str2$statistic,2),", p-valor=",pvalor(str2$p.value,3),"), por este motivo o teste-t também pode ser utilizado, com muitas vantagens do ponto de vista de interpretação do resultado.") else
if(str1$p.value>=0.05 & str2$p.value<0.05) sup=paste0(" Através do teste de Shapiro-Wilk, com p-valor menor que 0.05 rejeitamos a normalidade da distribuição de ",niveis[2]," - W=",round(str2$statistic,2),", p-valor=",pvalor(str2$p.value,3),", mas não de ",niveis[1]," - W=",round(str1$statistic,2),", p-valor=",pvalor(str1$p.value,3),", o que justifica a realização do teste Mann-Whitney ao invés do tradicional teste-t.")
if(str1$p.value<0.05 & str2$p.value>=0.05) sup=paste0(" Através do teste de Shapiro-Wilk, com p-valor menor que 0.05 rejeitamos a normalidade da distribuição de ",niveis[1]," - W=",round(str1$statistic,2),", p-valor=",pvalor(str1$p.value,3),", mas não de ",niveis[2]," - W=",round(str2$statistic,2),", p-valor=",pvalor(str2$p.value,3),", o que justifica a realização do teste Mann-Whitney ao invés do tradicional teste-t.")
if(str1$p.value<0.05 & str2$p.value<0.05) sup=paste0(" Através do teste de Shapiro-Wilk, com p-valor menor que 0.05 rejeitamos a normalidade da distribuição dos dois grupos (",niveis[1]," - W=",round(str1$statistic,2),", p-valor=",pvalor(str1$p.value,3),",  ",niveis[2]," - W=",round(str2$statistic,2),", p-valor=",pvalor(str2$p.value,3),"), o que justifica a realização do teste Mann-Whitney ao invés do tradicional teste-t.")

if(a$p.value < 0.05) {texto=c(" * **",ref,":** Realizando o teste de Mann Whitney (W=",as.numeric(c(a$statistic)),", p=",pvalor(a$p.value,3),"), rejeitamos a hipótese de igualdade de distribuições de ",nomey," entre os grupos. ",
"O grupo ",niveis[1]," (mediana=",round(median(y1, na.rm=T),2)," e intervalo interquartil = [",
round(quantile(y1,0.25,na.rm=T),2),",",round(quantile(y1,0.75,na.rm=T),2),"]) é ",dif," o grupo ",niveis[2]," (mediana=",round(median(y2, na.rm=T),2)," e intervalo interquartil = [",
round(quantile(y2,0.25,na.rm=T),2),",",round(quantile(y2,0.75,na.rm=T),2),"]).",mag,sup,"\n")} else
  texto= c(" * **",ref,":** Não encontramos evidência estatística através do teste de Mann Whitney para rejeitar diferenças entre as distribuições de ",nomey," dos grupos (W=",as.numeric(c(a$statistic)),", p=",pvalor(a$p.value,3),").",sup,"\n")

p=paste0(pvalor(a$p.value,3),"d (r=",round(r,2),")")

res=desc_bi_cont(d$y,d$x,F,respcol,F,dig)
tot=res[1,2]
  
if(excluirtotal==T) res=res[-1,]
  
res <- cbind(rbind(c(paste("**",ref,"** (", tot,")",sep=""),rep("",dim(res)[2])),res),"p-valor"=c("",p,rep("",dim(res)[1]-1)))

return(list("testes"=c("desc"=0,"catsame"=0,"t"=1,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
            "result"=res,
            "texto"=list(paste(texto,collapse=""))))}
