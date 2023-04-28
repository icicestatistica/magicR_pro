kruskall <- function(resp,fator,nomeresp,nomefator,niveis='auto',dig=2,respcol=T,excluirtotal=T,cor="cyan4",ordenar=F, idioma="PT", ordinal=F,labels=T){
  
  if(respcol==T) ref=nomefator else ref=nomeresp
  
  resp=unlist(resp)
  fator=unlist(fator)
  if(niveis[1]=="auto") niveis = names(table(fator))
  fator <- factor(fator,levels=niveis)
  
  dad <- data.frame("continua"=as.numeric(resp),"categorica"=fator)
  dad2= data.frame("continua"=resp,"categorica"=fator)
  
  a <- kruskal.test(dad$continua, dad$categorica)
  c=round(kruskal_effsize(dad, continua ~ categorica)$effsize,dig)
  p=paste0(pvalor(a$p.value),"f ($\\eta^2$=",c,")")
  
  if (a$p.value > 0.05) {tabela=NULL
  texto=c("* **",ref,"**: Não encontramos com o teste de Kruskall Wallis evidência de diferença entre os grupos (",paste("$\\chi^2$",collapse=NULL),"(",a$parameter,") =",round(a$statistic,dig),",p-valor=",pvalor(a$p.value),"). \n"); resumo=paste0("Não encontramos com o teste de Kruskall Wallis evidência de diferença de ",nomeresp," entre os grupos de ",nomefator," (",paste("$\\chi^2$",collapse=NULL),"(",a$parameter,")=",round(a$statistic,dig),",p-valor=",pvalor(a$p.value),").")
} else {
    texto=c("* **",ref,"**: O teste de Kruskall-Wallis mostrou que há diferença entre os grupos (",paste("$\\chi^2$",collapse=NULL),"(",a$parameter,") =",round(a$statistic,dig),",p-valor=",pvalor(a$p.value),").")
    resumo=paste0("O teste de Kruskall-Wallis mostrou que há diferença de ",nomeresp," entre os grupos de ",nomefator," (",paste("$\\chi^2$",collapse=NULL),"(",a$parameter,")=",round(a$statistic,dig),",p-valor=",pvalor(a$p.value),").")
    dunn <- quiet(dunn.test(dad$continua, dad$categorica,method = "bonferroni",kw=F,table=F,list=F))
    b <- data.frame(dunn$comparisons,dunn$P.adjusted)
    
    ordem <- dad %>% group_by(categorica) %>% 
      get_summary_stats(continua, type = "median_iqr")
    
    ord = c(ordem[order(ordem$median),1])$categorica
    
    d <- c(b$dunn.P.adjusted)
    names(d) <- str_replace(b$dunn.comparisons, " - ","-")
    
    #mult = multcompLetters(d)
    #new <- rep("",dim(dad)[1])
    #for (i in 1:length(mult$Letters)){
    #new[dad$categorica==names(mult$Letters)[i]] <- mult$Letters[i]}
    
    tabela=data.frame("Comparações"=dunn$comparisons,"Estatística"=round(dunn$Z,dig),"p-valor ajustado"=pvetor(dunn$P.adjusted))
    names(tabela)=c("Comparações","Estatística Z","p-valor ajustado")
    
    
    difs = matrix(unlist(str_split(tabela$Comparações," - ")),ncol=2,byrow=T)
    
    ncomps=dim(tabela)[1]
    
    r=rep("",ncomps)
    for (i in 1:ncomps){
      if(b$dunn.P.adjusted[i]>0.05) r[i] = "Não" else
        if(tabela$`Estatística Z`[i]>0) r[i]="Maior" else r[i]="Menor"}
    
    resumo = factor(r, levels=c("Maior","Menor","Não"))
    
    tex <- c()
    difs = data.frame(cbind(difs,as.character(resumo)))
    
    if(prop.table(table(resumo))[3]==1) tex=c(tex,"Apesar de termos encontrado diferença pelo teste global de Kruskall Wallis, ao realizar o teste de Dunn de comparação par-a-par, nenhuma das comparações de grupos teve diferença estatisticamente significativa.  \n") else
      if(sum(prop.table(table(resumo))[1:2])==1) tex=c(tex,"O teste de comparações múltiplas de Dunn apontou diferenças entre todos os grupos estudados") else {
        tex=c(tex," O teste de comparações múltiplas de Dunn apontou as seguintes diferenças:  \n")
        for (j in which(resumo!="Não")){
          if(r[j]=="Menor") tex=c(tex,c("  + \"",difs[j,2], "\" é maior que \"",difs[j,1],"\";"),"\n")
          if(r[j]=="Maior") tex=c(tex,c("  + \"",difs[j,1], "\" é maior que \"",difs[j,2],"\";"),"\n")}
        tex=c(tex, "\n  Podemos verificar esses resultados na seguinte tabela:")}
    texto = c(texto,tex)}
  
  if(ordinal==F) res=desc_bi_cont(dad$continua,dad$categorica,F,respcol,F,dig) else 
  {if(respcol==F) res=desc_bi_cat(linha=dad2$continua,col=dad2$categorica,respcol=F) else res=desc_bi_cat(linha=dad2$categorica,col=dad2$continua,respcol=T)}
  
  
  tot=dim(na.omit(dad))[1]
  if(excluirtotal==T) res=res[-1,]
  
  res <- cbind(rbind(c(paste("**",ref,"** (", tot,")",sep=""),rep("",dim(res)[2]-1)),res),"p-valor"=c("",p,rep("",dim(res)[1]-1)))
  
  if(is.null(tabela)==TRUE) texto=paste(texto,collapse="") else texto=list(paste(texto,collapse=""),tabela)
  
  a1=a$parameter  ; a2=round(a$statistic,dig) ; a3=ifelse(a$p.value<0.001,"<0.001",round(a$p.value,3))
  textograf <- substitute(paste("Kruskall-Wallis (",chi^2,"(",a1,") =",a2,",p=",a3,")",collapse=""),list(a1=a1,a2=a2,a3=a3))
  if(ordinal==F) grafico = grafico_comp_box(dad$continua,nomeresp,dad$categorica,nomefator,cor=cor,textograf,dig,ordenar, idioma) else
    grafico = grafico_catcat(dad2$categorica,nomefator,dad2$continua,nomeresp, cor=cor, textograf,idioma,labels=labels) + coord_flip()
  
  testes=data.frame(Nome1 = nomeresp, Nome2 = nomefator, tipo = "kw", sig_ou_não = ifelse(a$p.value<0.05,T,F), resumo = resumo,sup=NA)
  
  return(list("testes"=testes,
              "result"=res,
              "texto"=texto,
              "grafico"=grafico))}
