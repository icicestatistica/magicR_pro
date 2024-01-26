anovac <- function(continua,categorica,nomecont,nomecat,niveis="auto",dig=2,respcol=T,excluirtotal=F,cor="cyan4",idioma="PT",virgula=F){
  
  if(respcol==T) ref=nomecat else ref=nomecont
  tabela=NULL
  
  continua=unlist(continua)
  categorica=unlist(categorica)
  if(niveis[1]=="auto") niveis = names(table(categorica))
  categorica <- factor(categorica,levels=niveis)
  
  d <- data.frame(continua,categorica)
  names(d) <- c("resp","fator")
  
  homog = leveneTest(resp ~ fator, data=d)
  
  if(is.na(homog$`Pr(>F)`[1])) {supos=F; res=NULL;texto=NULL;grafico=NULL; testes=data.frame(Nome1 = nomecont, Nome2 = nomecat, tipo = "-", sig_ou_não = NA, resumo = NA,sup=F)
  } else {
    
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
    sig=ifelse(pv>0.05," NÃO "," ")
    if(is.null(a$ges)) eta = "-" else {eta=round(a$ges,(dig+1));
    tamanef = ifelse(eta<0.01,"insignificante. ",ifelse(eta<0.06,"pequeno. ",ifelse(eta<0.14,"médio. ","grande. ")))
    etatext=c("O tamanho de efeito $\\eta^2$ = ",eta," indica ",100*eta,"% de variabilidade de ",nomecont," explicada por ",nomecat,", o que Cohen(1988) classificou como um efeito ",tamanef)}
    
    #suposições
    
    shapresid=shapiro.test(res)
    dassump=c()
    for (i in niveis){
      obs=d$resp[d$fator==i]
      if(length(na.omit(obs))<3 | min(obs, na.rm=T)==max(obs, na.rm=T)) dassump=c(dassump,i,"","") else {shap=shapiro.test(obs); dassump=c(dassump,i,shap$statistic,shap$p.value)}}
    dassump <- data.frame(matrix(dassump,ncol=3,byrow=T))
    dassump <- rbind(dassump,c("Resíduos",shapresid$statistic,shapresid$p.value))
    
    supos= ifelse(min(as.numeric(dassump$X3[dassump$X3!=""]))>0.05 & sum(dassump$X3=="")==0,T,F)
    
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
    
    textotipo=data.frame(c(", com p-valor maior que 0.05 no teste de shapiro-wilk, cumpre a suposição de normalidade. ",", com p-valor maior que 0.05 no teste de shapiro-wilk, cumprem a suposição de normalidade. "),c(", com p-valor menor que 0.05 no teste de shapiro-wilk, NÃO cumpre a suposição de normalidade. ",", com p-valor menor que 0.05 no teste de shapiro-wilk, NÃO cumprem a suposição de normalidade. "),c(" não apresentou dados suficientes para realização do teste ou não possui variabilidade. "," não apresentaram dados suficientes para realização do teste ou não possuem variabilidade. "))
    
    anasup=paste(" Quanto às suposições dos testes, temos o seguinte cenário: ",printsup(tcumpre,textotipo[,1]),printsup(tnaocumpre,textotipo[,2]),printsup(tnaocalc,textotipo[,3])," ",paste("(",paste(supg[-length(supg)],collapse="; "),")",sep=""),". Verificamos também a normalidade dos resíduos do ajuste, que ",textosupres,paste("(",paste(supg[length(supg)],collapse="; "),").",sep=""),vali,sep="")
    
    texto=paste(c(" * **",ref,":** No total, ",dim(na.omit(d))[1]," linhas apresentaram dados completos sobre ",nomecat," e ",nomecont,". A análise foi feita a partir de uma ANOVA de uma via",corr,", que ",sig,"rejeitou a ausência de efeito de ",nomecat," em ",nomecont," (F(",a$DFn,",",round(a$DFd,dig),") = ",round(a$F,dig),", p = ",pvalor(pv),", $\\eta^2$ = ",eta,"). ",etatext,homogtext,anasup), collapse="")
    
    p=paste0(pvalor(pv),"e ($\\eta^2$=",eta,")")
    
    ordem <- d %>% group_by(fator) %>% get_summary_stats(resp, type = "mean_sd")
    if(is.na(ordem[dim(ordem)[1],1])) ordem = ordem[-dim(ordem)[1],]
    ord = c(ordem[order(ordem$mean),1])$fator
    

    
    estats = printvetor(paste0(ordem$fator[ord]," (M=",round(ordem$mean[ord],2),", DP=",round(ordem$sd[ord],2),")"),aspas=F)
    
    texto = paste0(texto, paste0(" Sendo assim, é adequado descrever os dados através da média e do desvio padrão, como vemos a seguir: ",estats,". "))
    
        #post hoc
    
    if(pv<0.05){
    
    fit <- aov(data=d,resp ~ fator)
    
    t=TukeyHSD(fit)$fator
    n = dim(t)[1]
    
    tabela<- data.frame("Comparação"=row.names(t),"Diferença"=round(t[,1],1), 
                        "IC 95%"=paste0("(",round(t[,2],1),", ",round(t[,3],1),")"),
                        "P-valor"=pvetor(t[,4]))
    
    difs = matrix(unlist(str_split(row.names(t),"-")),ncol=2,byrow=T)
    
    b <- data.frame(difs,t[,4])

    jafoi=c()

            comp = " Seguindo para as comparações múltiplas HSD de tukey com correção de Bonferroni, concluímos que "
            
              for (ni in niveis[as.numeric(ord)][-length(ord)]) {
                  linhas = which((b$X1==ni | b$X2==ni) & ((b$X1 %in% jafoi)==F & (b$X2 %in% jafoi)==F))
                  ger = b[linhas,]
                  ger$grupo = apply(ger[,1:2],1,function(x) x[which(x!=ni)])
                  signi = which(ger$t...4.<0.05)
                  if(length(signi)==0) {comp = c(comp,paste0(ni," não difere de ",printvetor(paste0(ger$grupo," (p=",pvetor(ger$t...4.),")"),aspas=F),"."))} else {
                      if(length(signi)==length(ger$grupo) & length(ger$grupo>1)) {comp = c(comp,paste0(ni," é menor que ",printvetor(paste0(ger$grupo," (p=",pvetor(ger$t...4.),")"),aspas=F),"."))} else {
  maio = ger$grupo[signi]; pmaio = pvetor(ger$t...4.[signi])
  naodif = ger$grupo[-signi] ; pnaodif = pvetor(ger$t...4.[-signi])
  comp = c(comp,paste0(ni," é menor que ",printvetor(paste0(maio," (p=",pmaio,")"),aspas=F),", mas não difere de ",printvetor(paste0(naodif," (p=",pnaodif,")"),aspas=F),"."))}
  }
              jafoi=c(jafoi,ni)}
        comp = paste0(comp,collapse=" ")    
        texto = paste0(texto,comp)
   }
    
    res=desc_bi_cont(d$resp,d$fator,F,respcol,F,dig)
    tot=dim(na.omit(d))[1]
    
    if(excluirtotal==T) res=res[-1,]
    
    res <- cbind(rbind(c(paste("**",ref,"** (", tot,")",sep=""),rep("",dim(res)[2]-1)),res),"p-valor"=c("",p,rep("",dim(res)[1]-1)))
    
    if(is.null(tabela)==TRUE) texto=paste(texto,collapse="") else texto=list(paste(texto,collapse=""),tabela)
    
    a1=a$DFn  ; a2=round(a$DFd,dig) ; a3=ifelse(pv<0.001,"<0.001",paste("=",round(pv,3))) ; a4=round(a$F,dig)
    textograf <- substitute(paste("ANOVA one-way F(",a1,",",a2,") = ",a4,", p",a3,collapse=""),list(a1=a1,a2=a2,a3=a3,a4=a4))
    grafico=grafico_comp_bar(d$resp,nomecont,d$fator,nomecat,cor=cor,teste=textograf,dig=dig, idioma=idioma,virgula=virgula)
    
    
    diferencas_resumo = ifelse(pv<0.05,"Houve","Não houve")
    
    inicio_resumo = paste0("\n -",diferencas_resumo," diferença entre as médias de ",nomecont," por ",nomecat," (F(",a$DFn,",",round(a$DFd,dig),") = ",round(a$F,dig),", p = ",pvalor(pv),"), com estatísticas ",estats,")",".",collapse="")
    
    resumo = ifelse(pv<0.05,paste0(inicio_resumo," ",comp),inicio_resumo)
    
    testes=data.frame(Nome1 = nomecont, Nome2 = nomecat, tipo = "aov1", sig_ou_não = ifelse(pv<0.05,T,F), resumo = resumo,sup=supos)
  }
  
  return(list("testes"=testes,
              "result"=res,
              "texto"=texto,
              "grafico"=grafico))}
