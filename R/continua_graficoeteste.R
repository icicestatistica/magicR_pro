desc_uni_continua <- function(vari,nome,bins=20,texto=T, grafico=T,cor='cyan4',digitos=2, idioma="PT"){
  nf=""
  vari=unlist(vari)
  if(length(summary(vari))==6) {N=length(vari); na=0} else {N=length(vari);na=summary(vari)[7]}
  if(length(vari)-sum(is.na(vari))<3 | length(vari)-sum(is.na(vari))>3000 | min(vari,na.rm=T)==max(vari,na.rm=T)) {p = "N/A";nf="Não foi possível realizar o teste shapiro-wilk para normalidade, uma vez que não há observações suficientes para fazê-lo.  \n"} else {shap = shapiro.test(vari) ; p=pvalor(shap$p.value); if(shap$p.value <0.05) rej <- "menor que 0.05, rejeitou" else rej="maior ou igual a 0.05, não rejeitou"}
  cv=round(sd(vari,na.rm=T)/summary(vari)[4]*100,digitos)
  parametros <- c("Total","N/A","N","Min-Máx","Q1-Q3","Mediana","Média","DP","CV", "SW")
  iqr = round(summary(vari)[5]-summary(vari)[2],digitos)
  if(sum(is.na(vari))==length(vari)) variavel=c(N,paste0(na," (100%)"),0,"-","-","-","-","-","-","-") else {
    variavel <- c(N,
                  paste0(na," (",round(100*na/N,digitos),"%)"),
                  N-na,
                  paste0(round(summary(vari)[1],digitos),"-",round(summary(vari)[6],digitos)),
                  paste0(round(summary(vari)[2],digitos),"-",round(summary(vari)[5],digitos)),
                  round(summary(vari)[3],digitos),
                  round(summary(vari)[4],digitos),
                  round(sd(vari,na.rm=T),digitos),
                  paste0(cv,"%"),
                  p)}
  d <- data.frame("Característica"=parametros,"Estatística"=unlist(variavel))
  tex=NULL
  
  
  outl = length(boxplot.stats(na.omit(vari))$out)
  
  missings = as.numeric(d$Estatística[1])-as.numeric(d$Estatística[3])
  
  texto_outliers = ifelse(outl==0,"Não há outliers.", paste("Há ",outl," outliers.",sep=""))
  texto_missings = ifelse(missings==0,"Não há perda de dados.", paste("Há ",missings," missings, ou seja, linhas com perda de dados.",sep=""))
  
  interpretacao = paste(" + A variável **'",nome,"'**, variou entre ",round(summary(vari)[1],digitos)," e ",round(summary(vari)[6],digitos),". Sua média foi ",round(summary(vari)[4],digitos),", com desvio padrão de ",round(sd(vari,na.rm=T),digitos),". A mediana é ",round(summary(vari)[3],digitos)," e o intervalo interquartil é ",iqr," (Q1=",round(summary(vari)[2],digitos),"-Q3=",round(summary(vari)[5],digitos),"). ",texto_missings," ",texto_outliers,sep="")
  
if(texto==T){
  if(nf=="") nf=c("  + O teste de shapiro wilk, com p-valor ",rej," a hipótese de normalidade dos dados (W=",round(shap$statistic,digitos),", p-valor=",pvalor(shap$p.value),"). \n") else shaptexto=nf
    if(cv>50) cvtexto = " Como isso não ocorreu, valores próximos à média podem não ter sido tão frequentes nos dados. \n" else cvtexto = " Como isso ocorreu, os dados tendem a se concentrar perto da média. \n"
  dif=as.numeric(d$Estatística[7])-as.numeric(d$Estatística[6])
  simetria = 5*(dif)/as.numeric(d$Estatística[8])
  if(abs(simetria) > 1) { if(simetria >0) qt = "é significativa, indicando assimetria com concentração à esquerda e cauda à direita." else qt = "é significativa, indicando assimetria com concentração à direita e cauda à esquerda."} else qt = "não é significativa, indicando simetria."
  if(d$Estatística[1]==d$Estatística[3]) {tex <- c("* **",nome,": ** A variável '",nome,"' não teve perda de dados, também chamada de *\"missings\"*, portanto todas as ",d$Estatística[1]," linhas do banco estão preenchidas.")} else
  {nr=eval(parse(text=str_sub(str_split(d$Estatística[2]," ")[[1]][2],2,-3)))
  if(nr<=5) miss <- c("Como há menos de 5% de *missings* (",nr,"%), não há motivos para se preocupar com a ausência de dados.") else { if(nr<20) miss <- c("Como as não respostas representam ",nr,"% das linhas, cabe perguntar-se se há algum tipo de viés (algum fator que influenciou essa ausência de forma sistemática).") else miss <- c("Ressaltamos que há uma grande quantidade de não respostas para essa variável (",nr,"%), por isso recomendamos que algum tipo de explicação seja dada pela ausência desses dados.")}
  tex <- c("* **",nome,": ** Das ",d$Estatística[1]," linhas presentes no banco de dados, houve ",str_split(d$Estatística[2]," ")[[1]][1], " não respostas,  também chamada \"missings\". Assim, totalizamos ",d$Estatística[3]," observações no banco de dados. ", miss," \n")}
  tex <- c(tex, " Passamos a avaliar como os valores estão distribuídos: \n")
      tex <- c(tex, "  + Os dados variaram no intervalo (",d$Estatística[4] ,"), portanto sua amplitude (diferença entre o maior e o menor) foi ",round(-eval(parse(text=d$Estatística[4])),digitos),"; \n",
                "  + Olhando para os quartis, percebemos que 25% dos valores foram menores que ",str_split(d$Estatística[5],"-")[[1]][1]," e 25% foram maiores que ",str_split(d$Estatística[5],"-")[[1]][2],". Assim, a metade \"central\"  dos dados se distribuiu ao longo de ", -eval(parse(text=d$Estatística[5]))," unidades. Esta quantia também é chamada \"Intervalo Interquartil\"; \n",     
               "  + A mediana obtida foi ",d$Estatística[6], ", que indica que 50% dos dados estão abaixo desse valor e 50% estão acima. A diferença entre a média (",d$Estatística[7],") e a mediana (",d$Estatística[6],") ",qt," \n",
               "  + A variabilidade é medida pelo desvio padrão (",d$Estatística[8],"), e indica quanto os dados variam da média obtida. \n",
               "  + O CV - Coeficiente de Variação - (",d$Estatística[9],") compara o desvio padrão com a média. O ideal é que este índice seja o mais baixo possível (idealmente menor que 50%).",cvtexto,
                nf)
      tex=paste(tex,collapse="")} else tex=NULL
  
  if(grafico==T) grafico=graficos_continua(vari,nome,20,cor,digitos,idioma) else grafico=NULL

  
return(list("result"=d,"texto"=tex,"interp"=interpretacao,"grafico"=grafico))}

graficos_continua <- function(var,nome,bins=20,cor='cyan4',digitos=2, idioma="PT"){
  d <- data.frame(var=as.numeric(unlist(var)))
  excess <- round((max(d$var, na.rm=T)-min(d$var, na.rm=T))/8,0)
  min <- min(d$var, na.rm=T)-excess; max <- max(d$var, na.rm=T)+excess
  dp <- sd(d$var,na.rm=T)
  media=mean(d$var,na.rm=T)
  if(idioma=="PT") medianomegraf="Média=" else medianomegraf="Mean="
  box <- ggplot(d) + geom_boxplot(aes(x=var),fill=lighten(cor,0.2)) + theme_clean() + ggtitle(vetor_comsep_c(paste0(nome," (n=",length(d$var[!is.na(d$var)]),")"),40)) + xlab("") + ylab("")+
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.x=element_line(colour="gray"),
      panel.grid.minor.x=element_line(colour="lightgray"),
      panel.grid.major.y=element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.background = element_rect(colour="white")) + xlim(min,max)+
    geom_label(x=min(d$var,na.rm=T), y=-0.2,label=paste0("Min=",round(summary(d$var)[1],digitos)),color="black") +
    geom_label(x=summary(var)[2], y=0.1, label=paste0("Q1=",round(summary(var)[2],digitos)),color="black") +
    geom_label(x=summary(d$var)[3], y=-0.1, label=paste0("Med=",round(summary(var)[3],digitos)),color="black") +
    geom_label(x=summary(var)[5], y=0.1, label=paste0("Q3=",round(summary(var)[5],digitos)),color="black") +
    geom_label(x=summary(var)[6], y=-0.2, label=paste0("Max=",round(summary(var)[6],digitos)),color="black")
  
  histo <- ggplot(d,aes(x=var)) + 
    geom_histogram(aes(y=..density..),bins = bins, fill=cor) +
    geom_density(alpha=0.5,colour=NA,fill=cor,alpha=0.9) +
    geom_errorbarh(aes(xmax = media+dp, xmin = media-dp,y=0),inherit.aes = F,height=max(density(d$var, na.rm=T)$y)/5) +
    theme_clean() + xlab(nome) + xlim(min,max) +
    ylab("") + geom_vline(xintercept = mean(var,na.rm=T),color="black",size=1) + theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      plot.background = element_rect(colour="white"),
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      panel.grid.major.x=element_line(colour="gray"),
      panel.grid.minor.x=element_line(colour="lightgray"),
      panel.grid.major.y=element_blank()) +
      geom_label(x=summary(var)[4], y=0, label=paste0(medianomegraf,round(summary(var)[4],digitos)),color="black")
  
  return(box / histo)}
