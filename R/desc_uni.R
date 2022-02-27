desc_uni_continua <- function(var,digitos){
  var=unlist(var)
  if(length(summary(var))==6) {N=length(var); na=0} else {N=length(var);na=summary(var)[7]}
  if(length(var)-sum(is.na(var))<3 | length(var)-sum(is.na(var))>3000 | min(var,na.rm=T)==max(var,na.rm=T)) p = "N/A" else p=pvalor(shapiro.test(var)$p.value)
  parametros <- c("N","N/A","Observações","Min-Máx","Q1-Q3","Mediana","Média","Desvio Padrão","CV", "Normalidade (Shapiro Wilk)")
  if(sum(is.na(var))==length(var)) variavel=c(N,paste0(na," (100%)"),0,"-","-","-","-","-","-","-") else {
  variavel <- c(N,
                paste0(na," (",round(100*na/N,digitos),"%)"),
                N-na,
                paste0(round(summary(var)[1],digitos),"-",round(summary(var)[6],digitos)),
                paste0(round(summary(var)[2],digitos),"-",round(summary(var)[5],digitos)),
                round(summary(var)[3],digitos),
                round(summary(var)[4],digitos),
                round(sd(var,na.rm=T),digitos),
                paste0(round(sd(var,na.rm=T)/summary(var)[4]*100,digitos),"%"),
                p)}
  d <- data.frame("Característica"=parametros,"Estatística"=unlist(variavel))
  return(d)}
