desc_uni_continua <- function(var,digitos){
  if(length(summary(var))==6) {N=length(var); na=0} else {N=length(var);na=summary(var)[7]}
  if(length(var)-sum(is.na(var))<3 | length(var)-sum(is.na(var))>3000 | min(var,na.rm=T)==max(var,na.rm=T)) p = "N/A" else p=pvalor(shapiro.test(var)$p.value)
  parametros <- c("N (NA's)","Observações","Min-Máx","Q1-Q3","Mediana","Média","Desvio Padrão","CV", "Normalidade (Shapiro Wilk)")
  variavel <- c(paste0(N,"(",na,")"),
                N-na,
                paste0(round(summary(var)[1],digitos),"-",round(summary(var)[6],digitos)),
                paste0(round(summary(var)[2],digitos),"-",round(summary(var)[5],digitos)),
                round(summary(var)[3],digitos),
                round(summary(var)[4],digitos),
                round(sd(var,na.rm=T),digitos),
                paste0(round(sd(var,na.rm=T)/summary(var)[4]*100,0),"%"),
                p)
  d <- data.frame(Resultado=variavel,row.names=parametros)
  return(d)}
