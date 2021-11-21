desc_uni_continua <- function(var,digitos){
  var = unlist(var)
  if(length(summary(var))==6) {N=length(var); na=0} else {N=length(var);na=summary(var)[7]}
  if(length(var)-sum(is.na(var))<3 | length(var)-sum(is.na(var))>3000 | min(var,na.rm=T)==max(var,na.rm=T)) p = "N/A" else p=pvalor(shapiro.test(var)$p.value)
  parametros <- c("N (NA's)","Observações","Min-Máx","Q1-Q3","Mediana","Média","Desvio Padrão","CV", "Normalidade (Shapiro Wilk)")
  variavel <- c(paste0(N," (",na,")"),
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

desc_uni_categorica <- function(var,niveis,nas,label,ordenar,acumula,digitos){
  var=unlist(var)
  if (niveis[1] != F) var <- factor(var, levels = niveis)
  if (nas==FALSE) {d<-t(rbind(round(table(var),0),paste0(round(100*prop.table(table(var)),digitos),"%")))} else
  {d<-t(rbind(round(table(var),0),paste0(round(100*table(var)/length(var),digitos),"%")))
   d <- rbind(d, "N/A"=c(sum(is.na(var)),paste0(round(100*sum(is.na(var))/length(var),digitos),"%")))}
  if (ordenar==TRUE) {d <- d[order(as.numeric(d[,1]),decreasing = T),]}
  if (label==TRUE) {d <- data.frame(d, "Freq."=paste0(d[,1]," (",d[,2],")"))}
  if (acumula==TRUE) {d <- data.frame(d,"Freq. Relativa Acumulada"= paste0(cumsum(d[,1])," (", round(100*cumsum(d[,1])/(cumsum(d[,1])[nrow(d)]),digitos),"%)"))}
  colnames(d) <- c("Frequência","Freq. Relativa","Freq.","Freq. Relativa Acumulada")[c(T,T,label,acumula)]
  return(d)}
