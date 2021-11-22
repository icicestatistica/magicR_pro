desc_uni_continua <- function(var,digitos){
  var=unlist(var)
  if(length(summary(var))==6) {N=length(var); na=0} else {N=length(var);na=summary(var)[7]}
  if(length(var)-sum(is.na(var))<3 | length(var)-sum(is.na(var))>3000 | min(var,na.rm=T)==max(var,na.rm=T)) p = "N/A" else p=pvalor(shapiro.test(var)$p.value)
  parametros <- c("N/A","Observações","Min-Máx","Q1-Q3","Mediana","Média","Desvio Padrão","CV", "Normalidade (Shapiro Wilk)")
  variavel <- c(paste0(na," (",100*round(na/N,digitos),"%)"),
                N-na,
                paste0(round(summary(var)[1],digitos),"-",round(summary(var)[6],digitos)),
                paste0(round(summary(var)[2],digitos),"-",round(summary(var)[5],digitos)),
                round(summary(var)[3],digitos),
                round(summary(var)[4],digitos),
                round(sd(var,na.rm=T),digitos),
                paste0(round(sd(var,na.rm=T)/summary(var)[4]*100,0),"%"),
                p)
  d <- data.frame("Característica"=parametros,"Estatística"=variavel)
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
  d <- data.frame(row.names(d),d)
  colnames(d) <- c("Característica","Frequência","Freq. Relativa","Freq.","Freq. Acumulada")[c(T,T,T,label,acumula)]
  row.names(d)=NULL
  return(d)}

cat_same_levels <- function(x,nomes,levels,nas,dig){
df = data.frame("Variável"=nomes[1],t(desc_uni_categorica(x[,1],levels,nas,T,F,F,dig)[,4]))
if(dim(x)[2]>1){
  for (i in 2:dim(x)[2]) df = rbind(df, data.frame("Variável"=nomes[i],t(desc_uni_categorica(x[,i],levels,nas,T,F,F,dig)[,4])))}
if(nas==T) levels=c(levels,"N/A")
names(df)=c("Variável",levels)
return(df)}

cat_same_levels_2 <- function(x,nomes,levels,nas,dig){
prop=prop.test(table(factor(x[,1],levels=levels)))
IC=paste0("(",round(100*prop$conf.int[1],dig),"%, ",round(100*prop$conf.int[2],dig),"%)")
df = data.frame("Variável"=nomes[1],desc_uni_categorica(x[,1],levels,F,F,F,F,dig)[1,c(2,3)],IC)
for (i in 2:dim(x)[2]){
  prop=prop.test(table(factor(x[,i],levels=levels)))
  IC=paste0("(",round(100*prop$conf.int[1],dig),"%, ",round(100*prop$conf.int[2],dig),"%)")
  df = rbind(df, data.frame("Variável"=nomes[i],desc_uni_categorica(x[,i],levels,F,F,F,F,dig)[1,c(2,3)],IC))}
df = df[order(as.numeric(df$Frequência), decreasing=T),]
names(df)=c("Variável",'Frequência',"Freq. Relativa", "IC 95% para Freq.")
return(df)}
