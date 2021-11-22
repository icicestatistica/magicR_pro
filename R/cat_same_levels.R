cat_same_levels <- function(x,nomes,levels,nas,dig){
df = data.frame("Variável"=nomes[1],t(desc_uni_categorica(x[,1],levels,nas,T,F,F,dig)[,4]))
if(dim(x)[2]>1){
  for (i in 2:dim(x)[2]) df = rbind(df, data.frame("Variável"=nomes[i],t(desc_uni_categorica(x[,i],levels,nas,T,F,F,dig)[,4])))}
if(nas==T) levels=c(levels,"N/A")
names(df)=c("Variável",levels)
return(df)}

cat_same_levels_2 <- function(x,nomes,levels,dig){
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
