cat_same_levels <- function(x,nomes,levels,nas,dig){
df = data.frame("Variável"=nomes[1],t(desc_uni_categorica(x[,1],levels,nas,T,F,F,dig)[,4]))
if(dim(x)[2]>1){
  for (i in 2:dim(x)[2]) df = rbind(df, data.frame("Variável"=nomes[i],t(desc_uni_categorica(x[,i],levels,nas,T,F,F,dig)[,4])))}
if(nas==T) levels=c(levels,"N/A")
names(df)=c("Variável",levels)
return(df)}

cat_same_levels_2 <- function(x,nomes,nomey,levels,dig,cor,sepvetor){
prop=prop.test(table(factor(unlist(x[,1]),levels=levels)))
df = data.frame("Variável"=nomes[1],desc_uni_categorica(x[,1],levels,F,F,F,F,dig)[1,c(2,3)],prop$conf.int[1],prop$conf.int[2])
for (i in 2:dim(x)[2]){
  prop=prop.test(table(factor(unlist(x[,i]),levels=levels)))
  df = rbind(df,data.frame("Variável"=nomes[i],desc_uni_categorica(x[,i],levels,F,F,F,F,dig)[1,c(2,3)],prop$conf.int[1],prop$conf.int[2]))}
df = df[order(as.numeric(df$Frequência), decreasing=T),]
names(df)=c("Variável",'Frequência',"Freq. Relativa", "ICmin","ICmax")

df_printar=data.frame(df[,1:3],paste("(",round(100*df$ICmin,dig),"%, ",round(100*df$ICmax,dig),"%)",sep=""))
names(df_printar)=c("Variável",'Frequência',"Freq. Relativa", "IC 95% para Freq.")

texto=c("Podemos avaliar esta tabela comparando os intervalos de confiança de cada ítem. Caso os intervalos de confiança de dois ítens se sobreponham, isso significa que não rejeitamos a diferença entre as proporções nesses dois ao nível de 5% de significância. Portanto, interpretamos da seguinte forma: \n")
for (i in 1:(dim(df)[1])){
if(sum(df$ICmin[i]>df$ICmax)>1) texto = c(texto,paste(" * ",df$Variável[i]," é maior que ",printvetor(df$Variável[df$ICmin[i]>df$ICmax]),". \n",sep="")) else
  if(sum(df$ICmin[i]>df$ICmax)==1) texto = c(texto,paste(" * ",df$Variável[i]," é maior que ",df$Variável[df$ICmin[i]>df$ICmax],". \n", sep="")) else
    if(i==dim(df)[1]) texto = c(texto,paste(" * ",df$Variável[i]," não é maior que nenhum. \n",sep="")) else {
      texto=c(texto, paste(" * ",printvetor(df$Variável[i:dim(df)[1]])," não são maiores que nenhum. \n",sep="")); break}}
texto=c(texto, "É possível visualizar esses resultados no gráfico a seguir: \n")
texto = paste(texto, sep="",collapse="")

df$Variável <- vetor_comsep(unlist(df$Variável),sepvetor)

result <- mutate(df, Variável=fct_reorder(Variável, desc(-as.numeric(Frequência))))
grafico=ggplot(result, aes(y = Variável, x = as.numeric(str_sub(`Freq. Relativa`,end=-2))/100)) +
  geom_bar(stat="identity", fill=cor) +
  geom_errorbar(aes(xmax = ICmax, xmin = ICmin)) + xlab("Proporção em %") + ylab(nomey) + theme_minimal() + scale_x_continuous(labels = scales::percent)

return(list("result"=df_printar,"texto"=texto,"gráfico"=grafico))}
