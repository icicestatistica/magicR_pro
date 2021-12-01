cat_same_levels <- function(x,nomes,levels,nas,dig){
cont=1
df = data.frame("Variável"=nomes[1],t(desc_uni_categorica(x[,1],levels,nas,T,F,F,dig)[,4]))
if(dim(x)[2]>1){
  for (i in 2:dim(x)[2]) {cont=cont+1 ; df = rbind(df, data.frame("Variável"=nomes[i],t(desc_uni_categorica(x[,i],levels,nas,T,F,F,dig)[,4])))}}
if(nas==T) levels=c(levels,"N/A")
names(df)=c("Variável",levels)
return(list("testes"=c("desc"=0,"catsame"=cont,"t"=0,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
            "result"=df))}

cat_same_levels_2 <- function(x,nomes,nomey,levels,dig,cor,sepvetor){
cont=1
prop=prop.test(table(factor(unlist(x[,1]),levels=levels)))
df = data.frame("Variável"=nomes[1],desc_uni_categorica(x[,1],levels,F,F,F,F,dig)[1,c(2,3)],prop$conf.int[1],prop$conf.int[2])
for (i in 2:dim(x)[2]){
  cont=cont+1
  prop=prop.test(table(factor(unlist(x[,i]),levels=levels)))
  df = rbind(df,data.frame("Variável"=nomes[i],desc_uni_categorica(x[,i],levels,F,F,F,F,dig)[1,c(2,3)],prop$conf.int[1],prop$conf.int[2]))}
df = df[order(as.numeric(df$Frequência), decreasing=T),]
names(df)=c("Variável",'Frequência',"Freq. Relativa", "ICmin","ICmax")

df_printar=data.frame(df[,1:3],paste("(",round(100*df$ICmin,dig),"%, ",round(100*df$ICmax,dig),"%)",sep=""))
names(df_printar)=c("Variável",'Frequência',"Freq. Relativa", "IC 95% para Freq.")

texto=list(paste("Podemos avaliar esta tabela comparando os intervalos de confiança de cada ítem. Caso os intervalos de confiança de dois ítens se sobreponham, isso significa que não rejeitamos a diferença entre as proporções nesses dois ao nível de 5% de significância. Portanto, interpretamos da seguinte forma:"," \n",sep=""))
for (i in 1:(dim(df)[1])){
if(sum(df$ICmin[i]>df$ICmax)>1) texto = list.append(texto,paste0(" *  ",df$Variável[i]," possui frequência maior que ",printvetor(df$Variável[df$ICmin[i]>df$ICmax]),"."," \n",sep="")) else
  if(sum(df$ICmin[i]>df$ICmax)==1) texto = list.append(texto,paste(" * ",df$Variável[i]," possui frequência maior que ",df$Variável[df$ICmin[i]>df$ICmax],"."," \n", sep="")) else
    if(i==dim(df)[1]) texto = list.append(texto,paste(" * ",df$Variável[i]," não possui frequência maior que nenhum."," \n",sep="")) else {
      texto=list.append(texto, paste(" * ",printvetor(df$Variável[i:dim(df)[1]])," não possuem frequências maiores que nenhum."," \n", sep="")); break}}
texto=list.append(texto, paste("É possível visualizar esses resultados no gráfico a seguir:"," \n"))

df$Variável <- vetor_comsep(unlist(df$Variável),sepvetor)

result <- mutate(df, Variável=fct_reorder(Variável, desc(-as.numeric(Frequência))))
grafico=ggplot(result, aes(y = Variável, x = as.numeric(str_sub(`Freq. Relativa`,end=-2))/100)) +
  geom_bar(stat="identity", fill=cor) +
  geom_errorbar(aes(xmax = ICmax, xmin = ICmin)) + xlab("Proporção em %") + ylab(nomey) + theme_minimal() + scale_x_continuous(labels = scales::percent)

return(list("testes"=c("desc"=0,"catsame"=cont,"t"=0,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
            "result"=df_printar,
            "texto"=texto,
            "gráfico"=grafico))}
