cat_same_levels <- function(dad,nomes='auto',niveis='auto',nas=F,dig=2,cor="cyan4",invertercores=F, idioma="PT"){
  
if(nomes[1]=='auto') nomes=names(dad)
names(dad)=nomes
if(niveis=="auto") niveis=names(table(matrix(unlist(dad))))
cont=1
df = data.frame("Variável"=nomes[1],t(desc_uni_categorica(dad[,1],"",niveis,nas,T,F,F,F,F,F,dig)$result[,4]))
if(dim(dad)[2]>1){
  for (i in 2:dim(dad)[2]) {cont=cont+1 ; df = rbind(df, data.frame("Variável"=nomes[i],t(desc_uni_categorica(dad[,i],"",niveis,nas,T,F,F,F,F,F,dig)$result[,4])))}}
if(nas==T) niveis=c(niveis,"N/A")
names(df)=c("Variável",niveis)
  
for (i in 1:dim(dad)[2]) dad[,i] = factor(unlist(dad[,i]), levels=niveis)

cores=ifelse(invertercores==T,"lighten(cor, seq(0, (1 - 1/(length(table(newmat$Resultado)))), 
            1/(length(table(newmat$Resultado)))))","rev(lighten(cor, seq(0, (1 - 1/(length(table(newmat$Resultado)))), 1/(length(table(newmat$Resultado))))))")

mat=data.frame("Coluna"=rep(names(dad),each=dim(dad)[1]),"Resultado"=matrix(unlist(dad)))
mat$Coluna = factor(mat$Coluna, levels=names(dad))
levels(mat$Coluna)= paste0(nomes," (n=",table(na.omit(mat)$Coluna),")")

ordem = mat %>% group_by(Coluna) %>% dplyr::summarise(mean=mean(as.numeric(Resultado), na.rm=T))
oc=as.character(ordem$Coluna[order(ordem$mean)])

newmat = data.frame(table(mat),"n"=data.frame(table(na.omit(mat)$Coluna))[,2])
newmat$label = paste(newmat$Freq," (",round(100*newmat$Freq/newmat$n,dig),"%)",sep="",collapse=NULL)
newmat$Coluna = factor(newmat$Coluna, levels=oc)

plot=ggplot(newmat, aes(y=Coluna, x=Freq/n, fill=Resultado)) + geom_bar(stat="identity", position = position_stack(reverse = T)) + labs(title = "Frequência e Frequência Relativa por resultado", y=NULL,x=NULL) + theme_minimal() + theme(plot.title=element_text(hjust=0.5)) + scale_x_continuous(labels = scales::percent) +
  geom_text(label=ifelse(newmat$label=="0 (0%)","",newmat$label), position = position_stack(reverse = T, vjust=0.5), check_overlap = T, hjust=0.5) + scale_fill_manual(values  = eval(parse(text=cores)))

return(list("testes"=c("desc"=0,"catsame"=cont,"t"=0,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
            "t1"="A análise detalhada das variáveis está na tabela a seguir: \n","result"=df,"t2"="\n Podemos visualizar esses resultados no seguinte gráfico: \n","grafico"=plot))}

cat_same_levels_2 <- function(x,nomes="auto",nomey,levels="auto",dig=2,cor="cyan4",sepvetor=7,idioma="PT"){
cont=1
if(nomes=="auto") nomes = names(x)
if (levels=='auto') levels = c("Sim","Não")
prop=prop.test(table(factor(unlist(x[,1]),levels=levels)))
df = data.frame("Variável"=nomes[1],desc_uni_categorica(x[,1],"",levels,F,F,F,F,F,F,F,dig)$result[1,c(2,3)],prop$conf.int[1],prop$conf.int[2])
for (i in 2:dim(x)[2]){
  cont=cont+1
  prop=prop.test(table(factor(unlist(x[,i]),levels=levels)))
  df = rbind(df,data.frame("Variável"=nomes[i],desc_uni_categorica(x[,i],"",levels,F,F,F,F,F,F,F,dig)$result[1,c(2,3)],prop$conf.int[1],prop$conf.int[2]))}

names(df)=c("Variável",'Frequência',"Freq. Relativa", "ICmin","ICmax")
df = df[order(as.numeric(str_sub(df$`Freq. Relativa`,end=-2)), decreasing=T),]

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

if(idioma=="PT") tituloeixox="Proporção em %" else tituloeixox="Proportion in %"
result <- mutate(df, Variável=fct_reorder(Variável, desc(-as.numeric(str_sub(`Freq. Relativa`,end=-2)))))
grafico=ggplot(result, aes(y = Variável, x = as.numeric(str_sub(`Freq. Relativa`,end=-2))/100)) +
  geom_bar(stat="identity", fill=cor) +
  geom_errorbar(aes(xmax = ICmax, xmin = ICmin)) + xlab(tituloeixox) + ggtitle(nomey) + ylab("") + theme_minimal() + 
  scale_x_continuous(labels = scales::percent) + theme(plot.title = element_text(hjust = 0.5))

return(list("testes"=c("desc"=0,"catsame"=cont,"t"=0,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
            "result"=df_printar,
            "texto"=texto,
            "gráfico"=grafico))}
