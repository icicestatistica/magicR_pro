matrizcorrel <- function(x,nomes,metodo){
  
m=cor(x, method=metodo,use="na.or.complete")
a=cor.mtest(x,method=metodo,use="na.or.complete")
n=length(nomes)

sig=matrix(nrow=n,ncol=n)

for (i in 1:n)
  for (j in 1:n)
    if(i==j) sig[i,j]="-" else
      if (i<j) sig[i,j]="" else
          if(a$p[i,j]<0.01) sig[i,j]=paste0(round(m[i,j],3),"**") else if(a$p[i,j]<0.05) sig[i,j]=paste0(round(m[i,j],3),"*") else sig[i,j]=paste0(round(m[i,j],3))

result=data.frame(nomes,sig)
names(result)=c("",nomes)
testes = data.frame(Nome1 = "", Nome2 = printvetor(nomes, aspas=F),tipo = "matcorrel", sig_ou_não = NA, resumo = NA, sup = NA)
return(list("testes"=testes,"result"=result))}
        
matcorrel_meta = function(){
  texto="- **Matriz de correlação**:
Uma matriz de correlação consiste em linhas e colunas que mostram as correlações entre duas variáveis de cada vez, sendo uma especificada na linha e uma na coluna, de modo que cada célula da tabela contém o coeficiente de correlação. É uma ferramenta poderosa para resumir um grande conjunto de dados e para identificar e visualizar padrões nos dados fornecidos. \n"
  return(list("texto"=texto,"bib"=NULL))}
