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

result=kable(data.frame(nomes,sig),col.names=c("",nomes), caption=paste0("Análises de correlação de ",metodo," entre ",printvetor(nomes)))
return(result)}
