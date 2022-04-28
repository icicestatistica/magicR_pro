mc <- function(i,prim,posim){

tab = as.data.frame.matrix(table(prim[,i],posim[,i]))
teste = mcnemar.test(prim[,i], y = posim[,i], correct = TRUE)
tab <- data.frame(tab,c(pvalor(teste$p.value),""))

if(teste$p.value<0.05) res = c("Testamos a diferença entre a proporção de acertos antes e imediatamente após a aplicação da intervenção através do teste de McNemar com correção de continuidade, que  não rejeitou a igualdade de acertos (McNemar's ", paste("$\\chi^2$",collapse=NULL),"(",teste$parameter,") = ",round(teste$statistic,dig),", p-value = ",pvalor(teste$p.value),"). \n") else
  res = c("Testamos a diferença entre a proporção de acertos antes e imediatamente após a aplicação da intervenção através do teste de McNemar com correção de continuidade, que  rejeitou a igualdade de acertos (McNemar's ", paste("$\\chi^2$",collapse=NULL),"(",teste$parameter,") = ",round(teste$statistic,dig),", p-value = ",pvalor(teste$p.value),"). \n")

return(list("tab"=tab,"res"=res))}

tab = as.data.frame.matrix(table(catprim[1:50],catposim[1:50]))
teste = mcnemar.test(catprim[1:50],catposim[1:50], correct = TRUE)
tab <- data.frame(tab,c(pvalor(teste$p.value),""))
kable(tab)

if(teste$p.value<0.05) res = c("Testamos a diferença entre a proporção de acertos antes e imediatamente após a aplicação da intervenção através do teste de McNemar com correção de continuidade, que  não rejeitou a igualdade de acertos (McNemar's ", paste("$\\chi^2$",collapse=NULL),"(",teste$parameter,") = ",round(teste$statistic,dig),", p-value = ",pvalor(teste$p.value),"). \n") else
  res = c("Testamos a diferença entre a proporção de acertos antes e imediatamente após a aplicação da intervenção através do teste de McNemar com correção de continuidade, que  rejeitou a igualdade de acertos (McNemar's ", paste("$\\chi^2$",collapse=NULL),"(",teste$parameter,") = ",round(teste$statistic,dig),", p-value = ",pvalor(teste$p.value),"). \n")
cat(res,sep="")

tab <- data.frame()
res <- c()
for (i in 1:12) {
  tab <- rbind(tab,mc(i,prim,posim)$tab)
  res <- c(res,mc(i,prim,posim)$res)}

kable(tab)
cat(res,sep="")
