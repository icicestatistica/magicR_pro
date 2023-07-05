reportar_regressao = function(fit){

sumafit = summary(fit)

tabela = data.frame("Fonte"=c("Intercepto",row.names(sumafit$coefficients)[-1]),
round(sumafit$coefficients[,c(1:3)],2),
"p-valor"=sapply(sumafit$coefficients[,4],pvalor))
names(tabela)=c("Fonte","Coeficiente (Beta)","Erro padrao","Valor t","p-valor")

equacao = paste(tabela$`Coeficiente (Beta)`[1]," + ",paste(paste("(",tabela$`Coeficiente (Beta)`[-1],")", sep=""),tabela$Fonte[-1], sep=" * ", collapse=" + "),"\n",sep="")

grafico = sjPlot::plot_model(fit, grid=FALSE,show.values=T,vline.color="darkgray") + theme_icic() + theme(legend.position = "top", plot.title=element_text(hjust=0.5))


resultado = list("O modelo que melhor se ajustou foi: \n",equacao,"Podemos verificar mais detalhes sobre os coeficientes na tabela a seguir: \n",tabela,"\n","É possível visualizar esses resultados no gráfico seguinte: \n", grafico,"\n")

return(resultado)}
