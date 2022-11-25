reportar_regressao = function(fit){

sumafit = summary(fit)

tabela = data.frame("Fonte"=c("Intercepto",row.names(sumafit$coefficients)[-1]),
round(sumafit$coefficients[,c(1:3)],2),
"p-valor"=sapply(sumafit$coefficients[,4],pvalor))
names(tabela)=c("Fonte","Coeficiente (Beta)","Erro padrao","Valor t","p-valor")

equacao = paste(tabela$`Coeficiente (Beta)`[1]," + ",paste(paste("(",tabela$`Coeficiente (Beta)`[-1],")", sep=""),tabela$Fonte[-1], sep=" * ", collapse=" + "),"\n",sep="")

grafico = sjPlot::plot_model(fit, grid=FALSE,show.values=T,vline.color="darkgray") + theme_icic() + theme(legend.position = "top", plot.title=element_text(hjust=0.5))

explicacao_tabela <- paste("A coluna Erro padrão é uma medida de variabilidade na estimativa dos coeficientes, idealmente este valor deve ser menor que o do coeficiente.","A coluna \'valor-t\' possui valores que são usados para calcular o p-valor e os níveis de significância, eles definem a significância do coeficiente da variável considerada.","A coluna P-valor é o p-valor do teste t, que representa a probabilidade que a variável não seja relevante para o modelo.","Os símbolos apresentados do lado direito do p-value mostram para qual significância os coeficientes são significativos. * (p<0.05), ** (p<0.01),*** (p<0.001)", sep="\n")


grafico_residuos = ggplot() + geom_histogram(aes(x=sumafit$residuals)) + theme_icic() + labs(title="Histograma dos residuos",y="Frequencia",x="Residuos do modelo")

shap = shapiro.test(sumafit$residuals)

verificacao_normalidade = paste("Realizamos o teste de shapiro-wilk (W=",round(shap$statistic,2),", p-valor=",pvalor(shap$p.value),"), ",ifelse(shap$p.value<0.05,"o que significa que rejeitamos a suposição de normalidade dos residuos, violando a suposicao do modelo.","o que significa que não rejeitamos a suposicao de normalidade dos residuos, atendendo a suposicao do modelo."),sep="")


resultado = list("O modelo que melhor se ajustou foi: \n",equacao,"Podemos verificar mais detalhes sobre os coeficientes na tabela a seguir: \n",tabela,explicacao_tabela,"É possível visualizar esses resultados no gráfico seguinte: \n", grafico, "Uma das suposições do modelo é que os resíduos tenham uma distribuição normal.",verificacao_normalidade, "Como podemos ver no grafico de residuos", grafico_residuos)

return(resultado)}
