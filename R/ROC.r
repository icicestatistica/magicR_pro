library("pROC")

ROC_icic = function(df$CPSUWA,df$Grupo){
df_na = na.omit(data.frame(df$CPSUWA,df$Grupo))
fit1 = glm(df.Grupo ~ df.CPSUWA, data=df_na, family=binomial(link="logit"))

fit1
roc1 = roc(predictor=fit1$fitted.values, response=factor(df_na$df.Grupo, levels=c("Controle","Paciente")),percent=TRUE, plot=TRUE, ci=TRUE, auc=T)

sensxesp = data.frame(roc1$thresholds,"sens"=roc1$sensitivities,"espec"=roc1$specificities)

kable(sensxesp)

library(ROCR)
pred1 = prediction(fit1$fitted.values,df_na$df.Grupo)
cost_perf = performance(pred1, "cost") 
cof = pred1@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]

print(paste("cof = ",round(cof,2)))

resultados = data.frame()
seque = seq(0.1,0.69,0.05)

for (i in seque) {
  
  tab = data.frame(table(ifelse(fit1$fitted.values<i,"Predito controle","Predito Paciente"),df_na$df.Grupo))

acuracia = round(100*(tab$Freq[tab$Var1=="Predito controle" & tab$Var2=="Controle"]+tab$Freq[tab$Var1=="Predito Paciente" & tab$Var2=="Paciente"])/sum(tab$Freq),2)

especificidade = round(100*(tab$Freq[tab$Var1=="Predito controle" & tab$Var2=="Controle"])/sum(tab$Freq[tab$Var2=="Controle"]),2)

sensibilidade = round(100*(tab$Freq[tab$Var1=="Predito Paciente" & tab$Var2=="Paciente"])/sum(tab$Freq[tab$Var2=="Paciente"]),2)

VPP = round(100*(tab$Freq[tab$Var1=="Predito Paciente" & tab$Var2=="Paciente"])/sum(tab$Freq[tab$Var1=="Predito Paciente"]),2)

VPN = round(100*(tab$Freq[tab$Var1=="Predito controle" & tab$Var2=="Controle"])/sum(tab$Freq[tab$Var1=="Predito controle"]),2)

resumo = data.frame("Estatísticas"=c(paste(tab$Var2," e ",tab$Var1),"Acurácia","Especificidade","Sensibilidade","VPP","VPN"),
                    "Valor"=c(round(tab$Freq,0),acuracia,especificidade,sensibilidade,VPP,VPN))

resultados = rbind(resultados,resumo[,2])
}

resultados = cbind(data.frame(seque),resultados)

names(resultados)=c("Cutoff",paste(tab$Var2," e ",tab$Var1),"Acurácia","Especificidade","Sensibilidade","VPP","VPN")

return(resultados)
}
