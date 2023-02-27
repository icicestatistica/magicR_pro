analise_sensibilidade = function(padrao_ouro,predito,nomepadraoouro,nomepredito,niveis=c("Positivo","Negativo")){

predito = factor(predito, levels=niveis)
padrao_ouro = factor(padrao_ouro, levels=niveis)

df = data.frame(table(predito,padrao_ouro))
acuracia = 100*round((df[df$predito=="Positivo" & df$padrao_ouro=="Positivo",]$Freq + df[df$predito=="Negativo" & df$padrao_ouro=="Negativo",]$Freq)/sum(df$Freq),3)
sensibilidade = 100*round(df[df$predito=="Positivo" & df$padrao_ouro=="Positivo",]$Freq/ sum(df[df$padrao_ouro=="Positivo",]$Freq),3)
especificidade = 100*round(df[df$predito=="Negativo" & df$padrao_ouro=="Negativo",]$Freq/ sum(df[df$padrao_ouro=="Negativo",]$Freq),3)
VPP = 100*round(df$Freq[df$predito=="Positivo" & df$padrao_ouro=="Positivo"]/sum(df$Freq[df$predito=="Positivo"]),3)
VPN = 100*round(df$Freq[df$predito=="Negativo" & df$padrao_ouro=="Negativo"]/sum(df$Freq[df$predito=="Negativo"]),3)

row.names(df) = c("Positivo certo","Falso negativo","Falso positivo","Negativo certo")

res = data.frame("Teste"=nomepredito,"n"=sum(df$Freq),t(df$Freq),"Acurácia"=acuracia,"Especificidade"=especificidade,"Sensibilidade"=sensibilidade,"VPP"=VPP,"VPN"=VPN)
names(res)=c("Teste","n",row.names(df),"Acurácia","Especificidade","Sensibilidade","VPP","VPN")

texto = paste0("Foram avaliados ",res$n," sujeitos. Entre eles, ",res$`Positivo certo`," foram classificados corretamente como positivos e ",res$`Negativo certo`," foram classificados corretamente como negativos, portanto ",sum(res$`Positivo certo` + res$`Negativo certo`)," foram classificados corretamente, com uma acurácia de ",res$`Acurácia`,"%. Dos ",res$`Positivo certo` + res$`Falso negativo`," que deveriam ter sido detectados, apenas ",res$`Positivo certo`," foram, portanto a sensibilidade foi ",res$`Sensibilidade`,"%. De forma análoga, verificamos que dos ",res$`Negativo certo` + res$`Falso positivo`," que deveriam ter sido identificados como negativos, apenas ",res$`Negativo certo`," foram, portanto a especificidade foi ",res$`Especificidade`,"%. Calculamos também a proporção dos ",res$`Falso negativo` + res$`Negativo certo`," que foram classificados negativos e de fato eram, a saber, o VPN (Valor predito negativo) que foi igual a ",VPN,"% nesse teste, bem como o VPP (valor predito positivo), que indica a proporção dos ",res$`Positivo certo` + res$`Falso positivo`," sujeitos que de fato eram positivos, que foi igual a ",res$`VPP`,"%.")

grafico = grafico_catcat(padrao_ouro,nomepadraoouro,predito,nomepredito,texto = NULL) + labs(title=vetor_comsep_c(paste0("Distribuição de frequências de resultados para o teste ",nomepredito," em relação ao ",nomepadraoouro, " (n=",sum(df$Freq),")"),40))

resumo = paste0("Dos ",res$n," sujeitos avaliados, ",sum(res$`Positivo certo` + res$`Negativo certo`)," foram classificados corretamente, com uma acurácia de ",res$`Acurácia`,"%. A sensibilidade foi ",res$`Sensibilidade`,"% e a especificidade foi ",res$`Especificidade`,"%.")

testes = data.frame(Nome1 = nomepadraoouro, Nome2 = nomepredito, tipo = "sensi", sig_ou_não = NA, resumo = resumo, sup = NA)

return(list("testes"=testes,"resultado"=res,"grafico"=grafico,"texto"=texto))}

analise_sensibilidade_meta = function(){
  texto="Essas são medidas de avaliação comuns usadas em estatística para análise de adequabilidade de um método de diagnóstico com relação a um método já estabelecido, tido como padrão-ouro:

- Acurácia: A acurácia é a proporção de sujeitos classificados corretamente em relação ao total de sujeitos;
- Sensibilidade: A sensibilidade é a proporção de sujeitos positivos que foram corretamente identificados em relação ao total de sujeitos que de fato eram positivos;
- Especificidade: A especificidade é a proporção de sujeitos negativos que foram corretamente identificados em relação ao total desujeitos que de fato eram negativos;
- Valor predito positivo: O valor predito positivo é a proporção de sujeitos positivos verdadeiros em relação ao total de sujeitos classificados como positivos pelo teste;
- Valor predito negativo: O valor predito negativo é a proporção de sujeitos negativos verdadeiros em relação ao total de sujeitos classificados como negativos pelo teste. \n"
  return(list("texto"=texto))
}
