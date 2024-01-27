kruskall = function (resp, fator, nomeresp, nomefator, niveis = "auto", 
    dig = 2, respcol = T, excluirtotal = T, cor = "cyan4", ordenar = F, 
    idioma = "PT", ordinal = F, labels = T, virgula = F) 
{
    stat.test=NULL
    if (respcol == T) {ref = nomefator} else ref = nomeresp
    resp = unlist(resp)
    fator = unlist(fator)
    if (niveis[1] == "auto") {niveis = names(table(fator))}
    fator <- factor(fator, levels = niveis)
    dad <- data.frame(continua = as.numeric(resp), categorica = fator)
    dad2 = data.frame(continua = resp, categorica = fator)
    a <- kruskal.test(dad$continua, dad$categorica)
    c = round(kruskal_effsize(dad, continua ~ categorica)$effsize, 
        dig)
    p = paste0(pvalor(a$p.value), "f ($\\eta^2$=", c, ")")
    if (a$p.value > 0.05) {
        tabela = NULL
        texto = c("* **", ref, "**: Não encontramos com o teste de Kruskall Wallis evidência de diferença entre os grupos (", 
            paste("$\\chi^2$", collapse = NULL), "(", a$parameter, 
            ") =", round(a$statistic, dig), ",p-valor=", pvalor(a$p.value), 
            "). \n")
        resumo_final = paste0("Não encontramos com o teste de Kruskall Wallis evidência de diferença de ", 
            nomeresp, " entre os grupos de ", nomefator, " (", 
            paste("$\\chi^2$", collapse = NULL), "(", a$parameter, 
            ")=", round(a$statistic, dig), ",p-valor=", pvalor(a$p.value), 
            ").")
    }
    else {
        texto = c("* **", ref, "**: O teste de Kruskall-Wallis mostrou que há diferença entre pelo menos um dos grupos (", 
            paste("$\\chi^2$", collapse = NULL), "(", a$parameter, 
            ") =", round(a$statistic, dig), ",p-valor=", pvalor(a$p.value), 
            ").")
        resumo_final = paste0("O teste de Kruskall-Wallis mostrou que há diferença de ", 
            nomeresp, " entre pelo menos um dos grupos de ", nomefator, " (", 
            paste("$\\chi^2$", collapse = NULL), "(", a$parameter, 
            ")=", round(a$statistic, dig), ",p-valor=", pvalor(a$p.value), 
            ").")
        dunn <- quiet(dunn.test(dad$continua, dad$categorica, 
            method = "bonferroni", kw = F, table = F, list = F))

        difs = matrix(unlist(str_split(dunn$comparisons, 
            " - ")), ncol = 2, byrow = T)

        b <- data.frame(difs,dunn$comparisons, dunn$Z,dunn$P.adjusted)
        b$signif = ifelse(b$dunn.P.adjusted<0.001,"***",ifelse(b$dunn.P.adjusted<0.01,"**",ifelse(b$dunn.P.adjusted<0.05,"*","")))

        ordenando = dad %>% group_by(categorica) %>% summarise("mediana"=median(continua,na.rm=T)) %>% arrange(mediana)
        ord = ordenando$categorica
        
        stat.test = b
        names(stat.test)[1:2]=c("group1","group2")
        stat.test=stat.test[stat.test$dunn.P.adjusted<0.05,]
        
        stat.test.c = data.frame()
        for (i in ord) {stat.test.c = rbind(stat.test.c, stat.test[str_detect(stat.test$dunn.comparisons,i),]);
                       stat.test.c = stat.test.c %>% sort(abs(dunn.Z))}
        stat.test = unique(stat.test.c)
        
        jafoi=c()

        if(sum(b$dunn.P.adjusted<0.05)==length(b$dunn.P.adjusted)) {comparacoes = "Através das comparações múltiplas de Dunn, encontramos diferença entre todos os grupos."} else {
          if(sum(b$dunn.P.adjusted<0.05)==0) {comparacoes = "Apesar disso, através dos testes de comparação pareada de Dunn, não encontramos diferença estatística entre nenhum dos grupos estudados."} else {
            comparacoes = "Seguindo para as comparações múltiplas de Dunn com correção de Bonferroni, concluímos que "

              for (ni in niveis[as.numeric(ord)][-length(ord)]) {
                  linhas = which((b$X1==ni | b$X2==ni) & ((b$X1 %in% jafoi)==F & (b$X2 %in% jafoi)==F))
                  ger = b[linhas,]
                  ger$grupo = apply(ger[,1:2],1,function(x) x[which(x!=ni)])
                  signi = which(ger$dunn.P.adjusted<0.05)
                  if(length(signi)==0) {comparacoes = c(comparacoes,paste0(ni," não difere de ",printvetor(paste0(ger$grupo," (p=",pvetor(ger$dunn.P.adjusted),")"),aspas=F),"."))} else {
                      if(length(signi)==length(ger$grupo) & length(ger$grupo>1)) {comparacoes = c(comparacoes,paste0(ni," é menor que ",printvetor(paste0(ger$grupo," (p=",pvetor(ger$dunn.P.adjusted),")"),aspas=F),"."))} else {
  maio = ger$grupo[signi]; pmaio = pvetor(ger$dunn.P.adjusted[signi])
  naodif = ger$grupo[-signi] ; pnaodif = pvetor(ger$dunn.P.adjusted[-signi])
  comparacoes = c(comparacoes,paste0(ni," é menor que ",printvetor(paste0(maio," (p=",pmaio,")"),aspas=F),", mas não difere de ",printvetor(paste0(naodif," (p=",pnaodif,")"),aspas=F),"."))}
  }
              jafoi=c(jafoi,ni)}
          }
          }
        tex = paste0(comparacoes,collapse=" ")
        texto = c(texto, tex)
    }
    if (ordinal == F) 
        res = desc_bi_cont(dad$continua, dad$categorica, F, respcol, 
            F, dig)
    else {
        if (respcol == F) 
            res = desc_bi_cat(linha = dad2$continua, col = dad2$categorica, 
                respcol = F)
        else res = desc_bi_cat(linha = dad2$categorica, col = dad2$continua, 
            respcol = T)
    }
    tot = dim(na.omit(dad))[1]
    if (excluirtotal == T) 
        res = res[-1, ]
    res <- cbind(rbind(c(paste("**", ref, "** (", tot, ")", sep = ""), 
        rep("", dim(res)[2] - 1)), res), `p-valor` = c("", p, 
        rep("", dim(res)[1] - 1)))
texto = paste(texto, collapse = "")
    a1 = a$parameter
    a2 = round(a$statistic, dig)
    a3 = ifelse(a$p.value < 0.001, "<0.001", round(a$p.value, 
        3))
    textograf <- substitute(paste("Kruskall-Wallis (", chi^2, 
        "(", a1, ") =", a2, ",p=", a3, ")", collapse = ""), list(a1 = a1, 
        a2 = a2, a3 = a3))
    if (ordinal == F) 
        grafico = grafico_comp_box(dad$continua, nomeresp, dad$categorica, 
            nomefator, cor = cor, textograf, dig, ordenar, idioma,stat.test = stat.test)
    else grafico = grafico_catcat(dad2$categorica, nomefator, 
        dad2$continua, nomeresp, cor = cor, textograf, idioma, 
        labels = labels, virgula) + coord_flip()
    testes = data.frame(Nome1 = nomeresp, Nome2 = nomefator, 
        tipo = "kw", sig_ou_não = ifelse(a$p.value < 0.05, T, 
            F), resumo = resumo_final, sup = NA)
    return(list(testes = testes,
                result = res,
                texto = texto, 
                grafico = grafico))
}
