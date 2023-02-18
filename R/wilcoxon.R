wilcox = function(id,time,num,nomex,moms,tipox="numeric"){

id=factor(id)
time=factor(time,levels=moms)
  
if(tipox=="ordinal") {num = as.numeric(num)}

df=data.frame(id,time,num)
df = df %>% filter(time %in% moms)

df_wide = tidyr::pivot_wider(df, names_from = time, values_from = num)
df_wide = na.omit(df_wide)
df_long = tidyr::pivot_longer(df_wide, 2:dim(df_wide)[2], values_to = "num", names_to = "time")
df_long$time = factor(df_long$time, levels=moms)
df_long$id = factor(df_long$id)

stat.test <- df_long  %>%
  wilcox_test(num ~ time, paired=T) %>%
  add_significance()
#stat.test

es = df_long %>% wilcox_effsize(num ~ time,paired=T)

a2 = unname(round(stat.test$statistic, dig))
a3 = ifelse(stat.test$p < 0.001, "<0.001", paste0("=", round(stat.test$p, 
        3)))

dif = ifelse(stat.test$p<0.05," foi estatisticamente diferente"," não foi estatisticamente diferente")

texto = paste(" - A variável '", nomex, "'",dif," nos momentos ao nível de 5% de significância  usando o teste de Wilcoxon (V=", a2, ",p", a3, "), com effect size =", round(unname(es$effsize), 2), ", que pode ser considerado um efeito ", ifelse(es$magnitude == "small", "pequeno", ifelse(es$magnitude ==  "moderate", "moderado", "grande")), ".", sep = "")
    
  if(tipox=="numeric"){
    grafico = ggpubr::ggboxplot(df_long, x = "time", y = "num") + 
    labs(subtitle = get_test_label(stat.test, detailed= TRUE), 
    y = nomex, x="Momento") + theme_icic("h") + ylim(0,max(df_long$num,na.rm=T))
    
    res = desc_bi_cont(df_long$num, df_long$time, respcol = F)[-1, 
        -2]
    res <- cbind(rbind(c(paste("**", nomex, "** (", dim(df_wide)[1], 
        ")", sep = ""), rep("", dim(res)[2]-1)), res), `p-valor` = c("", 
        paste0(pvalor(stat.test$p), "j"), rep("", dim(res)[1] - 1))) } else
    
    {
    textograf <- substitute(paste("Teste de Wilcoxon (V=", a2, ",p", a3, ")", collapse = ""), list(a2 = a2, a3 = a3))
    grafico = grafico_catcat(df_long$time, "Momento", df_long$num, 
        nomex, texto = textograf) + labs(title = vetor_comsep_c(paste0("Diferença de proporções de ", 
        nomex, " nos momentos"), 40))
    res = desc_bi_cat(df_long$num, col = df_long$time, respcol=F)[-1, 
        -2]
    res <- cbind(rbind(c(paste("**", nomex, "** (", dim(df_wide)[1], 
        ")", sep = ""), rep("", dim(res)[2]-1)), res), `p-valor` = c("", 
        paste0(pvalor(stat.test$p), "j"), rep("", dim(res)[1] - 1))) }
    
  testes = data.frame(Nome1 = "Momento", Nome2 = nomex, tipo = "t_par", sig_ou_não = ifelse(stat.test$p < 0.05, T, F), resumo = paste("Teste de Wilcoxon (V=", a2, ",p", a3, ")", collapse = ""), sup = NA)
  
    return(list(testes=testes,result = res, grafico = grafico, texto = texto))}
