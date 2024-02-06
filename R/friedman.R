friedman_icic = function(id,time,num,nomex,moms,nometime="Momento"){

df=data.frame(id,time,num)
df = df[df$time %in% moms,]

df_wide = tidyr::pivot_wider(df,names_from = time,values_from = num)

df_wide=na.omit(df_wide)

df_long = tidyr::pivot_longer(df_wide,2:dim(df_wide)[2],values_to = "num",names_to="time")
df_long$time=factor(df_long$time, levels=moms)
df_long$id = factor(df_long$id)

res.fried <- rstatix::friedman_test(num ~ time |id, data=df_long)

es = rstatix::friedman_effsize(num ~ time |id,data=df_long)

pwc <- df_long %>%
  wilcox_test(num ~ time, paired = TRUE, p.adjust.method = "bonferroni")

n=length(pwc$p)

a1 = res.fried$df
a2 = unname(round(res.fried$statistic, dig))
a3 = ifelse(res.fried$p < 0.001, "<0.001", paste0("=",round(res.fried$p, 
        3)))
textograf <- substitute(paste("Teste de Friedman (", chi^2, 
"(", a1, ") =", a2, ",p", a3, ")", collapse = ""), list(a1 = a1, a2 = a2, a3 = a3))
desc_pw = paste(pwc$group1," e ",pwc$group2," (p=",pvetor(pwc$p.adj),")",sep="")

if(res.fried$p<0.05) comp = ifelse(sum(pwc$p.adj<0.05)==0,
                            paste0("Apesar disso, os testes de postos sinalizados de wilcoxon com correção de bonferroni não detectaram nenhum momento diferindo dos demais (",printvetor(desc_pw,aspas=F),")."),
       ifelse(sum(pwc$p.adj<0.05)==n,paste0("Os testes de postos sinalizados de wilcoxon com correção de bonferroni detectaram diferenças de distribuições em todos os momentos (",printvetor(desc_pw,aspas=F),")."),
            paste0("Os momentos que diferiram pelos testes de postos sinalizados de wilcoxon com correção de bonferroni foram ",printvetor(desc_pw[pwc$p.adj<0.05],aspas=F)," e os que não diferiram foram ",printvetor(desc_pw[pwc$p.adj>=0.05],aspas=F),". \n"))) else comp=NULL


dif = ifelse(res.fried$p<0.05," foi estatisticamente diferente"," não foi estatisticamente diferente")

texto = paste(" - A variável '", nomex, "'",dif," nos momentos ao nível de 5% de significância  usando o teste de Friedman ($\\chi^2$ (", a1, ") =", a2, ",p", a3, "), com efeito ",es$method,"=",round(unname(es$effsize),2),", que pode ser considerado um efeito ",ifelse(es$magnitude=="small","pequeno",ifelse(es$magnitude=="moderate","moderado","grande")),". ",comp,sep="")

textoresumo = str_sub(texto,start=4)
  
pwc <- pwc %>% add_xy_position(x = "time")
grafico = grafico_comp_box_pareado(df_long$id, df_long$num,nomex, factor(df_long$time, levels=moms), nometime,moms=moms,teste=textograf) + scale_y_continuous(limits=c(0,max(pwc$y.position))) +
        ggpubr::stat_pvalue_manual(pwc, hide.ns = TRUE)

res=desc_bi_cont(df_long$num,df_long$time,respcol=F)[-1,-2]

res <- cbind(rbind(c(paste("**", nomex, "** (", dim(df_wide)[1], ")", sep = ""), 
        rep("", dim(res)[2]-1)), res), `p-valor` = c("", paste0(pvalor(res.fried$p),"i"), rep("", 
        dim(res)[1] - 1)))
  
 testes = data.frame(Nome1 = nometime, Nome2 = nomex, tipo = "fried", sig_ou_não = ifelse(res.fried$p < 0.05, T, F), resumo = textoresumo, sup = NA)

return(list("testes"=testes,"result"=res,"texto"=texto,"grafico"=grafico))}
