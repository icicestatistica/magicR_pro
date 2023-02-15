friedman_icic = function(id,time,num,nomex){

df=data.frame(id,time,num)

df_wide = tidyr::pivot_wider(df,names_from = time,values_from = num)

df_wide=na.omit(df_wide)

df_long = tidyr::pivot_longer(df_wide,2:4,values_to = "num",names_to="time")
df_long$time=factor(df_long$time)
df_long$id = factor(df_long$id)

res.fried <- rstatix::friedman_test(num ~ time |id, data=df_long)

es = rstatix::friedman_effsize(num ~ time |id,data=df_long)

pwc <- df_long %>%
  wilcox_test(num ~ time, paired = TRUE, p.adjust.method = "bonferroni")

a1 = res.fried$df
a2 = unname(round(res.fried$statistic, dig))
a3 = ifelse(res.fried$p < 0.001, "<0.001", paste0("=",round(res.fried$p, 
        3)))
textograf <- substitute(paste("Teste de Friedman (", chi^2, 
"(", a1, ") =", a2, ",p", a3, ")", collapse = ""), list(a1 = a1, a2 = a2, a3 = a3))
desc_pw = paste(pwc$group1," e ",pwc$group2," (p=",pvetor(pwc$p.adj),")",sep="")

texto = paste(" - A variável '",nomex,"' foi estatisticamente diferente nos momentos ao nível de 5% de significância  usando o teste de Friedman ($\\chi^2$ (", a1, ") =", a2, ",p", a3, "), com efeito ",es$method,"=",round(unname(es$effsize),2),", que pode ser considerado um efeito ",ifelse(es$magnitude=="small","pequena",ifelse(es$magnitude=="moderate","moderada","grande")),". \n O teste de posto sinalizado de Wilcoxon pareado entre os grupos revelou diferenças estatisticamente significativas entre os momentos ",paste0(paste(desc_pw[-length(desc_pw)],collapse=", ")," e ",desc_pw[length(desc_pw)]),".",sep="")

pwc <- pwc %>% add_xy_position(x = "time")
grafico = ggpubr::ggboxplot(df_long, x = "time", y = "num") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = textograf,
    caption = get_pwc_label(pwc),
    y=nomex)

res=desc_bi_cont(df_long$num,df_long$time,respcol=F)[-1,-2]

res <- cbind(rbind(c(paste("**", nomex, "** (", dim(df_wide)[1], ")", sep = ""), 
        rep("", dim(res)[2])), res), `p-valor` = c("", paste0(a3,"i"), rep("", 
        dim(res)[1] - 1)))

return(list("result"=res,"texto"=texto,"grafico"=grafico))}
