mc_icic = function(id,time,cat,moms,nomex,nometime){

df=data.frame(id,time,cat)

df = df[df$time %in% moms,]

df_wide = tidyr::pivot_wider(df,names_from = time,values_from = cat)

df_wide=na.omit(df_wide)

df_long = tidyr::pivot_longer(df_wide,2:(dim(df_wide)[2]),values_to = "cat",names_to="time")
df_long$time=factor(df_long$time)
df_long$id = factor(df_long$id)
df_long$cat = factor(df_long$cat)

mc = rstatix::mcnemar_test(table(df_wide[,moms]))

a1 = unname(mc$df)
a2 = unname(round(mc$statistic, dig))
a3 = ifelse(mc$p < 0.001, "<0.001", paste0("=",round(mc$p,3)))

textograf <- substitute(paste("Teste de McNemar (", chi^2, 
"(", a1, ") =", a2, ",p", a3, ")", collapse = ""), list(a1 = a1, a2 = a2, a3 = a3))

dif = ifelse(mc$p<0.05," teve proporções estatisticamente diferentes"," não teve proporções estatisticamente diferentes")

texto = paste(" - A variável '", nomex, "'",dif," nos momentos ao nível de 5% de significância  usando o teste de McNemar ($\\chi^2$ (", a1, ") =", a2, ",p", a3, ").",sep="",collapse="")


grafico = grafico_catcat(df_long$time,nometime,df_long$cat,nomex, texto=textograf) + labs(title=vetor_comsep_c(paste0("Diferença de proporções de ",nomex),40))

res=desc_bi_cat(df_long$cat,col=df_long$time,respcol=F)[-1,-2]

res <- cbind(rbind(c(paste("**", nomex, "** (", dim(df_wide)[1], ")", sep = ""), 
        rep("", dim(res)[2])), res), `p-valor` = c("", paste0(pvalor(mc$p),"k"), rep("", 
        dim(res)[1] - 1)))
        
testes = data.frame(Nome1 = nometime, Nome2 = nomex, tipo = "mcnem", sig_ou_não = ifelse(mc$p < 0.05, T, F), resumo = paste("$\\chi^2$ (", a1, ") =", a2, ",p", a3, collapse = ""), sup = NA)


return(list("testes"=testes,"result"=res,"texto"=texto,"grafico"=grafico))}
