wilcox = function(vec1_emordem, vec2_emordem,niveisemordem, nomey="Escore"){

t3 = wilcox.test(vec1_emordem,vec2_emordem, paired=T)

d= data.frame("ID"=rep(1:length(vec1_emordem),times=2),"Tempo"=rep(niveisemordem, each=length(vec1_emordem)),"Escore"=c(vec1_emordem,vec2_emordem))
d$Tempo = factor(d$Tempo, levels = niveisemordem)

grafico = ggplot(d, aes(y=Escore, x=Tempo)) +  geom_point() + geom_line(aes(group=ID)) + theme_clean() + labs(title=vetor_comsep_c(paste0("Comparação de ",printvetor(niveisemordem)," (n=",length(unique(d$ID)),")"),65), subtitle=paste0("Teste de Wilcoxon (V=",t3$statistic,", p-valor=",ifelse(t3$p.value<0.001,"<0.001",round(t3$p.value,3)),")"), y=nomey) + theme(plot.title=element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), axis.title.x=element_blank()) + scale_y_continuous(limits=c(0,max(d$Escore)))

return(grafico)}
