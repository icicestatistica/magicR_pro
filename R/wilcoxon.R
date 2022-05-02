wilcox = function(vec1_emordem, vec2_emordem,niveisemordem, nomey="Escore", idioma="PT"){

t3 = wilcox.test(vec1_emordem,vec2_emordem, paired=T)

d= data.frame("ID"=rep(1:length(vec1_emordem),times=2),"Tempo"=rep(niveisemordem, each=length(vec1_emordem)),"Escore"=c(vec1_emordem,vec2_emordem))
d$Tempo = factor(d$Tempo, levels = niveisemordem)

if(idioma=="PT") textos = c("Comparação de ", "Teste de Wilcoxon (V=", " p-valor=") else 
if(idioma=="EN") textos= c("Comparison of ","Wilcoxon's Test (V="," p-value=")
 
grafico = ggplot(d, aes(y=Escore, x=Tempo)) +  geom_point() + geom_line(aes(group=ID)) + theme_clean() + 
labs(title=vetor_comsep_c(paste0(textos[1],printvetor(niveisemordem)," (n=",length(unique(d$ID)),")"),50), 
subtitle=paste0(textos[2],t3$statistic,textos[3],ifelse(t3$p.value<0.001,"<0.001",round(t3$p.value,3)),")"), y=nomey) +
 theme(plot.title=element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5), axis.title.x=element_blank(), plot.background = element_rect(colour = "white")) +
 scale_y_continuous(limits=c(0,max(d$Escore)))

return(grafico)}
