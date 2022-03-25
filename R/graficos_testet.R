grafico_comp_bar <- function (cont, nomecont, cor, cat, nomecat,teste) 
{niveis=names(table(cat))
  tam = table(cat)
  dados = data.frame(cont = cont, cat = cat)
    df.summary <- data.frame("cat"=niveis,dados %>% group_by(factor(cat)) %>% dplyr::summarise(sd = sd(cont, 
        na.rm = TRUE), mean = mean(cont)))
    df.summary$cat <- factor(df.summary$cat, levels=df.summary$cat[order(df.summary$mean)])
    ggplot(df.summary, aes(cat, mean)) + theme_clean() + geom_bar(na.rm = TRUE, 
        stat = "identity", fill = cor, color = "black", 
        width = 0.8)  + 
        scale_x_discrete(label=paste(niveis,"\n n=",tam, sep="")) +
        ggtitle(paste0("Comparação de médias de \'",nomecont,"\' por \'",nomecat,"\'",collapse=""),subtitle=teste) +
        geom_errorbar(aes(x = cat, ymin = mean - 
        sd, ymax = mean + sd), width = 0.1, size = 1) +
        geom_label(aes(x = cat, y = mean, label = round(mean, 
            dig))) +
        ylab(nomecont) + xlab(nomecat)
return(plot)}

grafico_comp_box <- function(cont,nomecont,cor,cat,nomecat,teste){
  dadosd <- data.frame(cont=cont,cat=cat)
  tam=table(cat)
  niveis=names(table(cat))
  df.summary = dadosd %>% group_by(cat) %>% dplyr::summarise("med"=median(cont, na.rm=T),"q3"=quantile(cont,0.75))
  plot=ggplot() + 
    scale_x_discrete(label=paste(niveis,"\n n=",tam, sep="")) +
    geom_boxplot(dadosd  %>% filter(!is.na(cat)),mapping=aes(y=cont,x=reorder(cat,cont,FUN=median)),fill=cor) +
    geom_jitter(aes(y=cont,x=reorder(cat,cont,FUN=median)),width=0.2) +
    ylab(nomecont) + xlab(nomecat) + theme_clean() +
    #geom_text(df.summary, mapping=aes(y=q3,x=cat,label="letrinhas"))+
    ggtitle(paste0("Comparação de distribuições de \'",nomecont,"\' por \'",nomecat,"\'",collapse=""),subtitle = teste)
return(plot)}

grafico_teste_t_3 <- function(cont,nomecont,cores,cat,nomecat,niveis){
  
  d=data.frame(cont=cont,cat=factor(cat,levels=niveis,ordered=T))
  grupo1=d$cont[d$cat==niveis[1]]
  grupo2=d$cont[d$cat==niveis[2]]
  aux=data.frame(cat=niveis,n=c(paste0("n=",length(grupo1)),paste0("n=",length(grupo2))),max=c(max(grupo1)+1.5*sd(grupo1),max(grupo2)+1.5*sd(grupo2)),
                 media=c(mean(grupo1),mean(grupo2)),
                 dp=c(sd(grupo1),sd(grupo2)),
                 lugar=c(mean(grupo1)+2.5*sd(grupo1),mean(grupo2)+2.5*sd(grupo2)),
                 cor=cores,altura=c(unlist(quantile(density(grupo1)$y,0.75)),unlist(quantile(density(grupo2)$y,0.75))))
  row.names(aux)=niveis
  aux$cat <- factor(aux$cat, levels=niveis)
  
  ggplot(d,aes(x=cont,fill=cat)) + facet_grid(cat~.) +
    geom_histogram(aes(y=..density..,fill=cat), alpha=0.7,bins=30) +
    scale_fill_manual(values=setNames(cores,nm=niveis)) + 
    geom_density(alpha=0.5,colour=NA) +
    xlab(nomecont) + 
    geom_text(data=aux,aes(x=lugar,y=altura,label=cat,colour=cat),size=6,fontface="bold") +
    geom_text(data=aux,aes(x=max,y=0,label=n,colour=cat),size=4,fontface="bold") +
    geom_errorbarh(data=aux,aes(xmax = media+dp, xmin = media-dp,y=0),inherit.aes = F,height=max(density(d$cont)$y)/5) +
    geom_label(data=aux,aes(x=media,y=0,label=paste("Média \n",round(media,2)),fill=cat),size=4,lineheight=.8) +
    scale_color_manual(values=darken(cores,0.5)) + 
    theme_clean() + theme(legend.position = "none", plot.background = element_rect(colour="white"),strip.text.y = element_blank(),
                          axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank(),
                          axis.line.y = element_blank(),
                          plot.margin = unit(c(0,0,1,0), "cm"),
                          panel.grid.major.x=element_line(colour="gray"),
                          panel.grid.minor.x=element_line(colour="lightgray"),
                          panel.grid.major.y=element_blank())}
