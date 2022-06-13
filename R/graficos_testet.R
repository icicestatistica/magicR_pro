grafico_correl <- function(conty,nomey,cor,contx,nomex,text){
dadosd <- data.frame(conty,contx)
plot = ggplot(dadosd, aes(y=conty,x=contx)) + geom_point() + theme_clean() + geom_smooth(color=cor,method="gam", fullrange=T, span=1) + xlab(nomex) + ylab(nomey) +
  ggtitle(vetor_comsep_c(paste0("Correlação entre \'",nomex,"\' e \'",nomey,"\' (n=",dim(na.omit(dadosd))[1],")",collapse=""),40), subtitle=text) +
  theme(plot.background = element_rect(colour=NA, fill = "transparent"),
  panel.background = element_rect(fill = "transparent", color=NA),
  plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
return(plot)}

grafico_comp_bar <- function (cont, nomecont, cat, nomecat,cor="cyan4",teste="",dig=2, ordenar=T, idioma="PT") 
{
niveis = names(table(cat))
    dadosd = data.frame(cont = cont, cat = cat)
    n = table(dadosd$cat)
    dadosd$cat <- factor(dadosd$cat)
    levels(dadosd$cat) = paste(niveis, "\n n=", n, sep = "")
    df.summary <-  
        na.omit(dadosd) %>% group_by(factor(cat)) %>% dplyr::summarise(sd = sd(cont, 
            na.rm = TRUE), mean = mean(cont))
    names(df.summary)= c("cat","sd","mean")
   if(ordenar==T) {df.summary$cat <- factor(df.summary$cat, levels = df.summary$cat[order(df.summary$mean)])}
  
 titulo = ifelse(idioma=="PT",paste0("Comparação de médias de '", nomecont, "' por '", nomecat, "' (n=", dim(na.omit(dadosd))[1], ")", collapse = ""),
 paste0("comparison of means '", nomecont, "' by '", nomecat, "' (n=", dim(na.omit(dadosd))[1], ")", collapse = ""))
  
    plot = ggplot() + theme_clean() + geom_bar(df.summary, mapping = aes(cat, 
        mean), stat = "identity", fill = cor, color = "black", 
        width = 0.8) + ggtitle(vetor_comsep_c(titulo, 40), subtitle = teste) + 
        geom_errorbar(df.summary, mapping = aes(x = cat, ymin = mean - 
            sd, ymax = mean + sd), width = 0.1, size = 1) + geom_label(df.summary, 
        mapping = aes(x = cat, y = mean, label = round(mean, 
            dig))) + ylab(nomecont) + xlab(nomecat) + theme(plot.background = element_rect(colour=NA, fill = "transparent"),
  panel.background = element_rect(fill = "transparent", color=NA), 
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    return(plot)
}

grafico_comp_box <- function(cont,nomecont,cat,nomecat,cor="cyan4",teste="",dig=2,ordenar=T){
  dadosd <- data.frame(cont=cont,cat=cat)
  dadosd <- na.omit(dadosd)
  niveis=names(table(cat))
  n=table(dadosd$cat)
  dadosd$cat <- factor(dadosd$cat)
  levels(dadosd$cat) = paste(niveis,"\n n=",n, sep="")
  df.summary = dadosd %>% group_by(cat) %>% dplyr::summarise("med"=median(cont, na.rm=T),"q3"=quantile(cont,0.75, na.rm=T))
  if(ordenar==F) x_c="cat" else x_c="reorder(cat,cont,FUN=median)"
  if(sum(n)>200) {
  plot=ggplot(dadosd  %>% filter(!is.na(cat)),mapping=aes(y=cont,x=eval(parse(text=x_c)))) + 
    geom_boxplot(fill=cor) +
    ylab(vetor_comsep_c(nomecont,40)) + xlab(vetor_comsep_c(nomecat,50)) + theme_clean() +
    #geom_text(df.summary, mapping=aes(y=q3,x=cat,label="letrinhas"))+
    ggtitle(vetor_comsep_c(paste0("Comparação de distribuições de \'",nomecont,"\' por \'",nomecat,"\' (n=",dim(na.omit(dadosd))[1],")",collapse=""),40),subtitle = teste) +
    theme(plot.background = element_rect(colour=NA, fill = "transparent"),
  panel.background = element_rect(fill = "transparent", color=NA), plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))} else
   {plot=ggplot(dadosd  %>% filter(!is.na(cat)),mapping=aes(y=cont,x=eval(parse(text=x_c)))) + 
    geom_boxplot(fill=cor, outlier.alpha = 0) +
    geom_dotplot(binaxis='y', stackdir='center') +
    ylab(vetor_comsep_c(nomecont,40)) + xlab(vetor_comsep_c(nomecat,50)) + theme_clean() +
    #geom_text(df.summary, mapping=aes(y=q3,x=cat,label="letrinhas"))+
    ggtitle(vetor_comsep_c(paste0("Comparação de distribuições de \'",nomecont,"\' por \'",nomecat,"\' (n=",dim(na.omit(dadosd))[1],")",collapse=""),40),subtitle = teste) +
    theme(plot.background = element_rect(colour=NA, fill = "transparent"),
  panel.background = element_rect(fill = "transparent", color=NA), plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))}
return(plot)}


grafico_catcat <- function(x,nomex,y,nomey,cor="cyan4",texto="", idioma="PT"){
help=data.frame(x,y)
names(help) = c("x","y")
tabela <- table(help$x, help$y)
d_completo <- data.frame(tabela,prop.table(tabela,1))
d_completo$lab = paste0(d_completo$Freq," (",round(100*d_completo$Freq.1,1),"%)")
d_completo$Var1 <- factor(d_completo$Var1)
levels(d_completo$Var1) = paste0(levels(d_completo$Var1),"\n n=",table(help$x))
  
titulo = ifelse(idioma=="PT",paste0("Associação entre \'",nomex,"\' e \'",nomey,"\' (n=",dim(na.omit(help))[1],")"),
paste0("Association between \'",nomex,"\' and \'",nomey,"\' (n=",dim(na.omit(help))[1],")"))
nome_eixo_y=ifelse(idioma=="PT","Frequência","Frequency")

plot=ggplot(d_completo,aes(x=Var1, y=Freq.1, fill=Var2)) + geom_bar(stat="identity") +
theme_clean() + scale_y_continuous(labels = scales::percent) +
  geom_text(label=d_completo$lab, position = position_stack(vjust = 0.5)) +
labs(title=vetor_comsep_c(titulo,40),subtitle=texto,y=nome_eixo_y, x=vetor_comsep(nomex,8), fill="") +
scale_fill_manual(labels = vetor_comsep(names(table(y)),3),values=lighten(cor,seq(0,(1-1/(length(table(help$y)))),1/(length(table(help$y)))))) +
theme(plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  legend.background = element_rect(color = NA, fill = "transparent"),
  plot.background = element_rect(colour=NA, fill = "transparent"),
  panel.background = element_rect(fill = "transparent", color=NA))
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
