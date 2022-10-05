logisti = function(st){

a=jtools::summ(st, exp = T)

res=data.frame(round(summary(st)$coefficients[,1],3),round(a$coeftable[,1],3),paste0("(",round(a$coeftable[,2],3),", ",round(a$coeftable[,3],3),")"),round(a$coeftable[,4],3),pvetor(a$coeftable[,5]))
names(res)=c("Coeficiente (B)","OR","IC95%","Valor z","p-valor")

library("sjPlot")

plot = plot_model(st, grid=FALSE,show.values=T,vline.color="darkgray") + theme_icic + theme(legend.position = "top", plot.title=element_text(hjust=0.5))

return(list(res,plot))}
