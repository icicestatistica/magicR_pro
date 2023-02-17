#ICC

icc_icic = function(mat){
nomesmat = names(mat)
a=irr::icc(mat, model = "twoway", type = "agreement", unit = "single")
sn=ifelse(a$p.value<0.05,"rejeitamos","não rejeitamos")
interp = cut(a$value,c(0,0.4,0.6,0.75,1.1),right=F, labels=c("ruim","regular","bom","excelente"))
texto=paste(" - Calculamos o ICC (F(",round(a$df1,0),",",round(a$df2,0),")=",round(a$Fvalue,2), ", ICC=",round(a$value,3),", IC 95% = [",round(a$lbound,2),", ",round(a$ubound,2),"]), que pode ser interpretado, como sugere Cicchetti (1994), como uma concordância ",interp," e com p=",pvalor(a$p.value)," ",sn," a nulidade de Índice de Correlação Intraclasse entre ",printvetor(nomesmat,aspas=F),". \n",sep="",collapse="")
return(list("texto"=texto))}


#Cronbach

#library("ltm")
cronbach_icic = function(dad,nomescala){

a=cronbach.alpha(dad, standardized = FALSE, CI = TRUE, 
    probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

interp = cut(a$alpha,c(-0.1,0.21,0.41,0.61,0.8,1),labels=c("pequena","razoável","moderada","substancial","quase perfeita"))

texto=paste0(" - Calculamos o alpha de cronbach para a escala ",nomescala,", com ",a$p," itens aplicados em ",a$n," indivíduos: alpha=",round(a$alpha,3),", IC95% ",paste0("[",paste0(round(a$ci,3),collapse=", "),"]"),". Landis, J.R., Koch, G.G. (1997) sugerem a intepretação dessa medida como ",interp,". \n")

result=data.frame("Escala"=nomescala,"n"=a$n,"itens"=a$p,"alpha"=round(a$alpha,3),"IC95"=paste0("[",paste0(round(a$ci,3),collapse=", "),"]"),"interpretação"=interp)

return(list("result"=result,"texto"=texto))}
