escolha_summary_para_juntar <- function(x,nomesx,tipox,niveisx,dig){
if (tipox=="factor") result=desc_uni_categorica(x,eval(parse(text=niveisx)),T,T,T,F,dig)[,c(1,4)] else
  if (tipox=="ordinal") result=desc_uni_categorica(x,eval(parse(text=niveisx)),T,T,F,F,dig)[,c(1,4)] else
    result=desc_uni_continua(x,dig)
result=data.frame(result)
      result = cbind(c(nomesx,rep("",dim(result)[1]-1)),result)
      names(result) = c("Variável","Característica","Estatística")
      return(result)}

get_summary_2 <- function(x,nomesx,tipox,niveisx,dig){
  cont=0
  xdim <- dim(x)[2]
  cont=cont+1
  result <- escolha_summary_para_juntar(x[,1],nomesx[1],tipox[1],niveisx[1],dig)
  if (xdim>1){
  for (i in 2:xdim){
    cont=cont+1
    result <- rbind(result,escolha_summary_para_juntar(x[,i],nomesx[i],tipox[i],niveisx[i],dig))}}
row.names(result) <- 1:dim(result)[1]
return(list("testes"=c(cont,0,0,0,0),"result"=result))}

get_summary <- function(dados,gr,auxiliar,dig){
x <- dados[,gr]
nomesx <- auxiliar[gr,2]
tipox <- auxiliar[gr,3]
niveisx <- auxiliar[gr,4]
resultados = get_summary_2(x,nomesx,tipox,niveisx,testes,dig)
return(list("testes"=resultados$testes,"result"=resultados$result))}
