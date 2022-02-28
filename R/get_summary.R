escolha_summary_para_juntar <- function(x,nomesx,tipox,niveisx,nas,teste,grafico,cor,dig){
if (tipox=="factor") {resulta=desc_uni_categorica(x,nomesx,eval(parse(text=niveisx)),nas,T,T,F,teste,grafico,cor,dig)
                      result=resulta$result[,c(1,4)]
                      texto=resulta$texto
                      tabela=resulta$tabela
                      grafico=resulta$grafico} else
  if (tipox=="ordinal") {resulta=desc_uni_categorica(x,nomesx,eval(parse(text=niveisx)),nas,T,F,F,teste,grafico,cor,dig)
                        result=resulta$result[,c(1,4)]
                        texto=resulta$texto
                        tabela=resulta$tabela
                        grafico=resulta$grafico} else
    {result=desc_uni_continua(x,dig)
     texto=resulta$texto
     tabela=resulta$tabela
     grafico=resulta$grafico}
result=data.frame(result)
result = cbind(c(nomesx,rep("",dim(result)[1]-1)),result)
names(result) = c("Variável","Característica","Estatística")
return(list("result"=result,"texto"=texto,"tabela"=tabela,"grafico"=grafico))}

get_summary_2 <- function(x,nomesx,tipox,niveisx,nas,teste,grafico,cor,dig){
  cont=0
  xdim <- dim(x)[2]
  cont=cont+1
  resulta <- escolha_summary_para_juntar(x[,1],nomesx[1],tipox[1],niveisx[1],nas,teste,grafico,cor,dig)
  result=resulta$result
  complem=list("texto"=resulta$texto,"tabela"=resulta$tabela,"grafico"=resulta$grafico)
  if (xdim>1){
  for (i in 2:xdim){
    cont=cont+1
    resulta=escolha_summary_para_juntar(x[,i],nomesx[i],tipox[i],niveisx[i],nas,teste,grafico,cor,dig)
    result <- rbind(result,resulta$result)
    complem <- list.append(complem,resulta$texto,resulta$tabela,resulta$grafico)}}
row.names(result) <- 1:dim(result)[1]
return(list("testes"=c("desc"=cont,"catsame"=0,"t"=0,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
            "result"=result,
            "complem"=complem))}

get_summary <- function(dados,gr,auxiliar,nas,teste,grafico,cor,dig){
x <- dados[,gr]
nomesx <- auxiliar[gr,2]
tipox <- auxiliar[gr,3]
niveisx <- auxiliar[gr,4]
resultados = get_summary_2(x,nomesx,tipox,niveisx,nas,teste,grafico,cor,dig)
return(list("testes"=resultados$testes,"result"=resultados$result,"complem"=resultados$complem))}
