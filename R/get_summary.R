escolha_summary_para_juntar <- function(x,nomesx,tipox,niveisx,nas,teste,grafico,cor,bins,dig,idioma){
result1=NULL
result2=NULL
if (tipox=="factor") {resulta=desc_uni_categorica(x,nomesx,eval(parse(text=niveisx)),nas,T,T,T,teste,grafico,cor,dig)
                      result1=resulta$result[,c(1:3,5)]
                      texto=resulta$texto
                      tabela=resulta$tabela
                      grafico=resulta$grafico} else
  if (tipox=="ordinal") {resulta=desc_uni_categorica(x,nomesx,eval(parse(text=niveisx)),nas,T,F,T,teste,grafico,cor,dig)
                        result1=resulta$result[,c(1:3,5)]
                        texto=resulta$texto
                        tabela=resulta$tabela
                        grafico=resulta$grafico} else
    if (tipox=="numeric") {resulta=desc_uni_continua(x,nomesx,bins,teste,grafico,cor,dig,idioma)
                          result2=resulta$result
                          texto=resulta$texto
                          tabela=NULL
                          grafico=resulta$grafico}

if(is.null(result1)==F) {result1=data.frame(result1)
                        result1 = cbind(c(nomesx,rep("",dim(result1)[1]-1)),result1)
                        names(result1) = c("Variável","Característica","Frequência","Freq. Relativa","Freq. Acumulada")}
  
if(is.null(result2)==F) {result2=data.frame(result2)
                         result2 = cbind(c(nomesx,rep("",dim(result2)[1]-1)),result2)
                         names(result2) = c("Variável","Característica","Estatística")}
  
return(list("result1"=result1,"result2"=result2,"texto"=texto,"tabela"=tabela,"grafico"=grafico))}

get_summary_2 <- function(x,nomesx,tipox,niveisx,nas=F,teste=F,grafico=T,cor="cyan4",bins=20,dig=2,idioma="PT"){
  result1=NULL; result2=NULL; complem=list()
  xdim <- dim(x)[2]
  for (i in 1:xdim){
    resulta=escolha_summary_para_juntar(x[,i],nomesx[i],tipox[i],niveisx[i],nas,teste,grafico,cor,bins,dig,idioma)
    if(is.null(resulta$result1)==F) {
        if(is.null(result1)==F) {result1 <- rbind(result1,resulta$result1)
                                 row.names(result1) <- 1:dim(result1)[1]} else
              {result1=resulta$result1
               row.names(result1) <- 1:dim(result1)[1]}}
    if(is.null(resulta$result2)==F) {
        if(is.null(result2)==F) {result2 <- rbind(result2,resulta$result2)
                                 row.names(result2) <- 1:dim(result2)[1]} else
              {result2=resulta$result2
               row.names(result2) <- 1:dim(result2)[1]}}
    complem <- list.append(complem,resulta$grafico,"\n",resulta$texto,resulta$tabela,"\n")}
testes=data.frame("Nome1"=nomesx,"Nome2"="","tipo"=tipox,"sig_ou_não"="","resumo"="")
return(list("testes"=testes,
            "result1"=result1,
            "result2"=result2,
            "complem"=complem))}

get_summary <- function(dados,auxiliar,gr='auto',nas=F,teste=F,grafico=T,cor="cyan4",bins=20,dig=2, idioma="PT"){
if(gr=='auto') gr = which(auxiliar$tipo %in% c("factor","numeric","ordinal"))
x <- data.frame(dados[,gr])
nomesx <- auxiliar[gr,2]
tipox <- auxiliar[gr,3]
niveisx <- auxiliar[gr,4]
resultados = get_summary_2(x,nomesx,tipox,niveisx,nas,teste,grafico,cor,bins,dig,idioma)
return(list("testes"=resultados$testes,"result1"=resultados$result1,"result2"=resultados$result2,"complem"=resultados$complem))}
