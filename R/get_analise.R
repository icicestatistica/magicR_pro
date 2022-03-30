escolhateste <- function(x,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal,respcol,cor){
  result<-data.frame() ; tex="" ; grafico=NULL
  t=0 ; mw=0; kw=0; aov1=0;cc=0 ; correl=0
  if(tipox=="numeric") {
    if(tipoy=="factor") {
      if(length(niveisy)==2) 
           {res = testet(x,y,nomex,nomey,niveisy,dig,F,excluirtotal)
           if(res$sup==F) {res=mann(y,x,nomey,nomex,niveisy,dig,F,excluirtotal,cor) ; mw=1} else {t=1}
           result=res$resul
           tex=res$texto
           grafico=res$grafico} else
                        {res = anovac(x,y,nomex,nomey,niveisy,dig,F,excluirtotal)
                        if(res$sup==F) {res=kruskall(x,y,nomex,nomey,niveisy,dig,F,excluirtotal,cor);kw=1} else {aov1=1}
                        result=res$result
                        tex=res$texto
                        grafico=res$grafico}
  } else
    if(tipoy=="numeric") {res=contcont(y,x,nomey,nomex,dig,cor)
                          correl=1
                          result=res$resul
                          tex=res$texto
                          grafico=res$grafico}
  } else {
  if(tipox=="factor") {
    if(tipoy=="numeric") {
      if(length(niveisx)==2)
        {res = testet(y,x,nomey,nomex,niveisx,dig,T,excluirtotal)
        if(res$sup==F) {res=mann(x,y,nomex,nomey,niveisx,dig,T,excluirtotal,cor) ; mw=1} else {t=1}
                        result=res$resul
                        tex=res$texto
                        grafico=res$grafico} else
                                          {res = anovac(y,x,nomey,nomex,niveisx,dig,T,excluirtotal)
                                           if(res$sup==F) {res=kruskall(y,x,nomey,nomex,niveisx,dig,T,excluirtotal,cor);kw=1} else {aov1=1}
                                           result=res$result
                                           tex=res$texto
                                           grafico=res$grafico}} else
     if(tipoy=="factor") {res=catcat(x,y,nomex,nomey,niveisx,niveisy,dig,respcol,excluirtotal)
                          cc=1
                          result=res$result
                          tex=res$texto}
  }}
  return(list("testes"=c("desc"=0,"catsame"=0,"t"=t,"mw"=mw,"aov1"=aov1,"kw"=kw,"correl"=correl,"cc"=cc,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
              "result"=result,
              "texto"=list("grafico"=grafico,"tex"=tex)))}



get_analise_2 <- function(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal,respcol,cor){
  xdim=dim(xmat)[2]
  texto=c() ; textocont=c()
  testes=c("desc"=0,"catsame"=0,"t"=0,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0)
  result=data.frame() ; resultcont=data.frame()
  temcont=F
  for (i in 1:xdim)
    {if(tipox[i]=="numeric" & tipoy=="numeric") {
        temcont=T
        res=escolhateste(xmat[,i],y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol,cor)
        resultcont=rbind(resultcont,res$result)
        textocont=list.append(textocont,res$texto,res$tabela)
        resumo=res$resumo
        testes <- testes+res$testes} else
          {res = escolhateste(xmat[,i],y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol,cor)
          result= rbind(result, res$result)
          texto <- list.append(texto,res$texto)
          testes <- testes+res$testes}}
  if(temcont) lista=list("testes"=testes,"result"=result,"texto"=texto,"int"=paste0("\n Agora, passamos a analisar as correlações entre as variáveis e a variável ",nomey,":  \n"),"resultcont"=resultcont,"textocont"=textocont) else lista=list("testes"=testes,"result"=result,"texto"=texto)
return(lista)}


get_analise <- function(dados,col,auxiliar,y,tipoy,nomey,niveisy,dig,excluirtotal,respcol,cor){
  xmat=dados[,col]
  tipox=auxiliar$tipo[col]
  nomex=auxiliar$nomes[col]
  niveisx=auxiliar$niveis[col]
  res = get_analise_2(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal,respcol,cor)
  if(length(res)==6) lista=list("testes"=res$testes,"result"=res$result,"texto"=res$texto,"int"=res$int,"resultcont"=res$resultcont,"textocont"=res$textocont) else lista=list("testes"=res$testes,"result"=res$result,"texto"=res$texto)
return(lista)}
