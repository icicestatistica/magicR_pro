escolhateste <- function(x,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal,respcol){
  result<-data.frame() ; tex=""
  t=0 ; mw=0; aov1=0;cc=0 ; correl=0
  if(tipox=="numeric") {
    if(tipoy=="factor") {
      if(length(niveisy)==2) 
           {res = testet(x,y,nomex,nomey,niveisy,dig,F,excluirtotal)
           if(res$sup==F) {res=mann(y,x,nomey,nomex,niveisy,dig,F,excluirtotal) ; mw=1} else {t=1}
           result=res$resul
           tex=res$texto} else
                        {res = anovac(x,y,nomex,nomey,niveisy,dig,F,excluirtotal)
                        aov1=1
                        result=res$resul
                        tex=res$texto}
  } else
    if(tipoy=="numeric") {res=contcont(y,x,nomey,nomex,dig)
                          correl=1
                          result=res$resul
                          tex=res$texto}
  } else {
  if(tipox=="factor") {
    if(tipoy=="numeric") {
      if(length(niveisx)==2)
        {res = testet(y,x,nomey,nomex,niveisx,dig,T,excluirtotal)
        if(res$sup==F) {res=mann(x,y,nomex,nomey,niveisx,dig,T,excluirtotal) ; mw=1} else {t=1}
                        result=res$resul
                        tex=res$texto} else
                                          {res = anovac(y,x,nomey,nomex,niveisx,dig,T,excluirtotal)
                                           aov1=1
                                           result=res$resul
                                           tex=res$texto}} else
     if(tipoy=="factor") {res=catcat(x,y,nomex,nomey,niveisx,niveisy,dig,respcol,excluirtotal)
                          cc=1
                          result=res$result
                          tex=res$texto}   
  }}
  return(list("testes"=c("desc"=0,"catsame"=0,"t"=t,"mw"=mw,"aov1"=aov1,"kw"=0,"correl"=correl,"cc"=cc,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
              "result"=result,
              "texto"=tex))}



get_analise_2 <- function(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal,respcol){
  xdim=dim(xmat)[2]
  texto=c() ; textocont=c()
  testes=c("desc"=0,"catsame"=0,"t"=0,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0)
  result=data.frame() ; resultcont=data.frame()
  temcont=F
  for (i in 1:xdim)
    {if(tipox[i]=="numeric" & tipoy=="numeric") {
        temcont=T
        res=escolhateste(xmat[,i],y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol)
        resultcont=rbind(resultcont,res$result)
        textocont=res$texto
        testes <- testes+res$testes} else
          {res = escolhateste(xmat[,i],y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol)
          result= rbind(result, res$result)
          texto <- c(texto,res$texto)
          testes <- testes+res$testes}}
  if(temcont) lista=list("testes"=testes,"result"=result,"texto"=texto,paste0("\n Agora, passamos a analisar as correlações entre as variáveis e a variável ",nomey,":  \n"),"resultcont"=resultcont,"textocont"=textocont) else lista=list("testes"=testes,"result"=result,"texto"=texto)
return(lista)}


get_analise <- function(dados,col,auxiliar,y,nomey,niveisy,dig,excluirtotal,respcol){
  xmat=dados[,col]
  tipox=auxiliar$tipo[col]
  nomex=auxiliar$nomes[col]
  niveisx=auxiliar$niveis[col]
  res = get_analise_2(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal,respcol)
return(list("testes"=res$testes,"result"=res$result,"texto"=res$texto))}
