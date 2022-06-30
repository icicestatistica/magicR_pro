escolhateste <- function(x,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig=2,excluirtotal=F,respcol=T,cor="cyan4",idioma="PT"){
  result<-data.frame() ; tex="" ; grafico=NULL
  t=0 ; mw=0; kw=0; aov1=0;cc=0 ; correl=0
  if(tipox=="numeric" | tipox=="ordinal") {
    transform_ord = ifelse(tipox=="ordinal",T,F)
    if(tipoy=="factor") {
      if(length(niveisy)==2) 
           {res = testet(as.numeric(unlist(x)),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor)
           if(res$sup==F | tipox=="ordinal") {res=mann(y,x,nomey,nomex,niveisy,dig,F,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisx) ; mw=1} else {t=1}
           result=res$resul
           tex=res$texto
           grafico=res$grafico} else
                        {res = anovac(as.numeric(unlist(x)),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,idioma)
                        if(res$sup==F | tipox=="ordinal") {res=kruskall(as.numeric(unlist(x)),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,F,idioma,transform_ord);kw=1} else {aov1=1}
                        result=res$result
                        tex=res$texto
                        grafico=res$grafico}
  } else
    if(tipoy=="numeric" | tipoy=="ordinal") {res=contcont(as.numeric(unlist(y)),x,nomey,nomex,dig,cor)
                          correl=1
                          result=res$resul
                          tex=res$texto
                          grafico=res$grafico}
  } else {
  if(tipox=="factor") {
    if(tipoy=="numeric" | tipoy=="ordinal") {
      transform_ord = ifelse(tipoy=="ordinal",T,F)
      if(length(niveisx)==2)
        {res = testet(as.numeric(unlist(y)),x,nomey,nomex,niveisx,dig,T,excluirtotal,cor)
        if(res$sup==F | tipoy=="ordinal") {res=mann(x,y,nomex,nomey,niveisx,dig,T,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisy) ; mw=1} else {t=1}
                        result=res$resul
                        tex=res$texto
                        grafico=res$grafico} else
                                          {res = anovac(as.numeric(unlist(y)),x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,idioma)
                                           if(res$sup==F | tipoy=="ordinal") {res=kruskall(as.numeric(unlist(y)),x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,T,idioma,transform_ord);kw=1} else {aov1=1}
                                           result=res$result
                                           tex=res$texto
                                           grafico=res$grafico}} else
     if(tipoy=="factor") {res=catcat(x,y,nomex,nomey,niveisx,niveisy,dig,respcol,excluirtotal,cor,idioma)
                          cc=1
                          result=res$result
                          tex=res$texto
                          grafico=res$grafico}
  }}
  return(list("testes"=c("desc"=0,"catsame"=0,"t"=t,"mw"=mw,"aov1"=aov1,"kw"=kw,"correl"=correl,"cc"=cc,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0),
              "result"=result,
              "texto"=list("grafico"=grafico,"tex"=tex)))}



get_analise_2 <- function(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig=2,excluirtotal=F,respcol=T,cor="cyan4",idioma="PT"){
  xdim = ifelse(is.null(dim(xmat)),1,dim(xmat)[2])
  texto=c() ; textocont=c()
  testes=c("desc"=0,"catsame"=0,"t"=0,"mw"=0,"aov1"=0,"kw"=0,"correl"=0,"cc"=0,"t_par"=0,"wilc"=0,"aovmr"=0,"fried"=0,"mcnem"=0,"qcoch"=0)
  result=data.frame() ; resultcont=data.frame()
  temcont=F
  for (i in 1:xdim)
    {if(is.null(dim(xmat))==T) xmat_c = data.frame(xmat) else xmat_c = xmat[,i]
    if(tipox[i]=="numeric" & tipoy=="numeric") {
        temcont=T
        res=escolhateste(xmat_c,y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol,cor,idioma)
        resultcont=rbind(resultcont,res$result)
        textocont=list.append(textocont,res$texto,res$tabela)
        resumo=res$resumo
        testes <- testes+res$testes} else
          {res = escolhateste(xmat_c,y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol,cor,idioma)
          result= rbind(result, res$result)
          texto <- list.append(texto,res$texto)
          testes <- testes+res$testes}}
  if(temcont) lista=list("testes"=testes,"result"=result,"texto"=texto,"int"=paste0("\n Agora, passamos a analisar as correlações entre as variáveis e a variável ",nomey,":  \n"),"resultcont"=resultcont,"textocont"=textocont) else lista=list("testes"=testes,"result"=result,"texto"=texto)
return(lista)}


get_analise <- function(dados,col,auxiliar,y,tipoy="numeric",nomey="",niveisy="",dig=2,excluirtotal=F,respcol=T,cor='cyan4', idioma="PT"){

  if(is.numeric(y)==T & length(y)==1) {
    tipoy=auxiliar$tipo[y]
    nomey=auxiliar$nomes[y]
    niveisy=auxiliar$niveis[y]
    y=unlist(dados[,y])}

  xmat=dados[,col]
  tipox=auxiliar$tipo[col]
  nomex=auxiliar$nomes[col]
  niveisx=auxiliar$niveis[col]
  res = get_analise_2(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal,respcol,cor,idioma)
  if(length(res)==6) lista=list("testes"=res$testes,"result"=res$result,"texto"=res$texto,"int"=res$int,"resultcont"=res$resultcont,"textocont"=res$textocont) else lista=list("testes"=res$testes,"result"=res$result,"texto"=res$texto)
return(lista)}
