escolhateste <- function(x,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig=2,excluirtotal=T,respcol=T,cor="cyan4",idioma="PT"){
  testes=data.frame() ; result<-data.frame() ; tex="" ; grafico=NULL
  if(tipox=="numeric" | tipox=="ordinal")
    {transform_ord = ifelse(tipox=="ordinal",T,F)
    if(tipoy=="factor")
      {if(length(niveisy)==2)
        {res = try(testet(as.numeric(unlist(x)),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor))
          if(class(res)=="try-error") {res=mann(y,x,nomey,nomex,niveisy,dig,F,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisx)} else
            if(res$testes$sup==F | tipox=="ordinal") {res=mann(y,x,nomey,nomex,niveisy,dig,F,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisx)}
        } else
          {res = try(anovac(as.numeric(unlist(x)),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,idioma))
            if(class(res)=="try-error") {res=kruskall(as.numeric(unlist(x)),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,F,idioma,transform_ord)} else
              if(res$testes$sup==F | tipox=="ordinal") {res=kruskall(as.numeric(unlist(x)),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,F,idioma,transform_ord)}
          }
       } else
    if(tipoy=="numeric" | tipoy=="ordinal")
      {ordinaly=ifelse(tipoy=="numeric",F,T) ; ordinalx=ifelse(tipox=="numeric",F,T)
       res=contcont(y,x,nomey,nomex,dig,cor,ordinalx=ordinalx,ordinaly=ordinaly)}
    }
  else
  {if(tipox=="factor")
    {if(tipoy=="numeric" | tipoy=="ordinal")
      {transform_ord = ifelse(tipoy=="ordinal",T,F)
      if(length(niveisx)==2)
        {res = try(testet(as.numeric(unlist(y)),x,nomey,nomex,niveisx,dig,T,excluirtotal,cor))
          if(class(res)=="try-error") {res=mann(x,y,nomex,nomey,niveisx,dig,T,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisy)} else
            if(res$testes$sup==F | tipoy=="ordinal") {res=mann(x,y,nomex,nomey,niveisx,dig,T,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisy)}
        } else
        {res = try(anovac(as.numeric(unlist(y)),x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,idioma))
          if(class(res)=="try-error") {res=kruskall(y,x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,T,idioma,transform_ord)} else
            if(res$testes$sup==F | tipox=="ordinal") {res=kruskall(y,x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,T,idioma,transform_ord)}
        }
      } else
          if(tipoy=="factor") {res=catcat(x,y,nomex,nomey,niveisx,niveisy,dig,respcol,excluirtotal,cor,idioma)}
    }
  }
  result=res$result
  tex=res$texto
  grafico=res$grafico
  testes=res$testes
  return(list("testes"=testes,
              "result"=result,
              "texto"=list("grafico"=grafico,"tex"=tex)))}



get_analise_2 <- function(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig=2,excluirtotal=T,respcol=T,cor="cyan4",idioma="PT"){
  xdim = ifelse(is.null(dim(xmat)),1,dim(xmat)[2])
  texto=c() ; textocont=c()
  testes=c()
  result=data.frame() ; resultcont=data.frame()
  temcont=F
  for (i in 1:xdim)
    {if(is.null(dim(xmat))==T) xmat_c = data.frame(xmat) else xmat_c = xmat[,i]
    if((tipox[i]=="numeric" | tipox[i]=="ordinal") & (tipoy=="numeric"|tipoy=="ordinal")) {
        temcont=T
        res=escolhateste(xmat_c,y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol,cor,idioma)
        resultcont=rbind(resultcont,res$result)
        textocont=list.append(textocont,res$texto,res$tabela)
        resumo=res$resumo
        testes <- rbind(testes,res$testes)} else
          {res = escolhateste(xmat_c,y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol,cor,idioma)
          result= rbind(result, res$result)
          texto <- list.append(texto,res$texto)
          testes <- rbind(testes,res$testes)}}
  if(temcont) lista=list("testes"=testes,"result"=result,"texto"=texto,"int"=paste0("\n Agora, passamos a analisar as correlações entre as variáveis e a variável ",nomey,":  \n"),"resultcont"=resultcont,"textocont"=textocont) else lista=list("testes"=testes,"result"=result,"texto"=texto)
return(lista)}


get_analise <- function(dados,col,auxiliar,y,tipoy="numeric",nomey="",niveisy="",dig=2,excluirtotal=T,respcol=T,cor='cyan4', idioma="PT"){

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
