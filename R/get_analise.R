escolhateste <- function(x,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig=2,excluirtotal=T,respcol=T,cor="cyan4",idioma="PT",virgula=F){
  testes=data.frame() ; result<-data.frame() ; tex="" ; grafico=NULL
  if(tipox=="numeric" | tipox=="ordinal")
    {transform_ord = ifelse(tipox=="ordinal",T,F)
    if(tipoy=="factor")
      {if(length(niveisy)==2)
        {res = try(testet(as.numeric(unlist(x)),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,virgula))
          if(class(res)=="try-error") {res=mann(y,x,nomey,nomex,niveisy,dig,F,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisx,virgula=virgula)} else
            if(res$testes$sup==F | tipox=="ordinal") {res=mann(y,x,nomey,nomex,niveisy,dig,F,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisx,virgula=virgula)}
        } else
          {res = try(anovac(unlist(x),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,idioma,virgula=virgula))
            if(class(res)=="try-error") {res=kruskall(unlist(x),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,F,idioma,transform_ord,virgula=virgula)} else
              if(res$testes$sup==F | tipox=="ordinal") {res=kruskall(unlist(x),y,nomex,nomey,niveisy,dig,F,excluirtotal,cor,F,idioma,transform_ord,virgula=virgula)}
          }
       } else
    if(tipoy=="numeric" | tipoy=="ordinal")
      {ordinaly=ifelse(tipoy=="numeric",F,T) ; ordinalx=ifelse(tipox=="numeric",F,T)
       res=contcont(y,x,nomey,nomex,dig,cor,ordinalx=ordinalx,ordinaly=ordinaly,respcol=respcol,virgula=virgula)}
    }
  else
  {if(tipox=="factor")
    {if(tipoy=="numeric" | tipoy=="ordinal")
      {transform_ord = ifelse(tipoy=="ordinal",T,F)
      if(length(niveisx)==2)
        {res = try(testet(as.numeric(unlist(y)),x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,virgula=virgula))
          if(class(res)=="try-error") {res=mann(x,y,nomex,nomey,niveisx,dig,T,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisy,virgula=virgula)} else
            if(res$testes$sup==F | tipoy=="ordinal") {res=mann(x,y,nomex,nomey,niveisx,dig,T,excluirtotal,cor,idioma,transform_ord,niveis_ord=niveisy,virgula=virgula)}
        } else
        {res = try(anovac(unlist(y),x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,idioma,virgula=virgula))
          if(class(res)=="try-error") {res=kruskall(y,x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,T,idioma,transform_ord,virgula=virgula)} else
            if(res$testes$sup==F | tipox=="ordinal") {res=kruskall(y,x,nomey,nomex,niveisx,dig,T,excluirtotal,cor,T,idioma,transform_ord,virgula=virgula)}
        }
      } else
          if(tipoy=="factor") {res=catcat(x,y,nomex,nomey,niveisx,niveisy,dig,respcol,excluirtotal,cor,idioma,virgula=virgula)}
    }
  }
  result=res$result
  tex=res$texto
  grafico=res$grafico
  testes=res$testes
  return(list("testes"=testes,
              "result"=result,
              "texto"=list("grafico"=grafico,"tex"=tex)))}



get_analise_2 <- function(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig=2,excluirtotal=T,respcol=T,cor="cyan4",idioma="PT",virgula=F,nometab="Comparação"){
  xdim = ifelse(is.null(dim(xmat)),1,dim(xmat)[2])
  texto=c() ; textocont=c()
  testes=c()
  result=data.frame() ; resultcont=data.frame()
  temcont=F
  for (i in 1:xdim)
    {if(is.null(dim(xmat))==T) xmat_c = data.frame(xmat) else xmat_c = xmat[,i]
    if((tipox[i]=="numeric" | tipox[i]=="ordinal") & (tipoy=="numeric"|tipoy=="ordinal")) {
        temcont=T
        res=escolhateste(xmat_c,y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol,cor,idioma,virgula)
        resultcont=rbind(resultcont,res$result)
        textocont=list.append(textocont,res$texto,res$tabela)
        resumo=res$resumo
        testes <- rbind(testes,res$testes)} else
          {res = escolhateste(xmat_c,y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal,respcol,cor,idioma,virgula)
          result= rbind(result, res$result)
          texto <- list.append(texto,res$texto)
          testes <- rbind(testes,res$testes)}}
  resumo = resumo_inner(testes)

  cap = rodape_tabela(nometab,testes)
  cap2 = paste0(nometab,";")
  if("correl_p" %in% testes$tipo) cap2 = paste0(cap2," p-valores marcados pela letra 'h' indicam realização da correlação de Pearson;")
  if("correl_s" %in% testes$tipo) cap2 = paste0(cap2," p-valores marcados pela letra 'g' indicam realização da correlação de Spearman;")
  cap2=paste0(cap2," \\* significante a 5%; \\*\\* significante a 1%; \\*\\*\\* significante a 0.1% \n")
  if(is.null(result)) cap=NULL
  if(is.null(resultcont)) capcont=NULL
  
  if(temcont) lista=list("testes"=testes,"resumo" = paste0(paste0(resumo, collapse=""),". \n Podemos ver mais detalhes dos resultados na tabela a seguir: \n"),"result"=result,"caption"=cap,"texto"=texto,"int"=paste0("\n Agora, passamos a analisar as correlações entre as variáveis e a variável ",nomey,":  \n"),"resultcont"=resultcont,"capcont"=cap2,"textocont"=textocont) else lista=list("testes"=testes,"resumo"=paste0(paste0(resumo,collapse=""),". \n Podemos ver mais detalhes dos resultados na tabela a seguir: \n"),"result"=result,"caption"=cap,"texto"=texto)
return(lista)}


get_analise <- function(dados,col,auxiliar,y,tipoy="numeric",nomey="",niveisy="",dig=2,excluirtotal=T,respcol=T,cor='cyan4', idioma="PT",virgula=F,nometab="Comparação"){

  if(is.numeric(y)==T & length(y)==1) {
    tipoy=auxiliar$tipo[y]
    nomey=auxiliar$nomes[y]
    niveisy=auxiliar$niveis[y]
    y=unlist(dados[,y])}

  xmat=dados[,col]
  tipox=auxiliar$tipo[col]
  nomex=auxiliar$nomes[col]
  niveisx=auxiliar$niveis[col]
  res = get_analise_2(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal,respcol,cor,idioma,virgula,nometab)
  if(length(res)>7) lista=list("testes"=res$testes,"resumo"=res$resumo,"result"=res$result,"caption"=res$caption,"texto"=res$texto,"int"=res$int,"resultcont"=res$resultcont,"capcont"=res$capcont,"textocont"=res$textocont) else lista=list("testes"=res$testes,"resumo"=res$resumo,"result"=res$result,"caption"=res$cap,"texto"=res$texto)
return(lista)}
