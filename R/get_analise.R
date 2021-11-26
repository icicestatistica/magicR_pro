escolhateste <- function(x,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal){
  result<-data.frame()
  tex=""
  if(tipox=="numeric") {
    if(tipoy=="factor") {
      if(length(niveisy)==2)
        {res = testet(x,y,nomex,nomey,niveisy,dig,F,excluirtotal)
         if(res$sup==T) { result=res$resul
                          tex=res$texto}}
    }} else
  {if(tipox=="factor")
    if(tipoy=="numeric"){
      if(length(niveisx)==2)
        {res = testet(y,x,nomey,nomex,niveisx,dig,T,excluirtotal)
         if(res$sup==T) { result=res$resul
                          tex=res$texto}}
                        }
    }
  }
  return(list("result"=result,
         "texto"=tex))
}

get_analise_2 <- function(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal){
  xdim=dim(xmat)[2]
  texto=c()
  result=data.frame()
  for (i in 1:xdim)
    {res = escolhateste(xmat[,i],y,tipox[i],tipoy,nomex[i],nomey,eval(parse(text=niveisx[i])),eval(parse(text=niveisy)),dig,excluirtotal)
     result= rbind(result, res$result)
     texto <- c(texto,res$texto)}
return(list("testes"=list(),"result"=result,"texto"=texto))}


get_analise(dados,col,auxiliar,y,nomey,niveisy,dig,excluirtotal){
  xmat=dados[,col]
  tipox=auxiliar$tipo[col]
  nomex=auxiliar$nomes[col]
  niveisx=auxiliar$niveis[col]
  res = get_analise_2(xmat,y,tipox,tipoy,nomex,nomey,niveisx,niveisy,dig,excluirtotal)
return(list("testes"=res$testes,"result"=res$result,"texto"=res$texto))}
