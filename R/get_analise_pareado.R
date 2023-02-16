escolhateste_pareado = function(id,time,x,tipox,nomex,moms){

result<-data.frame() ; tex="" ; grafico=NULL

n=length(moms)
if(n==2){
  if(tipox=="numeric") {res=wilcox(id,time,unlist(x),nomex,moms)
    nometeste="wilc"
    result=res$result
    tex=res$texto
    grafico=res$grafico}
  else{
    if(tipox=="factor" | tipox=="character") {res=mc_icic(id,time,unlist(x),moms,nomex)
    nometeste="mcnem"
    result=res$result
    tex=res$texto
    grafico=res$grafico}}} else {
      if(tipox=="numeric") {res=friedman_icic(id,time,unlist(x),nomex,moms)
        nometeste="fried"
        result=res$result
        tex=res$texto
        grafico=res$grafico}
      else{
        if(tipox=="factor" | tipox=="character") {res=coch_icic(id,time,unlist(x),moms,nomex)
          nometeste="qcoch"
          result=res$result
          tex=res$texto
          grafico=res$grafico}}
    }

  testes=data.frame(Nome1 = "Momento", Nome2 = nomex, tipo = nometeste, sig_ou_não = "", resumo = "")
  return(list("testes"=testes,
              "result"=result,
              "texto"=list("grafico"=grafico,"tex"=tex)))}

get_analise_pareado_2 = function(id,time,xmat,tiposx,nomesx,moms){
  xdim = ifelse(is.null(dim(xmat)),1,dim(xmat)[2])
  texto=c();  testes=c() ; result=data.frame()
  if(is.null(dim(xmat))==T) xmat_c = data.frame(xmat) else xmat_c = xmat
  for (i in 1:xdim)
    {res=escolhateste_pareado(id,time,unlist(xmat_c[,i]),tiposx[i],nomesx[i],moms)
        result=rbind(result,res$result)
        texto=list.append(texto,res$texto)
        testes <- rbind(testes,res$testes)}
  return(list("testes"=testes,"result"=result,"texto"=texto))}


get_analise_pareado <- function(data,auxiliar,n_id,n_time,cols,moms){

  id=unlist(data[,n_id])
  time=unlist(data[,n_time])
  xmat=data[,cols]
  tiposx=auxiliar$tipo[cols]
  nomesx=auxiliar$nomes[cols]
  
  res = get_analise_pareado_2(id,time,xmat,tiposx,nomesx,moms)
  
  lista=list("testes"=res$testes,"result"=res$result,"texto"=res$texto)
return(lista)}
