escolhateste_pareado = function(id,time,x,tipox,nomex,moms,nometime,cor){

result<-data.frame() ; tex="" ; grafico=NULL

n=length(moms)
if(n==2){
  if(tipox=="numeric" | tipox=="ordinal") {res=wilcox(id,time,unlist(x),nomex,moms,tipox,nometime,cor=cor)}
  else{
    if(tipox=="factor" | tipox=="character") {res=mc_icic(id,time,unlist(x),moms,nomex,nometime)
}}} else {
      if(tipox=="numeric") {res=friedman_icic(id,time,unlist(x),nomex,moms,nometime,cor=cor)}
      else{
        if(tipox=="factor" | tipox=="character") {res=coch_icic(id,time,unlist(x),moms,nomex,nometime)}}
    }
    testes=res$testes
    result=res$result
    tex=res$texto
    grafico=res$grafico
  return(list("testes"=testes,
              "result"=result,
              "texto"=list("grafico"=grafico,"tex"=tex)))}

get_analise_pareado_2 = function(id,time,xmat,tiposx,nomesx,moms,nometime,nometab,cor){
  xdim = ifelse(is.null(dim(xmat)),1,dim(xmat)[2])
  texto=c();  testes=c() ; result=data.frame()
  if(is.null(dim(xmat))==T) xmat_c = data.frame(xmat) else xmat_c = xmat
  for (i in 1:xdim)
    {res=escolhateste_pareado(id,time,unlist(xmat_c[,i]),tiposx[i],nomesx[i],moms,nometime,cor)
        result=rbind(result,res$result)
        texto=list.append(texto,res$texto)
        testes <- rbind(testes,res$testes)}

  capt = rodape_tabela(nometab,testes)
  return(list("testes"=testes,"result"=result,"caption"=capt,"texto"=texto))}


get_analise_pareado <- function(data,auxiliar,n_id,n_time,cols,moms,nometime="Momento",nometab="Comparação",cor="cyan4"){

  id=unlist(data[,n_id])
  time=unlist(data[,n_time])
  xmat=data[,cols]
  tiposx=auxiliar$tipo[cols]
  nomesx=auxiliar$nomes[cols]
  
  res = get_analise_pareado_2(id,time,xmat,tiposx,nomesx,moms,nometime,nometab,cor)
  
  lista=list("testes"=res$testes,"result"=res$result,"caption"=res$caption,"texto"=res$texto)
return(lista)}
