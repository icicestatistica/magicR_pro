escolha_summary_para_juntar = function (x, nomesx, tipox, niveisx, nas, teste, grafico, cor, 
    bins, dig, idioma,virgula) 
{
    result = NULL
    if (tipox == "factor") {
        resulta = desc_uni_categorica(x, nomesx, eval(parse(text = niveisx)), 
            nas, T, T, T, teste, grafico, cor, dig,virgula)
        result = resulta$result[, c(1,4)]
        result[,1]=paste("&ensp;",unlist(result[,1]))
        texto = resulta$texto
        tabela = resulta$tabela
        grafico = resulta$grafico
        interp = resulta$interp
    } else if (tipox == "ordinal") {
        resulta = desc_uni_categorica(x, nomesx, eval(parse(text = niveisx)), 
            nas, T, F, T, teste, grafico, cor, dig,virgula)
        result = resulta$result[, c(1,4)]
        result[,1]=paste("&ensp;",unlist(result[,1]))
        texto = resulta$texto
        tabela = resulta$tabela
        grafico = resulta$grafico
        interp = resulta$interp
    } else if (tipox == "numeric") {
        resulta = desc_uni_continua(x, nomesx, bins, teste, grafico, 
            cor, dig, idioma,virgula)
        result = resulta$result[-c(1:3,9,10), ]
        result[4,1]= "Média (DP)";result[4,2]=paste0(result[4,2]," (",result[5,2],")")
        result = result[-5,]
        result[,1]=paste("&ensp;",unlist(result[,1]))
        texto = resulta$texto
        tabela = NULL
        grafico = resulta$grafico
        interp = resulta$interp
    }
    if (is.null(result) == F) {
        result = data.frame(result)
        result = rbind(c(paste0("*",nomesx," (n=",length(na.omit(x)),")*"),""), result)
        names(result) = c("Variável", 
            "Estatística")
    }
    testes = resulta$testes
    return(list(testes=testes,result = result, texto = texto, 
        tabela = tabela, grafico = grafico, interp = interp))}


get_summary_2 = function (x, nomesx, tipox, niveisx, nas = F, teste = F, grafico = T, 
    cor = "cyan4", bins = 20, dig = 2, idioma = "PT",virgula=F,nometab="Tabela descritiva") 
{
    result = NULL
    complem = list()
    textointerp = c()
    testes=c()
    xdim <- dim(x)[2]
    if(sum(tipox=="numeric") == xdim) vert=T else vert=F
    for (i in 1:xdim) {
        resulta = escolha_summary_para_juntar(x[, i], nomesx[i], 
            tipox[i], niveisx[i], nas, teste, grafico, cor, bins, 
            dig, idioma,virgula)
            if(vert==F) result <- rbind(result, resulta$result) else {result = cbind(result, resulta$result[,2]); result[1,i]=resulta$result[1,1]}
            complem <- list.append(complem, resulta$grafico, "\n", 
            resulta$texto, resulta$tabela, "\n")
            textointerp = c(textointerp, resulta$interp)
            testes=rbind(testes,resulta$testes)}
    if(vert==T) {result = data.frame(t(result)) ; names(result)=c("Variável","Min-Máx","Q1-Q3","Mediana","Média (DP)")}
    row.names(result) <- 1:dim(result)[1]
    
    return(list(testes = testes, interp=c(textointerp,"\n Podemos ver esses resultados na tabela a seguir: \n"),result = result, caption=rodape_tabela(nometab,testes),"\n",complem = complem))}

get_summary <- function(dados,auxiliar,gr='auto',nas=F,teste=F,grafico=T,cor="cyan4",bins=20,dig=2, idioma="PT",virgula=F,nometab="Tabela descritiva"){
    if (gr[1] == "auto") 
        gr = which(auxiliar$tipo %in% c("factor", "numeric", 
            "ordinal"))
    x <- data.frame(dados[, gr])
    nomesx <- auxiliar[gr, 2]
    tipox <- auxiliar[gr, 3]
    niveisx <- auxiliar[gr, 4]
    resultados = get_summary_2(x, nomesx, tipox, niveisx, nas, 
        teste, grafico, cor, bins, dig, idioma,virgula,nometab)
    return(list(testes = resultados$testes, interp=resultados$interp,result = resultados$result, caption=resultados$caption, "\n", complem = resultados$complem))}

### METODOLOGIA

intro_desc = function(){

texto=c("# Metodologias \n
**Estatísticas Descritivas:** Medidas-resumo ou estatísticas descritivas são usadas para resumir um conjunto de observações, a fim de comunicar a maior quantidade de informações da forma mais simples possível [@estatdesc].")

bib = "@article{estatdesc,
  title = {Descriptive statistics and normality tests for statistical data},
  volume = {22(1)},
  journal = {Annals of cardiac anaesthesia},
  author = {Mishra, P., Pandey, C. M., Singh, U., Gupta, A., Sahu, C., & Keshri, A.},
  year = {2019},
  pages = {67–-72},
}"

return(list("texto"=texto,"bib"=bib))}


numeric_meta = function() {

texto="
Para variáveis numéricas, usaremos as seguintes medidas: \n
+ N: Número de respostas
+ NA’s: Número de não respostas, também chamados missings ou informações faltantes.

**Medidas de tendência central:** Nos fornecem um valor que busca representar toda a distribuição. \n
+ Média: Soma de todos os valores dividido pelo número de indivíduos avaliados
+ Mediana: Valor central do conjunto de valores ordenados, ou seja, metade dos dados estão acima deste valor e a outra metade, abaixo.

**Medidas de dispersão:** Medidas de dispersão é outra medida usada para mostrar como se espalhou (variação) em um conjunto de dados. \n
+ Min – Máx: Valores mínimo e máximo encontrados
+ 1Q – 3Q: Desprezando os 25% menores valores e os 25% maiores valores, encontramos este intervalo de dados. Ou seja, a metade 'central' dos dados está neste intervalo.
+ DP: Desvio padrão. Medida de variabilidade dos dados: quanto maior, mais longe os dados estão da média."

bib=NULL

return(list("texto"=texto,"bib"=bib))}

categoric_meta = function(){
  
  texto="
Para variáveis categóricas nominais e ordinais, usaremos: \n
+ Frequência: Simplesmente contam o número de vezes que ocorre em cada variável;
+ Frequência Relativa: Divide a frequência pelo total. Representa a ocorrência em valor percentual. \n"
  
  
bib=NULL
  
return(list("texto"=texto,"bib"=bib))}
