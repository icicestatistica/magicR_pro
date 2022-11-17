escolha_summary_para_juntar = function (x, nomesx, tipox, niveisx, nas, teste, grafico, cor, 
    bins, dig, idioma) 
{
    result = NULL
    if (tipox == "factor") {
        resulta = desc_uni_categorica(x, nomesx, eval(parse(text = niveisx)), 
            nas, T, T, T, teste, grafico, cor, dig)
        result = resulta$result[, c(1,4)]
        result[,1]=paste("&ensp;",unlist(result[,1]))
        texto = resulta$texto
        tabela = resulta$tabela
        grafico = resulta$grafico
    }
    else if (tipox == "ordinal") {
        resulta = desc_uni_categorica(x, nomesx, eval(parse(text = niveisx)), 
            nas, T, F, T, teste, grafico, cor, dig)
        result = resulta$result[, c(1,4)]
        result[,1]=paste("&ensp;",unlist(result[,1]))
        texto = resulta$texto
        tabela = resulta$tabela
        grafico = resulta$grafico
    }
    else if (tipox == "numeric") {
        resulta = desc_uni_continua(x, nomesx, bins, teste, grafico, 
            cor, dig, idioma)
        result = resulta$result[-c(1:3,9,10), ]
        result[4,1]= "Média (DP)";result[4,2]=paste0(result[4,2]," (",result[5,2],")")
        result = result[-5,]
        result[,1]=paste("&ensp;",unlist(result[,1]))
        texto = resulta$texto
        tabela = NULL
        grafico = resulta$grafico
    }
    if (is.null(result) == F) {
        result = data.frame(result)
        result = rbind(c(paste0("*",nomesx," (n=",length(na.omit(x)),")*"),""), result)
        names(result) = c("Variável", 
            "Estatística")
    }
    return(list(result = result, texto = texto, 
        tabela = tabela, grafico = grafico))}



get_summary_2 = function (x, nomesx, tipox, niveisx, nas = F, teste = F, grafico = T, 
    cor = "cyan4", bins = 20, dig = 2, idioma = "PT") 
{
    result = NULL
    complem = list()
    xdim <- dim(x)[2]
    if(sum(tipox=="numeric") == xdim) vert=T else vert=F
    for (i in 1:xdim) {
        resulta = escolha_summary_para_juntar(x[, i], nomesx[i], 
            tipox[i], niveisx[i], nas, teste, grafico, cor, bins, 
            dig, idioma)
            if(vert==F) result <- rbind(result, resulta$result) else {result = cbind(result, resulta$result[,2]); result[1,i]=resulta$result[1,1]}
            complem <- list.append(complem, resulta$grafico, "\n", 
            resulta$texto, resulta$tabela, "\n")}
    if(vert==T) {result = data.frame(t(result)) ; names(result)=c("Variável","Min-Máx","Q1-Q2","Mediana","Média (DP)")}
    row.names(result) <- 1:dim(result)[1]
    testes = data.frame(Nome1 = "", Nome2 = nomesx, tipo = tipox, 
        sig_ou_não = "", resumo = "")
    return(list(testes = testes, result = result, complem = complem))}

get_summary <- function(dados,auxiliar,gr='auto',nas=F,teste=F,grafico=T,cor="cyan4",bins=20,dig=2, idioma="PT"){
    if (gr[1] == "auto") 
        gr = which(auxiliar$tipo %in% c("factor", "numeric", 
            "ordinal"))
    x <- data.frame(dados[, gr])
    nomesx <- auxiliar[gr, 2]
    tipox <- auxiliar[gr, 3]
    niveisx <- auxiliar[gr, 4]
    resultados = get_summary_2(x, nomesx, tipox, niveisx, nas, 
        teste, grafico, cor, bins, dig, idioma)
    return(list(testes = resultados$testes, result = resultados$result, complem = resultados$complem))}
