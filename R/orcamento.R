orcamento <- function(dataenviobancobruto="10/11/2022",
                      bancobruto=dados,
                      auxiliar=auxiliar,
                      analises=analises,
                      prazoprop=3,
                      prazo=7,
                      parcelas=2,
                      conhecimento=c('muito'=T,'pouco'=F,'nada'=F),
                      continuidade=c('dificilmente'=T,'provavel'=F,'certeza'=F),
                      perfil=c('tranquilo'=T,'perdido'=F,'perdido_enrolado'=F),
                      tempo = c('suporte'=T,'tempo_reuniao'=30,'outros'=0),
                      imposto=0.175,
                      nomepesquisador="nome",
                      cpfpesquisador="123.456.789-00"){
  
  mês <- c("janeiro","fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro")
  
  gs4_deauth()
  preçotestes = read_sheet("https://docs.google.com/spreadsheets/d/1VAuR_iQHx-pazZvWJCR_2qHhXPNI6pB3IlGl4Qo4WSU/edit?usp=sharing")
  
  matanalises = c()
  sessoes=unique(analises$Sessão)
  n_analises = length(sessoes)
  for (i in 1:n_analises){
    matanalises <- rbind(matanalises,data.frame("Nome"=sessoes[i],t(unname(c(table(factor(analises[analises$Sessão==sessoes[i],]$tipo, levels=preçotestes$cod))))), "Variáveis"=printvetor(analises[analises$Sessão==sessoes[i],]$Nome2)))}
  
  totaltab = table(factor(analises$tipo,levels=preçotestes$cod))
  valorbruto=sum(totaltab*preçotestes$preço)
  
  descricaobanco <- c()
  for (i in 1:dim(auxiliar)[1]){
    if(auxiliar$tipo[i]=="factor"){
      descricaobanco=c(descricaobanco,paste(" + **",auxiliar$nomes[i],":** Variável categórica com ",length(eval(parse(text=auxiliar$niveis[i])))," grupo(s) (",printvetor(eval(parse(text=auxiliar$niveis[i]))),"). \n",sep=""))} else 
        if(auxiliar$tipo[i]=="ordinal"){
          descricaobanco=c(descricaobanco,paste(" + **",auxiliar$nomes[i],":** Variável ordinal com ", length(eval(parse(text=auxiliar$niveis[i]))), " grupo(s) (",printvetor(eval(parse(text=auxiliar$niveis[i]))),"). \n",sep=""))} else
            if(auxiliar$tipo[i]=="ID"){
              descricaobanco=c(descricaobanco,paste(" + **",auxiliar$nomes[i],":** Coluna de identificação. Não será utilizada na análise. \n", sep=""))} else
                if(auxiliar$tipo[i]=="catsame"){
                  descricaobanco=c(descricaobanco,paste(" + **",auxiliar$nomes[i],":** Variável do tipo caixas de respostas (o respondente pode marcar mais de uma opção). Para realizar a análise, é necessário separar essa coluna em ",length(eval(parse(text=auxiliar$niveis[i]))),", sendo uma coluna para cada resposta possível (",printvetor(eval(parse(text=auxiliar$niveis[i]))),"). \n", sep=""))} else
                    if(auxiliar$tipo[i]=="numeric"){
                      descricaobanco <- c(descricaobanco,paste(" + **",auxiliar$nomes[i],":** Variável numérica. \n",sep=""))} else
                      {descricaobanco <- c(descricaobanco,paste(" + **",auxiliar$nomes[i],":** Variável textual ou não categorizada corretamente. Não será utilizada na análise. \n",sep=""))}}
  descricaobanco = paste(descricaobanco, sep="",collapse="")
  
  
  descricaoanalises <- c()
  for (i in 1:n_analises){
    if(matanalises[i,2]+matanalises[i,3]>0) {descricaoanalises <- c(descricaoanalises,paste(" + **",matanalises[i,1],"**, com cálculos das frequências absoluta (n) e relativa (%) para as variáveis categóricas ou análise descritiva (N, Mínimo, Máximo, Quartis, Média, Mediana, Desvio Padrão, Coeficiente de variação e teste de Normalidade de Shapiro-Wilk) para variáveis numéricas. As ",matanalises[i,2]+matanalises[i,3]," variáveis utilizadas na análise serão: ",matanalises$Variáveis[i],"; \n",sep=""))}
    if(matanalises[i,4]>0) {descricaoanalises <- c(descricaoanalises,paste(" + **",matanalises[i,1],"**, com descrições das variáveis categóricas em termos de frequência, frequência relativa e intervalo de confiança para a proporção, incluindo  todas as comparações entre as proporções de respostas das variáveis. As ",matanalises[i,4]," variáveis utilizadas na análise serão: ",matanalises$Variáveis[i],"; \n",sep=""))}  
    if(sum(matanalises[i,-c(1:4,dim(matanalises)[2])])>0) {descricaoanalises <- c(descricaoanalises,paste(" + **", matanalises[i,1],"**, com testes de comparação (podendo ser Qui-quadrado, Exato de Fisher, Teste de correlação, Teste-t, Mann-Whitney, Anova ou Kruskall Wallis, a depender da natureza e características dos dados) e suas devidas análises post-hoc e, quando necessário, tamanho de efeito. As ",sum(matanalises[i,-c(1:4,dim(matanalises)[2])])," variáveis utilizadas na análise serão: ",matanalises$Variáveis[i],"; \n",sep=""))}}
  
  
  crono = paste("Entrega do relatório no dia ",format(Sys.Date()+prazo, "%d/%m/%Y"),".",sep="",collapse="")
  
  datavalido = format(Sys.Date()+prazoprop, "%d/%m/%Y")
  dataprop=format(Sys.Date(), "%d/%m/%Y")
  
  ## Prazo
  cortes_prazo = c(1,2,3,5,10)
  aumento_prazo = c(0.5,0.3,0.15,0.05,0)
  mult_prazo = aumento_prazo[max(which(prazo>=cortes_prazo))]
  
  ## Número de análises
  numero = sum(totaltab)
  cortes_numero = c(1,11,51,101)
  aumentos_numero = c(0,-0.01,-0.03,-0.05)
  mult_numero = aumentos_numero[max(which(numero>=cortes_numero))]
  
  ## Conhecimento
  aumentos_conhecimento = c(0,0.05,0.10)
  mult_conhecimento = sum(conhecimento*aumentos_conhecimento)
  
  ## Continuidade
  aumentos_continuidade=c(0,-0.05,-0.1)
  mult_continuidade = sum(continuidade*aumentos_continuidade)
  
  ## Perfil
  aumentos_perfil=c(0,0.05,0.15)
  mult_perfil = sum(perfil*aumentos_perfil)
  
  ## Extras
  aumentos_extra = c(150,300/60,1)
  soma_extras = sum(tempo*aumentos_extra)
  
  ## Taxas e impostos
  mult_imposto = 1/(1-imposto)
  
  
  preco_parcelado = round(((valorbruto*(1+mult_prazo)*(1+mult_perfil)*(1+mult_conhecimento)*(1+mult_numero)*(1+mult_continuidade))+soma_extras)*mult_imposto,2)
  
  preco_desconto = round(0.95*preco_parcelado,2)
  
  ### Parcelamentos
  
  preço_parcelas=rep(0,parcelas)
  preço_parcelas[-1]=round(preco_parcelado/parcelas,2)
  preço_parcelas[1]=preco_parcelado-sum(preço_parcelas[-1])
  
  dataparcelas = Sys.Date()+30*(1:parcelas-1)
  
  crono = paste("Entrega do relatório no dia ",format(Sys.Date()+prazo, "%d/%m/%Y"),".",sep="",collapse="")
  
  datavalido = format(Sys.Date()+prazoprop, "%d/%m/%Y")
  dataprop=format(Sys.Date(), "%d/%m/%Y")
  dataparcelas = format(Sys.Date()+30*(1:parcelas-1), "%d/%m/%Y")
  
  
  # Investimento
  
  invest1 = paste("O investimento para consultoria é de **R$ ",format_real(preco_parcelado),"**, pago em ",parcelas,"x, sendo: \n
", paste(paste(" * _R$ ",format_real(preço_parcelas),"_ no dia ", dataparcelas,"; \n",sep=""),collapse=""),"
OU \n
 * **R$ ",format_real(preco_desconto),"** (5% de desconto) à vista via pix, no ato do aceite da proposta. \n",sep="",collapse="")
  
  invest2 =  paste("Proposta válida até ",datavalido,".",sep="",collapse="")
  
  descbase = paste("A base de dados foi enviada na data ",dataenviobancobruto,", com ",dim(bancobruto)[1]," linhas e ",dim(bancobruto)[2]," colunas.
As variáveis presentes no banco são: \n",sep="",collapse="")
  
  cat("
\\newpage

# ORÇAMENTO

## BASE DE DADOS
", descbase, "", descricaobanco,"
Qualquer alteração no banco após o aceite da proposta poderá gerar retrabalho por parte da estatística e, portanto, poderá ser cobrado. Por este motivo, pedimos que tenha certeza sobre o presente banco de dados.

## OBJETIVOS DA CONSULTORIA
O objetivo da presente consultoria é a entrega dos seguintes resultados:
", descricaoanalises,"

## CRONOGRAMA
",crono,"

## FORMA DE ENTREGA
A consultoria proposta inclui:

* Definição dos métodos de análise
* Aplicação da metodologia apropriada
* Relatório de análise estatística com análises, tabelas, interpretação dos resultados, detalhes da metodologia e bibliografia utilizadas;
* Construção de gráficos sob demanda por 6 meses a partir da entrega do relatório;
* Uma reunião de 30 minutos para esclarecimento de dúvidas metodológicas.

## INVESTIMENTO
",invest1,"
**Dados bancários:** \n
* Banco BS2 S.A. - 218
* Agência: 0001-9
* Conta: 9085203
* Razão Social - ISABELLE CRISTINA IDALGO LTDA
* CNPJ - 41.986.330/0001-85

A chave pix é o número do CNPJ. Pedimos a gentileza de envio do comprovante de pagamento através do e-mail icicestatistica@gmail.com.
A nota fiscal será enviada até 3 dias após a conclusão do pagamento. \n", invest2,"

## OBSERVAÇÕES
* Não nos cabe a conclusão e argumentação dos resultados, uma vez que não detemos a “inteligência” de cada trabalho e sua respectiva área de pesquisa.
* O trabalho que desenvolvemos é único e exclusivamente para a realização da análise estatística, não é de nossa competência fazer a edição e formatação dos resultados conforme regimento de cada universidade e/ou revista.

\\newpage",
      paste0("Campinas, ",mday(Sys.Date())," de ",mês[month(Sys.Date())]," de ",year(Sys.Date()),sep=""),"

<br> <br>

------------------------------------------------------------------------ \n

###### **Estatística Responsável:** Isabelle Cristina Idalgo Carnielli \n

###### CONRE: 10734 \n

<br>

------------------------------------------------------------------------ \n
",
      paste0("###### **Pesquisador(a):** ",nomepesquisador," \n",sep=""),
      
      paste0("###### CPF: ",cpfpesquisador," \n",sep=""),"

\\newpage

# Bibliografia

",sep="\n", collapse="")}

