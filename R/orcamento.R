orcamento <- function(dataenviobancobruto,bancobruto,auxiliar,analises,prazoprop=3,prazorelat=7,nomepesquisador="nome",cpfpesquisador="123.456.789-00",somar=0){
mês <- c("janeiro","fevereiro","março","abril","maio","junho","julho","agosto","setembro","outubro","novembro","dezembro")

matanalises = c()
sessoes=unique(analises$Sessão)
n_analises = length(sessoes)
for (i in 1:n_analises){
matanalises <- rbind(matanalises,data.frame("Nome"=sessoes[i],t(unname(c(table(factor(analises[analises$Sessão==sessoes[i],]$tipo, levels=c("numeric","factor", "catsame", "t", "mw", "aov1", "kw", "correl", "cc","t_par","wilc","aovmr","fried","mcnem","qcoch")))))), "Variáveis"=printvetor(analises[analises$Sessão==sessoes[i],]$Nome2)))}

totaltab = apply(matanalises[,c(-1,-17)],2,sum)

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
  if(sum(matanalises[i,-c(1:4,17)])>0) {descricaoanalises <- c(descricaoanalises,paste(" + **", matanalises[i,1],"**, com testes de comparação (podendo ser Qui-quadrado, Exato de Fisher, Teste de correlação, Teste-t, Mann-Whitney, Anova ou Kruskall Wallis, a depender da natureza e características dos dados) e suas devidas análises post-hoc e, quando necessário, tamanho de efeito. As ",sum(matanalises[i,-c(1:4,17)])," variáveis utilizadas na análise serão: ",matanalises$Variáveis[i],"; \n",sep=""))}}

  
crono = paste("Entrega do relatório no dia ",format(Sys.Date()+prazorelat, "%d/%m/%Y"),".",sep="",collapse="")
  
datavalido = format(Sys.Date()+prazoprop, "%d/%m/%Y")
dataprop=format(Sys.Date(), "%d/%m/%Y")
dataparcela2 = format(Sys.Date()+30, "%d/%m/%Y")

gs4_deauth()
aumentoprazo = read_sheet("https://docs.google.com/spreadsheets/d/1Adw20p6zDahYIx3a-L_LBZj0bAQZZliO5tYNh6SVFBY/edit?usp=sharing")
preçotestes = read_sheet("https://docs.google.com/spreadsheets/d/1VAuR_iQHx-pazZvWJCR_2qHhXPNI6pB3IlGl4Qo4WSU/edit?usp=sharing")
total = sum(totaltab)
    if (total < 30) 
        col = 2 else
          if (total < 100) col = 3 else col = 4
    p1 = sum(preçotestes[, col] * unlist(totaltab))
    preço = unlist(aumentoprazo[which(aumentoprazo$Prazo == prazorelat), 
        2]) * p1
    preço = preço + 0.5 * 297
    preço = preço + soma
    preço = preço/(1 - 0.17)
    precocheio = preço/0.95
    parcela2 = round(precocheio/2, 2)
    parcela1 = precocheio - parcela2

  
invest1 = paste("O investimento para consultoria é de: \n
 * **R$ ",format_real(precocheio),"**, pago em 2x, sendo R$ ",format_real(parcela1)," no dia ", dataprop," e R$ ", format_real(parcela2)," no dia ",dataparcela2,"; \n
OU \n
 * **R$ ",format_real(preço),"** (5% de desconto) à vista via pix, no ato do aceite da proposta. \n",sep="",collapse="")

invest2 =  paste("Proposta válida até ",datavalido,".",sep="",collapse="")
  
descbase = paste("A base de dados foi enviada na data ",dataenviobancobruto,", com ",dim(bancobruto)[1]," linhas e ",dim(bancobruto)[2]," colunas.
As variáveis presentes no banco são: \n",sep="",collapse="")

cat("
\\newpage
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
