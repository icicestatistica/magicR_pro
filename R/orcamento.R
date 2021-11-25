orcamento <- function(dataenviobancobruto,bancobruto,auxiliar,prazoprop,parcelas,prazorelat){

n_analises = dim(analises)[1]
matanalises = c()
for (i in 1:n_analises)
matanalises= rbind(matanalises, unlist(eval(parse(text=analises$analises[i]))))
matanalises <- data.frame("Nome"=analises$Nome,matanalises)

totaltab = apply(matanalises[,2:6],2,sum)

descricaobanco <- c()
for (i in 1:dim(auxiliar)[1]){
  if(auxiliar$tipo[i]=="factor"){
    descricaobanco=c(descricaobanco,paste(" * **",auxiliar$nomes[i],":** Variável categórica com ",length(eval(parse(text=auxiliar$niveis[i])))," grupo(s) (",printvetor(eval(parse(text=auxiliar$niveis[i]))),"). \n",sep=""))} else 
        if(auxiliar$tipo[i]=="ordinal"){
           descricaobanco=c(descricaobanco,paste(" * **",auxiliar$nomes[i],":** Variável ordinal com ", length(eval(parse(text=auxiliar$niveis[i]))), " grupo(s) (",printvetor(eval(parse(text=auxiliar$niveis[i]))),"). \n",sep=""))} else
            {descricaobanco <- c(descricaobanco,paste(" * **",auxiliar$nomes[i],":** Variável numérica. \n",sep=""))}}

descricaoanalises <- c()
for (i in 1:n_analises){
  if(matanalises[i,2]>0) {descricaoanalises <- c(descricaoanalises,paste(" * **",matanalises[i,1],"**, com ",matanalises[i,2]," descrições das variáveis categóricas em termos de frequência e frequência relativa e das variáveis numéricas com medidas de centralidade e dispersão; \n",sep=""))}
  if(matanalises[i,3]>0) {descricaoanalises <- c(descricaoanalises,paste(" * **",matanalises[i,1],"**, com ",matanalises[i,3]," descrições das variáveis categóricas dicotômicas em termos de frequência, frequência relativa e intervalo de confiança para a proporção, incluindo  todas as comparações entre as proporções de respostas das variáveis; \n",sep=""))}  
  if(sum(matanalises[i,4:6])>0) {descricaoanalises <- c(descricaoanalises,paste(" * **", matanalises[i,1],"**, com ",sum(matanalises[i,4:6])," testes de comparação e suas devidas análises post-hoc e, quando necessário, tamanho de efeito; \n",sep=""))}}

datapropdesform = Sys.Date()+prazoprop
  
dataprop=format(datapropdesform, "%d/%m/%Y")

gs4_deauth()
aumentoprazo=read_sheet("https://docs.google.com/spreadsheets/d/1Adw20p6zDahYIx3a-L_LBZj0bAQZZliO5tYNh6SVFBY/edit?usp=sharing")
preçotestes=read_sheet("https://docs.google.com/spreadsheets/d/1VAuR_iQHx-pazZvWJCR_2qHhXPNI6pB3IlGl4Qo4WSU/edit?usp=sharing")

total=sum(totaltab)
if(total<30) col=2 else
  if(total<100) col=3 else col=4

p1=sum(preçotestes[,col]*unlist(totaltab))
preço=unlist(aumentoprazo[which(aumentoprazo$Prazo==prazorelat),2])*p1
preço=preço/(1-0.17)

preçoparcelas=round(preço/parcelas,2)
parcela1=preço-(parcelas-1)*preçoparcelas

pagamento = c("+ **R$",format_real(parcela1),"**, no ato do aceite da proposta; \n", sep="")
if (parcelas>1) for (i in 2:parcelas) pagamento = c(pagamento,"+ **R$",format_real(preçoparcelas),"**, ",30*(i-1)," dias após o aceite da proposta; \n")

cat("
\\newpage
## BASE DE DADOS
A base de dados foi enviada por email na data ",dataenviobancobruto,", com ",dim(bancobruto)[1]," linhas e ",dim(bancobruto)[2]," colunas.
As variáveis presentes no banco são:
", descricaobanco,"
Qualquer alteração no banco após o aceite da proposta poderá gerar retrabalho por parte da estatística e, portanto, poderá ser cobrado. Por este motivo, pedimos que tenha certeza sobre o presente banco de dados.

## OBJETIVOS DA CONSULTORIA
O objetivo da presente consultoria é a entrega dos seguintes resultados:
", descricaoanalises,"

## CRONOGRAMA
Entrega do relatório no dia ",format(Sys.Date()+prazorelat, "%d/%m/%Y"),".

## FORMA DE ENTREGA
A consultoria proposta inclui:

* Definição dos métodos de análise
* Aplicação da metodologia apropriada
* Relatório de análise estatística com análises, tabelas, interpretação dos resultados, detalhes da metodologia e bibliografia utilizadas;
* Construção de gráficos sob demanda por 6 meses a partir da entrega deste relatório;
* Uma reunião de 30 minutos para esclarecimento de dúvidas metodológicas.

## INVESTIMENTO
  
O investimento para consultoria é de **R$ ",format_real(preço),"**, pago via pix nas seguintes ocasiões: \n
",pagamento,"
**Dados bancários:** \n
+ Banco BS2 S.A. - 218
+ Agência: 0001-9
+ Conta: 9085203
+ Razão Social - ISABELLE CRISTINA IDALGO LTDA
+ CNPJ - 41.986.330/0001-85

A chave pix é o número do CNPJ. Pedimos a gentileza de envio do comprovante de pagamento através do e-mail icicestatística@gmail.com.
A nota fiscal será enviada até 3 dias após o pagamento.
Proposta válida até ",dataprop,".

## OBSERVAÇÕES
* Não nos cabe a conclusão e argumentação dos resultados, uma vez que não detemos a “inteligência” de cada trabalho e sua respectiva área de pesquisa.
* O trabalho que desenvolvemos é único e exclusivamente para a realização da analise estatística, não é de nossa competência fazer a edição e formatação dos resultados conforme regimento de cada universidade e/ou revista.", sep="\n")}
