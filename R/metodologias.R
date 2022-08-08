metodologias <- function(analises){

n_analises = dim(analises)[1]
matanalises = c()
for (i in 1:n_analises)
matanalises= rbind(matanalises, unlist(eval(parse(text=analises$analises[i]))))
matanalises <- data.frame("Nome"=analises$Nome,matanalises)

totaltab = apply(matanalises[,-1],2,sum)
texto=c()
  
if(totaltab[1]>0) {
texto=c(texto,"
**Estatísticas Descritivas:**

Medidas-resumo ou estatísticas descritivas são usadas para resumir um conjunto de observações, a fim de comunicar a maior quantidade de informações da forma mais simples possível [@estatdesc]. Para variáveis numéricas, usaremos as seguintes medidas:

*	N: Número de respostas
*	NA’s: Número de não respostas, também chamados missings ou informações faltantes

**Medidas de tendência central:** Nos fornecem um valor que busca representar toda a distribuição.

* Média: Soma de todos os valores dividido pelo número de indivíduos avaliados
*	Mediana: Valor central do conjunto de valores ordenados, ou seja, metade dos dados estão acima deste valor e a outra metade, abaixo.

**Medidas de dispersão:** Medidas de dispersão é outra medida usada para mostrar como se espalhou (variação) em um conjunto de dados.

*	Min – Máx: Valores mínimo e máximo encontrados
*	1Q – 3Q: Desprezando os 25% menores valores e os 25% maiores valores, encontramos este intervalo de dados. Ou seja, a metade 'central' dos dados está neste intervalo.
*	DP: Desvio padrão. Medida de variabilidade dos dados: quanto maior, mais longe os dados estão da média.
* CV: Coeficiente de Variação: O CV considera a interpretação do DP em relação a magnitude da média (em porcentagem), onde CV = 100 × (DP / média).

Para variáveis categóricas, usaremos:

* Frequência: Simplesmente contam o número de vezes que ocorre em cada variável
* Frequência Relativa: Divide a frequência pelo total. Representa a ocorrência em valor percentual
* Frequência acumulada: Soma-se as frequências uma a uma. Útil para agrupar as categorias mais frequentes e para categorias ordinais


**P-valor:**

Todos os testes estatísticos aqui apresentados serão avaliados, entre outras estatísticas, pelo p-valor. Para cada teste, há a definição de duas hipóteses:

+	Hipótese Nula: A hipótese que você deseja REJEITAR, ao encontrar evidências contra;
+	Hipótese Alternativa: Oposta a hipótese nula, será utilizada caso a hipótese nula seja rejeitada.

Exemplo: Se eu quero provar que há diferença entre dois grupos, a hipótese nula será que a diferença é 0 e a hipótese alternativa é que a diferença é diferente de 0.
O p-valor é a probabilidade de se obter os dados que você obteve considerando que a hipótese NULA é verdadeira. Se essa probabilidade for pequena (menor que o nível de significância adotado), rejeitaremos a hipótese nula em favor da hipótese alternativa.

Adotaremos neste trabalho a significância de 5%. P-valores menores que 5% serão considerados significantes e destacados com um \\* (significante a 5%). Dois asteriscos (\\*\\*) indicam significância a 1% e três (\\*\\*\\*) indicam significância a 0,1%.


**Testes Estatísticos:**

* **Testes paramétricos e não paramétricos**

Os testes chamados \"paramétricos\" requerem suposições importantes, principalmente a de normalidade da distribuição das médias da amostra. No entanto, o teste paramétrico pode ser enganoso quando essa suposição não é satisfeita. Esse é um dos erros estatísticos mais comuns encontrados em periódicos. @parounaopar argumenta que as técnicas estatísticas não paramétricas têm a vantagem de ser mais conservadoras, com estatísticas não muito afetadas por outliers e poder ser utilizado mesmo em amostras pequenas. Por outro lado, há uma dificuldade em encontrar as diferenças reais em uma população através dessas técnicas e seus resultados são difíceis de interpretar do ponto de vista prático. Defendemos que quando os métodos paramétricos são selecionados, os pesquisadores devem garantir que todas as suposições exigidas sejam satisfeitas. Se este não for o caso, é mais válido usar métodos não paramétricos porque eles são \"sempre válidos, mas nem sempre eficientes\", enquanto os métodos paramétricos são \"sempre eficientes, mas nem sempre válidos\".

* **Teste de Shapiro-Wilk**

O teste de Shapiro-Wilk é baseado na correlação entre os dados e os escores de normalidade correspondentes e fornece maior poder do que os demais testes comumente utilizados, ou seja, tem maior capacidade de detectar se uma amostra vem de uma distribuição não normal, mesmo com diferentes tamanhos de amostra e graus de assimetria [@norm]. Ao nível de significância de 5%, rejeita-se a hipótese de normalidade se o p-valor encontrado for menor que 0,05, causando então a necessidade de recorrer a um teste não paramétrico ou a alguma outra técnica que aceite a não normalidade dos dados. \n")}

if(totaltab[3]>0) {texto <- c(texto,"
* **Teste t:**

Teste representado pela letra \"c\" ao lado do p-valor. Este é um teste paramétrico em que temos uma variável nominal que divide a população em duas categorias (Exemplo: Sexo feminino e masculino; Doente ou não doente;  Desfecho positivo ou negativo) e uma variável numérica a ser comparada. O objetivo do teste é verificar se a média da população definida por uma categoria é diferente da média da outra população. Como este é um teste paramétrico, depende da hipótese de que ambas as populações possuem distribuição normal, o que é testado aqui através do teste de Shapiro-Wilk. No teste-t de Student, há também a suposição de igualdade de variâncias nas duas populações, que segundo @field, pode ser abandonada caso seja usada a correção de Welch dos graus de liberdade (Capítulo 9.4). Por este motivo, como sugerido, utilizamos a correção para ganhar maior robustez. O tamanho do efeito calculado é a estatística d de cohen [@cohen], que informa quantos desvios-padrão (DP) de diferença existem entre os resultados dos dois grupos em comparação. Cohen propôs quantificar a magnitude do efeito em: Pequeno (d = 0.2 – 0.4), Médio  (d = 0.4 – 0.8), Grande (d = maior que 0.8). Também significa que se a diferença entre as médias de dois grupos for menor que 0.2 DP, a diferença é desprezível, mesmo que seja estatisticamente significativa. \n")}

if(totaltab[4]>0) {texto <- c(texto,"
* **Mann-Whitney:**

Teste representado pela letra “d” ao lado do p-valor. Neste teste não paramétrico, temos uma variável nominal que divide a população em duas categorias (Exemplo: Sexo feminino e masculino; Doente ou não doente;  Desfecho positivo ou negativo). O objetivo do teste é verificar se uma variável numérica não normal ou categórica ordinal é diferente entre as duas categorias. Sendo assim, a hipótese nula é que a variável testada é igual entre os dois grupos e a alternativa, que a variável é diferente entre os grupos. Ao nível de significância de 5%, rejeita-se a hipótese de igualdade entre os dois grupos se o p-valor encontrado for menor que 0,05. O tamanho do efeito calculado é a estatística r, que é calculada como Z dividido pela raiz quadrada do total de observações. Esta estatística relata um tamanho de efeito, em que quanto mais distante do 0, maior o efeito. Quando os dados no primeiro grupo são maiores do que no segundo grupo, r é positivo. Quando os dados no segundo grupo são maiores do que no primeiro grupo, r é negativo. Comumente, considera-se r > 0.1 como efeito pequeno, r > 0.3 efeito médio e r > 0.5 grande. \n")}
  
if(totaltab[7]>0) {texto <- c(texto,"
* **Correlação:**

Teste representado de duas formas: 

  +   pela letra ‘h’ ao lado do p-valor e pela letra r, se correlação de Pearson; \n
  +   pela letra ‘g’ e a letra grega $\\rho$ (pronuncia=se ‘rô”) se correlação de spearman. \n

Estas estatísticas medem o grau da correlação (e a direção dessa correlação - se positiva ou negativa) entre duas variáveis de escala métrica (intervalar ou de razão). Elas assumem apenas valores entre -1 e 1, em que se o sinal da correlação for negativa, isso indica uma associação indireta entre as variáveis (Os indivíduos com maiores valores em uma variável possuem menores valores na outra) enquanto um sinal positivo indica uma relação direta (Os indivíduos com maiores valores em uma variável possuem também maiores valores na outra). Quanto mais distante de 0, mais forte é a correlação. A hipótese nula do teste é que a correlação é igual a 0 (Não existe nenhum tipo de correlação), testado contra a hipótese de que ela é diferente de 0. P-valores menores que 0,05 indicam que a correlação encontrada é de fato distante o suficiente do 0. A diferença entre esses dois tipos de correlação é que a correlação de Spearman é um teste não paramétrico, que não depende da magnitude dos números, e sim somente da ordem (rank) em que elas ocorrem. Por ser não paramétrico, não depende da suposição de normalidade, enquanto a correlação de Pearson depende desta suposição [@field] (capítulo 6.5). A normalidade das variáveis foi testada com Shapiro-wilk [@norm].

A magnitude do efeito foi classificada com @cohen92, sendo uma das medidas mais usuais na literatura, que os separa em ‘grande’ (de 0,50 a 1,00); ‘moderada’ (de 0,30 a 0,49), ‘pequena’ (de 0,10 a 0,29) e ‘irrisória’ (menor que 0,1). @field também sugere o cálculo do coeficiente de determinação, que consiste no quadrado da correlação e indica a proporção de variabilidade compartilhada entre as variáveis, no caso da correlação de Pearson e entre os ranks, no caso da correlação de Spearman. O mesmo autor também indica o cálculo de Intervalos de confiança, em que *bootstrap* (BCa) com 1000 reamostragens foi utilizado para o cômputo do IC da correlação de Spearman, uma vez que não há uma fórmula fechada para esta estatística. \n")}

texto = c(texto,"# Sobre as análises \n","

As presentes análises foram feitas através do software livre R (",R.version.string,"). Você pode citá-lo da seguinte forma:

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <https://www.R-project.org/>.

A confecção de gráficos pode ser solicitada por 6 meses e o prazo de entrega é de 5 dias úteis. \n

Neste relatório, foi utilizado como padrão o arredondamento de ",dig," casas decimais. Os números com menos que ",dig," dígitos apenas omitem 'zeros' à direita, e portanto correspondem ao valor exato. \n

Qualquer dúvida sobre o conteúdo deste relatório pode ser tirada via e-mail ou whatsapp. Lembrando que também está inclusa uma reunião de 30 minutos para tirar dúvidas. Recomendo realizar o agendamento da reunião apenas depois de uma leitura minuciosa do relatório para seu melhor aproveitamento. \n

Fico à disposição!")

return(texto)}
