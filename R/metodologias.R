metodologias <- function(analises){

mat_analises = table(factor(analises$tipo, levels=c("numeric","factor","catsame","t","mw","aov1","kw","correl","cc")))

texto=c("# Metodologias \n","

**Estatísticas Descritivas:**

Medidas-resumo ou estatísticas descritivas são usadas para resumir um conjunto de observações, a fim de comunicar a maior quantidade de informações da forma mais simples possível [@estatdesc]. ")
  
if(mat_analises["numeric"]>0) {
texto=c(texto,"Para variáveis numéricas, usaremos as seguintes medidas:

*	N: Número de respostas
*	NA’s: Número de não respostas, também chamados missings ou informações faltantes

**Medidas de tendência central:** Nos fornecem um valor que busca representar toda a distribuição.

* Média: Soma de todos os valores dividido pelo número de indivíduos avaliados
*	Mediana: Valor central do conjunto de valores ordenados, ou seja, metade dos dados estão acima deste valor e a outra metade, abaixo.

**Medidas de dispersão:** Medidas de dispersão é outra medida usada para mostrar como se espalhou (variação) em um conjunto de dados.

*	Min – Máx: Valores mínimo e máximo encontrados
*	1Q – 3Q: Desprezando os 25% menores valores e os 25% maiores valores, encontramos este intervalo de dados. Ou seja, a metade 'central' dos dados está neste intervalo.
*	DP: Desvio padrão. Medida de variabilidade dos dados: quanto maior, mais longe os dados estão da média.
* CV: Coeficiente de Variação: O CV considera a interpretação do DP em relação a magnitude da média (em porcentagem), onde CV = 100 × (DP / média).")}

if(mat_analises["factor"]>0) {texto=c(texto,"Para variáveis categóricas, usaremos:

* Frequência: Simplesmente contam o número de vezes que ocorre em cada variável;
* Frequência Relativa: Divide a frequência pelo total. Representa a ocorrência em valor percentual. \n")}


if(sum(mat_analises)>(mat_analises["factor"] + mat_analises["numeric"])) texto=c(texto,"**P-valor:**

Todos os testes estatísticos aqui apresentados serão avaliados, entre outras estatísticas, pelo p-valor. Para cada teste, há a definição de duas hipóteses:

+	Hipótese Nula: A hipótese que você deseja REJEITAR, ao encontrar evidências contra;
+	Hipótese Alternativa: Oposta a hipótese nula, será utilizada caso a hipótese nula seja rejeitada.

Exemplo: Se eu quero provar que há diferença entre dois grupos, a hipótese nula será que a diferença é 0 e a hipótese alternativa é que a diferença é diferente de 0.
O p-valor é a probabilidade de se obter os dados que você obteve considerando que a hipótese NULA é verdadeira. Se essa probabilidade for pequena (menor que o nível de significância adotado), rejeitaremos a hipótese nula em favor da hipótese alternativa.

Adotaremos neste trabalho a significância de 5%. P-valores menores que 5% serão considerados significantes e destacados com um \\* (significante a 5%). Dois asteriscos (\\*\\*) indicam significância a 1% e três (\\*\\*\\*) indicam significância a 0,1%. \n")

if(sum(mat_analises[c("t","mw","aov1","correl")])>0) {texto=c(texto,"**Testes Estatísticos:**

* **Testes paramétricos e não paramétricos**

Os testes chamados \"paramétricos\" requerem suposições importantes, principalmente a de normalidade da distribuição das médias da amostra. No entanto, o teste paramétrico pode ser enganoso quando essa suposição não é satisfeita. Esse é um dos erros estatísticos mais comuns encontrados em periódicos. @parounaopar argumenta que as técnicas estatísticas não paramétricas têm a vantagem de ser mais conservadoras, com estatísticas não muito afetadas por outliers e poder ser utilizado mesmo em amostras pequenas. Por outro lado, há uma dificuldade em encontrar as diferenças reais em uma população através dessas técnicas e seus resultados são difíceis de interpretar do ponto de vista prático. Defendemos que quando os métodos paramétricos são selecionados, os pesquisadores devem garantir que todas as suposições exigidas sejam satisfeitas. Se este não for o caso, é mais válido usar métodos não paramétricos porque eles são \"sempre válidos, mas nem sempre eficientes\", enquanto os métodos paramétricos são \"sempre eficientes, mas nem sempre válidos\".

* **Teste de Shapiro-Wilk**

O teste de Shapiro-Wilk é baseado na correlação entre os dados e os escores de normalidade correspondentes e fornece maior poder do que os demais testes comumente utilizados, ou seja, tem maior capacidade de detectar se uma amostra vem de uma distribuição não normal, mesmo com diferentes tamanhos de amostra e graus de assimetria [@norm]. Ao nível de significância de 5%, rejeita-se a hipótese de normalidade se o p-valor encontrado for menor que 0,05, causando então a necessidade de recorrer a um teste não paramétrico ou a alguma outra técnica que aceite a não normalidade dos dados. \n")}

if(mat_analises["t"]>0) {texto <- c(texto,"
* **Teste t:**

Teste representado pela letra \"c\" ao lado do p-valor. Este é um teste paramétrico em que temos uma variável nominal que divide a população em duas categorias (Exemplo: Sexo feminino e masculino; Doente ou não doente;  Desfecho positivo ou negativo) e uma variável numérica a ser comparada. O objetivo do teste é verificar se a média da população definida por uma categoria é diferente da média da outra população. Como este é um teste paramétrico, depende da hipótese de que ambas as populações possuem distribuição normal, o que é testado aqui através do teste de Shapiro-Wilk. No teste-t de Student, há também a suposição de igualdade de variâncias nas duas populações, que segundo @field, pode ser abandonada caso seja usada a correção de Welch dos graus de liberdade (Capítulo 9.4). Por este motivo, como sugerido, utilizamos a correção para ganhar maior robustez. O tamanho do efeito calculado é a estatística d de cohen [@cohen], que informa quantos desvios-padrão (DP) de diferença existem entre os resultados dos dois grupos em comparação. Cohen propôs quantificar a magnitude do efeito em: Pequeno (d = 0.2 – 0.4), Médio  (d = 0.4 – 0.8), Grande (d = maior que 0.8). Também significa que se a diferença entre as médias de dois grupos for menor que 0.2 DP, a diferença é desprezível, mesmo que seja estatisticamente significativa. \n")}

if(mat_analises["mw"]>0) {texto <- c(texto,"
* **Mann-Whitney:**

Teste representado pela letra “d” ao lado do p-valor. Neste teste não paramétrico, temos uma variável nominal que divide a população em duas categorias (Exemplo: Sexo feminino e masculino; Doente ou não doente;  Desfecho positivo ou negativo). O objetivo do teste é verificar se uma variável numérica não normal ou categórica ordinal é diferente entre as duas categorias. Sendo assim, a hipótese nula é que a variável testada é igual entre os dois grupos e a alternativa, que a variável é diferente entre os grupos. Ao nível de significância de 5%, rejeita-se a hipótese de igualdade entre os dois grupos se o p-valor encontrado for menor que 0,05. O tamanho do efeito calculado é a estatística r, que é calculada como Z dividido pela raiz quadrada do total de observações. Esta estatística relata um tamanho de efeito, em que quanto mais distante do 0, maior o efeito. Quando os dados no primeiro grupo são maiores do que no segundo grupo, r é positivo. Quando os dados no segundo grupo são maiores do que no primeiro grupo, r é negativo. Comumente, considera-se r > 0.1 como efeito pequeno, r > 0.3 efeito médio e r > 0.5 grande. \n")}

if(mat_analises["aov1"]>0) {texto <- c(texto, "
* **Anova one-way:**

Teste representado pela letra “e” ao lado do p-valor. Neste teste paramétrico, temos uma variável nominal que divide a população em três ou mais categorias. O objetivo do teste é verificar se as médias de uma certa variável resposta numérica com distribuição normal difere entre as categorias. Sendo assim, a hipótese nula é que a média de todas as categorias são iguais e a alternativa, que há pelo menos uma categoria diferente das demais. Ao nível de significância de 5%, rejeita-se a hipótese de igualdade entre todos os grupos se o p-valor encontrado for menor que 0,05. O tamanho do efeito para a One Way ANOVA é o eta-quadrado, que assume valores de 0 a 1 e multiplicada por 100 indica a porcentagem de variância na variável dependente explicada pela variável independente. Os valores de interpretação comumente encontrados na literatura publicada são: 0,01-0,06 (efeito pequeno), 0,06-0,14 (efeito moderado) e >= 0,14 (efeito grande). \n

* **Teste HSD de Tukey:**

Ao comparar as médias para os níveis de um fator em uma análise de variância (anova), uma comparação simples usando testes t aumentará a probabilidade de declarar uma diferença significativa quando ela não está de fato presente. Tukey introduziu intervalos com base no intervalo das médias da amostra, e não nas diferenças individuais. Os intervalos retornados por esta função são baseados nas estatísticas de intervalo estudentizado, e são úteis para avaliar quais os grupos cujas médias diferem. \n")}

if(mat_analises["aov1"]>0) {texto <- c(texto, "
* **Kruskall-Wallis:**

Teste representado pela letra “f” ao lado do p-valor. Neste teste não paramétrico, temos uma variável nominal que divide a população em três ou mais categorias. O objetivo do teste é verificar se há pelo menos uma categoria que difere das demais. Sendo assim, a hipótese nula é que todas as categorias são iguais e a alternativa, que há pelo menos uma categoria diferente das demais. Ao nível de significância de 5%, rejeita-se a hipótese de igualdade entre todos os grupos se o p-valor encontrado for menor que 0,05. O tamanho do efeito para o teste de Kruskal-Wallis é o eta-quadrado, que assume valores de 0 a 1 e multiplicada por 100 indica a porcentagem de variância na variável dependente explicada pela variável independente. Os valores de interpretação comumente encontrados na literatura publicada são: 0,01-0,06 (efeito pequeno), 0,06-0,14 (efeito moderado) e >= 0,14 (efeito grande). \n

* **Teste de Dunn:**

Após um resultado significativo no teste de Kruskall Wallis, sabemos que há pelo menos um grupo que difere dos demais. Para saber quais são os grupos que diferem entre si, realizamos o teste de Dunn, que compara todos os pares de grupos. Assim, a hipótese nula é que dois grupos são iguais. P-valores menores que 0,05 indicam que os dois grupos testados são diferentes. \n")}
                            
if(mat_analises["correl"]>0) {texto <- c(texto,"
* **Correlação:**

Teste representado de duas formas: 

  +   pela letra ‘h’ ao lado do p-valor e pela letra r, se correlação de Pearson; \n
  +   pela letra ‘g’ e a letra grega $\\rho$ (pronuncia=se ‘rô”) se correlação de spearman. \n

Estas estatísticas medem o grau da correlação (e a direção dessa correlação - se positiva ou negativa) entre duas variáveis de escala métrica (intervalar ou de razão). Elas assumem apenas valores entre -1 e 1, em que se o sinal da correlação for negativa, isso indica uma associação indireta entre as variáveis (Os indivíduos com maiores valores em uma variável possuem menores valores na outra) enquanto um sinal positivo indica uma relação direta (Os indivíduos com maiores valores em uma variável possuem também maiores valores na outra). Quanto mais distante de 0, mais forte é a correlação. A hipótese nula do teste é que a correlação é igual a 0 (Não existe nenhum tipo de correlação), testado contra a hipótese de que ela é diferente de 0. P-valores menores que 0,05 indicam que a correlação encontrada é de fato distante o suficiente do 0. A diferença entre esses dois tipos de correlação é que a correlação de Spearman é um teste não paramétrico, que não depende da magnitude dos números, e sim somente da ordem (rank) em que elas ocorrem. Por ser não paramétrico, não depende da suposição de normalidade, enquanto a correlação de Pearson depende desta suposição [@field] (capítulo 6.5). A normalidade das variáveis foi testada com Shapiro-wilk [@norm].

A magnitude do efeito foi classificada com @cohen92, sendo uma das medidas mais usuais na literatura, que os separa em ‘grande’ (de 0,50 a 1,00); ‘moderada’ (de 0,30 a 0,49), ‘pequena’ (de 0,10 a 0,29) e ‘irrisória’ (menor que 0,1). @field também sugere o cálculo do coeficiente de determinação, que consiste no quadrado da correlação e indica a proporção de variabilidade compartilhada entre as variáveis, no caso da correlação de Pearson e entre os ranks, no caso da correlação de Spearman. O mesmo autor também indica o cálculo de Intervalos de confiança, em que *bootstrap* (BCa) com 1000 reamostragens foi utilizado para o cômputo do IC da correlação de Spearman, uma vez que não há uma fórmula fechada para esta estatística. \n")}
  
if(mat_analises["cc"]>0) {texto <- c(texto,"
* **Teste Chi-Quadrado:**

O teste Qui-Quadrado (marcado pela letra “a”) é usado para descobrir se existe uma associação entre a variável da linha e a variável da coluna em uma tabela de contingência construída à partir de dados da amostra. Para realização do teste, se faz necessário calcular o valor esperado de cada célula, que pode ser interpretado como o valor esperado para ocorrer sub a hipótese de independência. Quanto mais diferente o observado for com relação ao esperado, maior evidência de que as variáveis estão associadas. Ao nível de significância de 5%, rejeita-se a hipótese de independência entre todos os grupos se o p-valor encontrado for menor que 0,05. Caso estejamos comparando mais de dois grupos, uma análise dos resíduos nos dá a direção para entender quais caselas possuem mais ou menos observações do que o esperado. O tamanho do efeito mais utilizado no ambiente das análises de Qui-quadrado é o V de Cramer. Esta estatística gera valores 0-1. A interpretação é baseada nos graus de liberdade (em parênteses, em frente ao v) e é feita da seguinte maneira:

  +   Para 1 grau de liberdade, v<0.1 indica um efeito irrisório, v entre 0.1 e 0.3 um efeito pequeno, v entre 0.3 e 0.5 um efeito médio e maior que 0.5, um efeito grande;
  +   Para 2 graus de liberdade, v<0.07 indica um efeito irrisório, v entre 0.07 e 0.21 um efeito pequeno, v entre 0.21 e 0.35 um efeito médio e maior que 0.35, um efeito grande;
  +   Para 3 ou mais graus de liberdade, v<0.06 indica um efeito irrisório, v entre 0.06 e 0.17 um efeito pequeno, v entre 0.17 e 0.29 um efeito médio e maior que 0.29, um efeito grande. \n

* **Teste Exato de Fisher:**

O teste exato de Fisher (marcado pela letra “b”) é utilizado quando uma das suposições do teste qui-quadrado é violada (quando mais de 20% dos valores esperados são menores que 5). O objetivo do teste é o mesmo do qui-quadrado: testar a hipótese de independência entre duas variáveis. Ao nível de significância de 5%, rejeita-se a hipótese de independência entre todos os grupos se o p-valor encontrado for menor que 0,05. Caso estejamos comparando mais de dois grupos, um teste post-hoc ajusta p-valores para cada comparação entre dois grupos para analisarmos quais são os grupos que estão associados entre si. Não há medidas de tamanho de efeito para esse teste. \n")}

if(mat_analises["catsame"]>0) {texto <- c(texto,"
* **Teste Z para igualdade de proporções:**

O teste-Z para igualdade de proporções é utilizado para comparar a frequência de uma certa resposta em dois grupos. Utilizando a aproximação normal, como um teste bicaudal (que testa a diferença entre dois grupos), podemos concluir se as proporções diferem ou não e também calcular um intervalo de confiança para cada proporção obtida, extrapolando um intervalo de valores plausíveis para a proporção verdadeira. \n")}

texto = c(texto,"\n","# Sobre as análises \n","

As presentes análises foram feitas através do software livre R (",R.version.string,"). Você pode citá-lo da seguinte forma:

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <https://www.R-project.org/>.

A confecção de gráficos pode ser solicitada por 6 meses e o prazo de entrega é de 5 dias úteis. \n

Neste relatório, foi utilizado como padrão o arredondamento de ",dig," casas decimais. Os números com menos que ",dig," dígitos apenas omitem 'zeros' à direita, e portanto correspondem ao valor exato. \n

Qualquer dúvida sobre o conteúdo deste relatório pode ser tirada via e-mail ou whatsapp. Lembrando que também está inclusa uma reunião de 30 minutos para tirar dúvidas. Recomendo realizar o agendamento da reunião apenas depois de uma leitura minuciosa do relatório para seu melhor aproveitamento. \n

Fico à disposição!")

return(texto)}
