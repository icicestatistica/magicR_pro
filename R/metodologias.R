metodologias <- function(analises){

n_analises = dim(analises)[1]
matanalises = c()
for (i in 1:n_analises)
matanalises= rbind(matanalises, unlist(eval(parse(text=analises$analises[i]))))
matanalises <- data.frame("Nome"=analises$Nome,matanalises)

totaltab = apply(matanalises[,2:6],2,sum)
  
if(totaltab[1]>0) {
cat("**Estatísticas Descritivas:**

*	N: Número de indivíduos em cada categoria
*	NA’s: Número de não respostas, também chamados missings ou informações faltantes
*	Min – Máx: Valores mínimo e máximo encontrados
*	1Q – 3Q: Desprezando os 25% menores valores e os 25% maiores valores, encontramos este intervalo de dados. Ou seja, a metade 'central' dos dados estão neste intervalo.
* Média: Soma de todos os valores dividido pelo número de indivíduos avaliados
*	Mediana: Valor central do conjunto de valores ordenados, ou seja, metade dos dados estão acima deste valor e a outra metade, abaixo.
*	DP: Desvio padrão. Medida de variabilidade dos dados: quanto maior, mais longe os dados estão da média.

**P-valor:**

Todos os testes estatísticos aqui apresentados serão avaliados, entre outras estatísticas, pelo p-valor. Para cada teste, há a definição de duas hipóteses:

*	Hipótese Nula: A hipótese que você deseja REJEITAR, ao encontrar evidências contra;
*	Hipótese Alternativa: Oposta a hipótese nula, será utilizada caso a hipótese nula seja rejeitada.

Exemplo: Se eu quero provar que há diferença entre dois grupos, a hipótese nula será que a diferença é 0 e a hipótese alternativa é que a diferença é diferente de 0.

O p-valor é a probabilidade de se obter os dados que você obteve considerando que a hipótese NULA é verdadeira. Se essa probabilidade for pequena (menor que o nível de significância adotado), rejeitaremos a hipótese nula em favor da hipótese alternativa.

Adotaremos neste trabalho a significância de 5%. P-valores menores significantes serão destacados com um '\*'.

**Testes Estatísticos:**

* **Shapiro-Wilk (SW):** Teste de normalidade realizado nos dados. A hipótese nula é que os dados apresentam distribuição normal e a hipótese alternativa, que não apresentam. Ao nível de significância de 5%, rejeita-se a hipótese de normalidade se o p-valor encontrado for menor que 0,05. Este teste é utilizado para determinar qual o teste estatístico adequado para ser utilizado nos dados. \n")}
