metodo = function(analises){
  
  final = NULL
  contagem = data.frame("numeric"=F,"ordinal"=F,"factor"=F,"catsame"=F,"toumw"=F,"anovaoukruskall"=F,"cc"=F,"correl"=F,"t_par"=F)
  
  sess = unique(analises$Sessão)
  
  for (i in sess) {
    
    parte=analises[analises$Sessão==i,]
  
    desc=paste0("\n **",i,"**: \n")
  
      ## Numéricas
  
    cont=table(factor(parte$tipo,levels=c("numeric","ordinal","factor","catsame","t","mw","aov","kw","cc","correl","t_par")))>0
  
    if (cont[1]>0) {
      if (contagem$numeric==F) {desc = c(desc,paste0("\n + Descrevemos as variáveis ", printvetor(parte$Nome2[parte$tipo=="numeric"], aspas=F), " com objetivo de representar a distribuição dos dados. Para isso, foram utilizadas medidas de tendência central (Média e mediana) e medidas de dispersão, como o desvio padrão, os quartis (Primeiro e Terceiro), o mínimo e o máximo. O número de outliers foi computado verificando observações 1.5 vezes o intervalo interquartil acima ou abaixo dos quartis, além de ser verificado o número de missings (perda de informações). Ressaltamos que segundo [@mishra_descriptive_2019], em casos de presença de outliers, assimetria ou não normalidade da distribuição, as medidas baseadas em posição (mediana, quartis, mínimo e máximo) são mais adequadas, enquanto para distribuições próximas da Gaussiana, medidas como a média e o desvio padrão representam melhor a distribuição dos dados e podem ser utilizadas em testes estatísticos.")) ; contagem$numeric=T} else
        desc = c(desc,paste0("\n + Descrevemos as variáveis ",printvetor(parte$Nome2[parte$tipo=="numeric"],aspas=F)," por meio das estatísticas descritivas de tendência central e dispersão, utilizando as mesmas medidas citadas anteriormente."))
    }
  
      ## Categóricas
  
      if (cont[2] + cont[3]>0) {desc = c(desc,paste0("\n+ Descrevemos as variáveis ",printvetor(parte$Nome2[parte$tipo %in% c("ordinal","factor")],aspas=F)," por meio do cálculo das frequências absoluta e relativa de cada categoria."))}
    
    
    ## Catsame
    
    if (cont[4]>0) {desc = c(desc,paste0("\n+ Descrevemos as variáveis ",printvetor(parte$Nome2[parte$tipo %in% c("catsame")],aspas=F)," por meio do cálculo das frequências absoluta e relativa de cada categoria, seguido do intervalo de 95% de confiança para a estimativa percentual, segundo recomendado em [@vollset_confidence_1993]."))}
    
    ## Teste-t/Mann-whitney
    
    if (cont[5]+cont[6]>0) {
      if (contagem$toumw==F) {desc = c(desc,paste0("\n+ Comparamos as variáveis ",printvetor(unique(parte$Nome1[parte$tipo %in% c("t","mw")]),aspas=F)," para cada categoria de ",printvetor(unique(parte$Nome2[parte$tipo %in% c("t","mw")]),aspas=F)," através dos teses de comparação de dois grupos levando em consideração a natureza das variáveis resposta. O teste-t foi utilizado quando a suposição de normalidade da distribuição de variáveis numéricas não foi rejeitada pelo teste Shapiro-Wilk e o Mann-whitney quando não foi acatada ou quando a variável resposta é categórica ordinal, segundo preconiza @s_nonparametric_2016 e @andy_field_discovering_2012. Em conjunto com o p-valor, foi calculado o tamanho de efeito para cada teste, sendo utilizado o 'd de Cohen' para o teste-t e a estatística 'r' para o teste Mann-whitney. A interpretação do tamanho de efeito é feita segundo @cohen_statistical_1992, que propôs d = 0.2 a 0.3 como pequeno, d = 0.5 a 0.8 como médio e d>0.8 como grande, bem como r > 0.1 como efeito pequeno, r > 0.3 efeito médio e r > 0.5 grande." )); contagem$toumw=T} else {
        desc = c(desc,paste0("\n+ Comparamos as variáveis ",printvetor(unique(parte$Nome1[parte$tipo %in% c("t","mw")]),aspas=F)," para cada categoria de ",printvetor(unique(parte$Nome2[parte$tipo %in% c("t","mw")]),aspas=F)," através dos teses de comparação de dois grupos levando em consideração a natureza das variáveis resposta (Teste-t ou Mann-whitney), segundo as condições já descritas acima." ))}}
    
    
    ## ANOVA/Kruskall-Wallis
    
    if (cont[7]+cont[8]>0) {
      if (contagem$anovaoukruskall==F) {desc = c(desc,paste0("\n+ Comparamos as variáveis ",printvetor(unique(parte$Nome1[parte$tipo %in% c("aov","kw")]),aspas=F)," para cada categoria de ",printvetor(unique(parte$Nome2[parte$tipo %in% c("aov","kw")]),aspas=F)," através dos teses de comparação de mais de dois grupos levando em consideração a natureza das variáveis resposta. O teste ANOVA foi utilizado quando a suposição de normalidade dos resíduos foi acatada pelo teste Shapiro-Wilk e o Kruskall-Wallis quando não foi acatada ou quando a variável resposta é categórica ordinal, segundo preconiza @s_nonparametric_2016 e @andy_field_discovering_2012. Em conjunto com o p-valor, foi calculado o tamanho de efeito 'eta quadrado'. Segundo @cohen_statistical_1992, interpretamos da seguinte forma: 0,01-0,06 (efeito pequeno), 0,06-0,14 (efeito moderado) e >= 0,14 (efeito grande). Esses testes são globais, e quando p<0.05 interpretamos que há pelo menos um grupo com respostas diferentes dos demais. Para determinar qual o(s) grupo(s) que difere(m), são realizados testes de comparação pareada, sendo o teste HSD de tukey [@tukey] o adequado para a ANOVA e o teste de Dunn [@dunn_multiple_1964] adequado para o kruskall-Wallis, ambos com correção de Bonferroni." )); contagem$anovaoukruskall=T} else {
        desc = c(desc,paste0("\n+ Comparamos as variáveis ",printvetor(unique(parte$Nome1[parte$tipo %in% c("aov","kw")]),aspas=F)," para cada categoria de ",printvetor(unique(parte$Nome2[parte$tipo %in% c("aov","kw")]),aspas=F)," através dos teses de comparação de mais de dois grupos levando em consideração a natureza das variáveis resposta (ANOVA ou kruskall-wallis), segundo as condições já descritas acima." ))}}

    ## Exato de Fisher/Qui-quadrado
    
    if (cont[9]>0) {
      if (contagem$cc==F) {desc = c(desc,paste0("\n+ Verificamos a associação entre  ",printvetor(unique(parte$Nome1[parte$tipo %in% c("cc")]),aspas=F)," e ",printvetor(unique(parte$Nome2[parte$tipo %in% c("cc")]),aspas=F)," através de testes de associação estatística. O teste Qui-quadrado foi utilizado quando a suposição de não mais de 20% dos valores esperados serem inferiores a 5 e nenhum valor esperado inferior a 1 , enquanto o teste Exato de Fisher foi utilizado quando essa suposição foi violada, segundo @agresti_2007. Em conjunto com o p-valor, para o teste qui-quadrado foi calculado o tamanho de efeito 'V de Cramer'. Interpretamos segundo @cohen_statistical_1988, de acordo com os graus de liberdade." )); contagem$cc=T} else {
        desc = c(desc,paste0("\n+ Verificamos a associação entre  ",printvetor(unique(parte$Nome1[parte$tipo %in% c("cc")]),aspas=F)," e ",printvetor(unique(parte$Nome2[parte$tipo %in% c("cc")]),aspas=F)," através de testes de associação estatística adequados (Qui-quadrado ou Exato de Fisher), segundo as condições já descritas acima." ))}}   
    
    ## Correlação
    
    if (cont[10]>0) {
      if (contagem$correl==F) {desc = c(desc,paste0("\n+ Verificamos a correlação entre  ",printvetor(unique(parte$Nome1[parte$tipo %in% c("correl")]),aspas=F)," e ",printvetor(unique(parte$Nome2[parte$tipo %in% c("correl")]),aspas=F)," através de testes de correlação estatística. O teste de correlação de pearson avalia relações lineares, portanto é utilizado quando a suposição de normalidade de ambas as variáveis foi suportada, enquanto para variáveis numéricas não normais ou para variáveis categóricas ordinais foi utilizada a correlação de spearman, que avalia relações monótonas, mas não necessariamente lineares [@myles_1973]. A própria correlação é considerada o tamanho de efeito, pois quanto mais distante de 0, mais forte a correlação. Interpretamos segundo @cohen_statistical_1988: “grande” 0,50 a 1,00; “moderada” de 0,30 a 0,49 e “pequena” de 0,10 a 0,29." )); contagem$correl=T} else {
        desc = c(desc,paste0("\n+ Verificamos a associação entre  ",printvetor(unique(parte$Nome1[parte$tipo %in% c("correl")]),aspas=F)," e ",printvetor(unique(parte$Nome2[parte$tipo %in% c("correl")]),aspas=F)," através de testes de correlação estatística adequados (Spearman ou Pearson), segundo as condições de normalidade e natureza das variáveis." ))}}

    ## Pareado wilcoxon
    
    if (cont[11]>0) {
      if (contagem$t_par==F) {desc = c(desc,paste0("\n+ Comparamos as variáveis  ",printvetor(unique(parte$Nome2[parte$tipo %in% c("t_par")]),aspas=F)," em diferentes  ",printvetor(unique(parte$Nome1[parte$tipo %in% c("t_par")]),aspas=F)," através do teste não paramétrico de Wilcoxon [@myles_1973]. Em conjunto com o p-valor, foi calculado o tamanho de efeito r [@maciej_2014], com a seguinte interpretação: r > 0.1 como efeito pequeno, r > 0.3 efeito médio e r > 0.5 grande." )); contagem$t_par=T} else {
        desc = c(desc,paste0("\n+ Comparamos as variáveis  ",printvetor(unique(parte$Nome2[parte$tipo %in% c("t_par")]),aspas=F)," em diferentes  ",printvetor(unique(parte$Nome1[parte$tipo %in% c("t_par")]),aspas=F)," através do teste não paramétrico de Wilcoxon em conjunto com o tamanho de efeito r." ))}}  
      
    final = c(final, paste0(desc,collapse="\n"))
  }

  return(final)}
