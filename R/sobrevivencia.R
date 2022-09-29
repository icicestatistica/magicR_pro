sobrevivencia = function(wpm){
df = na.omit(tidyr::pivot_longer(data=wpm, cols=c("t30","t60","t90"),names_to = "Tempo",values_to = "Ocorrência"))

library(survival)
m=survfit(Surv(as.numeric(str_sub(Tempo,start=2)),Ocorrência)~Tratamento,data=df)
library(survminer)
ggsurv = survminer::ggsurvplot(m, data = df, conf.int = T,pval = T,xlab = "Tempo em dias",   # customize X axis label.
           break.time.by = 30,palette = c("#E7B800", "#2E9FDF"))
ggsurv$plot + labs(y="Percentual sem brotação", title="Análise de sobrevivência: Comparando o tempo \n até brotação nos grupos") + theme(plot.title=element_text(hjust=0.5))
}
