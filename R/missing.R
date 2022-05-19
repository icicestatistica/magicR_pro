aux_missing <- function(d){

d_missing = data.frame(apply(is.na(d)==F,2,sum))
d_missing = data.frame(rownames(d_missing), d_missing)
rownames(d_missing) = 1:(dim(d)[2])
names(d_missing) = c("Variável","Observações")
d_missing$`Prop. de respostas (%)`= round(100*d_missing$Observações/(dim(d)[1]),1)

return(d_missing)}
