desc_bi_cat <- function(linha,niveislinha,col,niveiscol,margem,nas,dig){

d <- data.frame(col,linha)
if(nas==F) d <- na.omit(d)
names(d) <- c("col","linha")

if(niveiscol==F) niveiscol = names(table(col))
if(niveislinha==F) niveislinha = names(table(linha))

if(margem==2){
result = desc_uni_categorica(d[d$col==niveiscol[1] & is.na(d$col)==F,]$linha,niveislinha,nas,T,F,F,dig)[,c(1,4)]
if(length(niveiscol)>1) for (i in 2:length(niveiscol)) result <- cbind(result,desc_uni_categorica(d[d$col==niveiscol[i] & is.na(d$col)==F,]$linha,niveislinha,nas,T,F,F,dig)[,4])
if(nas==T) {result <- cbind(result, desc_uni_categorica(d[is.na(d$col)==T,]$linha,niveislinha,nas,T,F,F,dig)[,4])
names(result)=c("Característica",niveiscol,"N/A")} else names(result)=c("Característica",niveiscol)}

if(margem==1){
result = t(desc_uni_categorica(d[d$linha==niveislinha[1] & is.na(d$linha)==F,]$col,niveiscol,nas,T,F,F,dig)[,c(4)])
if(length(niveislinha)>1) for (i in 2:length(niveislinha)) result <- rbind(result,t(desc_uni_categorica(d[d$linha==niveislinha[i] & is.na(d$linha)==F,]$col,niveiscol,nas,T,F,F,dig)[,4]))
if(nas==T) {result <- rbind(result,t(desc_uni_categorica(d[is.na(d$linha)==T,]$col,niveiscol,nas,T,F,F,dig)[,4]))
result = data.frame(c(niveislinha,"N/A"),result)
names(result)=c("Característica",niveiscol,"N/A")} else {
  result = data.frame(niveislinha,result)
  names(result)=c("Característica",niveiscol)}}

return(result)}

desc_bi_cont <- function(cont,cat,niveiscat,respcol,nas,dig){

d=data.frame(cont,cat)
names(d)=c("cont","cat")

if(niveiscat[1]==F) niveiscat = names(table(cat))

result=desc_uni_continua(d[d$cat==niveiscat[1] & is.na(d$cat)==F,]$cont,dig)
if(length(niveiscat)>1) for (i in 2:length(niveiscat)) result = cbind(result, desc_uni_continua(d[d$cat==niveiscat[i] & is.na(d$cat)==F,]$cont,dig)[,2])
if(nas==T) {result = cbind(result, desc_uni_continua(d[is.na(d$cat)==T,]$cont,dig)[,2])
names(result)=c("Característica",niveiscat,"N/A")} else names(result)=c("Característica",niveiscat)

if(respcol==T){
t=data.frame(t(result[,-1]))
colnames(t)=result[,1]
result=t}

return(result)}
