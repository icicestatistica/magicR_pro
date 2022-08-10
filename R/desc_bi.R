desc_bi_cat <- function(linha,niveislinha=F,col,niveiscol=F,nas=F,dig=2,respcol=T){

d <- data.frame(col,linha)
names(d) <- c("col","linha")

if(niveiscol[1]==F) niveiscol = names(table(col))
if(niveislinha[1]==F) niveislinha = names(table(linha))

if(respcol==T) {
  d$linha=col;d$col=linha
  aux=niveislinha
  niveislinha=niveiscol
  niveiscol=aux}

result = desc_uni_categorica(d[d$col==niveiscol[1] & is.na(d$col)==F,]$linha,"",niveislinha,nas,T,F,F,F,F,F,dig)$result[,4]
if(length(niveiscol)>1) for (i in 2:length(niveiscol)) result <- cbind(result,desc_uni_categorica(d[d$col==niveiscol[i] & is.na(d$col)==F,]$linha,"",niveislinha,nas,T,F,F,F,F,F,dig)$result[,4])
if(nas==T) result = cbind(desc_uni_categorica(d$linha,"",niveislinha,nas,T,F,F,F,F,F,dig)$result[,c(1,4)],result) else result = cbind(desc_uni_categorica(d[is.na(d$col)==F,]$linha,"",niveislinha,nas,T,F,F,F,F,F,dig)$result[,c(1,4)],result)
if(nas==T) {result <- cbind(result, desc_uni_categorica(d[is.na(d$col)==T,]$linha,"",niveislinha,nas,T,F,F,F,F,F,dig)$result[,4])
names(result)=c("Característica","Geral",niveiscol,"N/A")} else names(result)=c("Característica","Geral",niveiscol)
if(nas==T) result <- rbind(c("Geral",dim(d)[1],desc_uni_categorica(d$col,"",niveiscol,nas,T,F,F,F,F,F,dig)$result[,2]),result) else result <- rbind(c("Geral",dim(na.omit(d))[1],desc_uni_categorica(d[is.na(d$linha)==F,]$col,"",niveiscol,nas,T,F,F,F,F,F,dig)$result[,2]),result)

if(respcol==T) result = transpordf(result)

return(result)}

desc_bi_cont <- function(cont,cat,niveiscat=F,respcol=T,nas=F,dig=2){

d=data.frame(cont,cat)
names(d)=c("cont","cat")

if(niveiscat[1]==F) niveiscat = names(table(cat))

result=desc_uni_continua(d[d$cat==niveiscat[1] & is.na(d$cat)==F,]$cont,"",30,F,F,F,dig)$result[-c(1:2,9:10),]
if(length(niveiscat)>1) for (i in 2:length(niveiscat)) result = cbind(result, desc_uni_continua(d[d$cat==niveiscat[i] & is.na(d$cat)==F,]$cont,"",30,F,F,F,dig)$result[-c(1:2,9:10),2])
if(nas==T) {result = cbind(result, desc_uni_continua(d[is.na(d$cat)==T,]$cont,"",30,F,F,F,dig)$result[-c(1:2,9:10),2])}
if (nas==F) result <- cbind(desc_uni_continua(d[is.na(d$cat)==F,]$cont,"",30,F,F,F,dig)$result[-c(1:2,9:10),],result[,-1]) else result <- cbind(desc_uni_continua(d$cont,"",30,F,F,F,dig)$result[-c(1:2,9:10),],result[,-1])
if(nas==T) names(result)=c("Característica","Geral",niveiscat,"N/A") else names(result)=c("Característica","Geral",niveiscat)
            
if(respcol==T){
result=transpordf(result)}
  
return(result)}
