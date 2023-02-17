#ICC
library("irr")
#a=icc(dados[,c(114,202)], model = "twoway", type = "agreement", unit = "single")
#paste("F(",round(a$df1,0),",",round(a$df2,0),")=",round(a$Fvalue,2), ", p=",pvalor(a$p.value),", ICC=",round(a$value,3),", IC 95% = (",round(a$lbound,2),", ",round(a$ubound,2),")",sep="",collapse="")

#Cronbach

#library("ltm")
#cronbach.alpha(dados[,27:36], standardized = FALSE, CI = TRUE, probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)
