googlesheets4::gs4_deauth()
bib=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1LIjyLRj2Yr-wD8rwUQzgpA8PhQsLz2KyLEC9mLC9kKU/edit#gid=0")

ref <- function(chave,cont,bibliografia){
bibli = bib$Bibliografia[bib$ref==chave]
cont=cont+1
marc = paste("[",cont,"]", sep="")
bibliografia = c(bibliografia, paste(marc, " ",bibli, sep=""))
return(list("marc"=marc,"cont"=cont,"bibliografia"=bibliografia))}
