

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("rtracklayer")

install.packages("bedr")
install.packages("rtracklayer")
##
library(bedr)
library(rtracklayer)

npat<-300
Nsnps<-700000
Nsnps_cols<-Nsnps*2

gen<-matrix(NA,Nsnps_cols,npat)

for(i in 1:npat)
{
 gen[,i] <- sample(LETTERS[c(1,3,7,20)],Nsnps_cols,c(0.1,0.2,0.5,0.2),replace=T) 
}

fam_ID<-sprintf("%03d", 1:npat)
ind_ID<-rep(1,npat)
gender<-sample(c(1,2),npat,c(0.5,0.5),replace = T)

gen2<-cbind(fam_ID,ind_ID,gender,gen)

export(gen,"gen.bed")

write.table(gen, "gen.bed")


gen2<-import("gen.bed", format="bed")