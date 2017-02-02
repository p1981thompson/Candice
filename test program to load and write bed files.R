

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("rtracklayer")

install.packages("bedr")
install.packages("rtracklayer")
##
library(bedr)
library(rtracklayer)

gen<-as.data.frame(matrix(NA,1000,300))

for(i in 1:300)
{
 gen[,i] <- sample(LETTERS[c(1,3,7,20)],1000,c(0.1,0.2,0.5,0.2),replace=T) 
}

import("example.txt", format="bed")

export

write.table(gen, "gen.bed", sep = "\t")
