################################################################################
#
# Phenotype generator - takes input from REDcap and then generates based on data.
#
################################################################################
install.packages(c("data.table","scales","lavaan"))
library(devtools) # to run script from gist and bring in data
library(lavaan)
library(semPlot)
library(knitr)
library(ggplot2)
library(reshape2)
library(psych)


# 06-02-2017

# Connect to redcap database

library(redcapAPI)
options(redcap_api_url = [REDcap URL]])

#Need to request an API token for each project.

rcon <- redcapConnection("redcap url", "token")

  myProject <- redcapProjectInfo(rcon)
  redcap.data <- exportRecords(rcon, proj=myProject)
  #save(toyData, myProject, file="toy.Rdata")


#Clean data if required
  
  library(car)
  
  recode()

################################################################################
################################################################################
# SEM program. 
################################################################################


##http://rstudio-pubs-static.s3.amazonaws.com/9923_8c2532ee1f154554a7aa91e1ef8fe0fa.html


#Which data for model

data.f1<-redcap.data[,c("","","","")]

#plot variable distributions
mydata.melt <- melt(data.f1)
ggplot(mydata.melt, aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") +
  geom_density() 

#Single factor model

model.f1 <- ' f1  =~ x1 + x2 + x3 + x4 + x5'
fit.mod.A <- cfa(model.f1, data = data.f1,orthogonal=T)
summary(fit.mod.A, fit.measures = TRUE, standardized=FALSE, rsq=TRUE)

semPaths(fit.mod.A, "std", title = FALSE, 
         nCharNodes=0, edge.label.cex=0.6,esize=0.5)

standardizedsolution(fit)

# model with cross loading factors

mod.C <- '
  f1 =~ v1 + v2 + v3 + v4
f2 =~ v4 + v5 + v6 + v7
f3 =~ v7 + v8 + v9
'
fit.mod.C <- cfa(mod.C, data = mydata, ordered=items)
semPaths(fit.mod.C, "std", title = FALSE,
         nCharNodes=0, edge.label.cex=0.6)

#Bifactor model ################################################################

mod.E <- '
  g =~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9
  f1 =~ v1 + v2 + v3
  f2 =~ v4 + v5 + v6
  f3 =~ v7 + v8 + v9
  '
fit.mod.E <- cfa(mod.E, data = mydata, ordered=items, orthogonal=TRUE, std.lv=TRUE)
semPaths(fit.mod.E, "std", title = FALSE,
         nCharNodes=0, edge.label.cex=0.6, rotate=2)


##Hierarchical model ###########################################################

mod.D <- '
  g =~ f1 + f2 + f3
f1 =~ v1 + v2 + v3
f2 =~ v4 + v5 + v6
f3 =~ v7 + v8 + v9
'
fit.mod.D <- cfa(mod.D, data = mydata, ordered=items, std.lv=TRUE)
semPaths(fit.mod.D, "std", title = FALSE,
         nCharNodes=0, edge.label.cex=0.6)



#  

m.modelfit.2 <- cfa(m.model.2, data = bifac, std.lv = TRUE, information="observed")





