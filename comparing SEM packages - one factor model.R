# R SEM scripts - do they all match up results on the same data?

# Comparing SEM packages and models in R

#If Mplus not available, we can check the results using three opensource software packages from R.

#This example uses

#OpenMx#########################################################################

source('http://openmx.psyc.virginia.edu/getOpenMx.R')


require(OpenMx)
data(demoOneFactor)
manifests <- names(demoOneFactor)
latents <- c("G")
factorModel <- mxModel("One Factor",
                       type="RAM",
                       manifestVars = manifests,
                       latentVars = latents,
                       mxPath(from=latents, to=manifests),
                       mxPath(from=manifests, arrows=2),
                       mxPath(from=latents, arrows=2,
                              free=c(FALSE,TRUE,TRUE,TRUE,TRUE), values=1.0),
                       mxData(cov(demoOneFactor), type="cov",
                              numObs=500))
mod.run<-mxRun(factorModel)
standardizeMx(mod.run)

summary(mxRun(factorModel))

#Lavaan#########################################################################

install.packages("lavaan")
library(lavaan)

HS.model <- ' fac1  =~ x1 + x2 + x3 + x4 + x5'
fit <- cfa(HS.model, data = demoOneFactor,orthogonal=F, std.lv=TRUE)
summary(fit, fit.measures = TRUE, standardized=FALSE, rsq=TRUE)

standardizedsolution(fit)


#sem package####################################################################
install.packages("sem")
library(sem)
mydata.cov <- cov(demoOneFactor)
model.mydata <- specifyModel() 
F1 ->  x1, lam1, 1
F1 ->  x2, lam2, NA 
F1 ->  x3, lam3, NA 
F1 ->  x4, lam4, NA 
F1 ->  x5, lam5, NA 
x1 <-> x1, e1,   NA 
x2 <-> x2, e2,   NA 
x3 <-> x3, e3,   NA 
x4 <-> x4, e4,   NA 
x5 <-> x5, e5,   NA 
F1 <-> F1, NA,    NA 



mydata.sem <- sem(model.mydata, mydata.cov, nrow(demoOneFactor))
# print results (fit indices, paramters, hypothesis tests) 
summary(mydata.sem)
# print standardized coefficients (loadings) 
std.coef(mydata.sem)
