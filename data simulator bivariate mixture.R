library(distr)

## Construct the distribution object.
myMix <- UnivarMixingDistribution(Norm(mean=-3, sd=1), 
                                  Norm(mean=4, sd=2),
                                  mixCoeff=c(0.2, 0.8))
## ... and then a function for sampling random variates from it
rmyMix <- r(myMix)

## Sample a million random variates, and plot (part of) their histogram
x <- rmyMix(1000)
hist(x,100)


myMix2 <- UnivarMixingDistribution(Norm(mean=-2, sd=1), 
                                  Norm(mean=5, sd=2),
                                  mixCoeff=c(0.8, 0.2))
## ... and then a function for sampling random variates from it
rmyMix <- r(myMix2)

## Sample a million random variates, and plot (part of) their histogram
y <- rmyMix(1000)

DF <- data.frame(x=x,y=y)

for(i in 1:1000)
{
  
}

mix_mod_x <- normalmixEM(x, k = 2)
mix_mod_y <- normalmixEM(y, k = 2)

ret<-as.data.frame(cbind(mix_mod_x$x,mix_mod_x$posterior))


