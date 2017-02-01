#Original Script from: http://stackoverflow.com/questions/17370460/scatterplot-with-alpha-transparent-histograms-in-r






library(ggplot2)
library(gridExtra)

set.seed(42)
DF <- data.frame(x=rnorm(100,mean=c(1,5)),y=rlnorm(100,meanlog=c(8,6)),group=1:2)

p0 <- ggplot(DF,aes(x=x,y=y,colour=factor(group))) + geom_point() +
  theme_bw() +
  theme(legend.position="right",plot.margin=unit(c(0,0,0,0),"points"))


p1 <- ggplot(DF,aes(x=x,y=y,colour=factor(group))) + geom_point() +
  theme_bw() +
  theme(legend.position=c(15000,5),plot.margin=unit(c(0,0,0,0),"points"))

theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.margin = unit(0,"null"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.length = unit(0,"null"),
                               axis.ticks.margin = unit(0,"null"),
                               panel.border=element_rect(color=NA),...)

p2 <- ggplot(DF,aes(x=x,colour=factor(group),fill=factor(group))) + 
  geom_density(alpha=0.5) + 
  theme_bw() +
  theme0(plot.margin = unit(c(1,0,0,2.2),"lines")) 

p3 <- ggplot(DF,aes(x=y,colour=factor(group),fill=factor(group))) + 
  geom_density(alpha=0.5) + 
  coord_flip()  + 
  theme_bw() +
  theme0(plot.margin = unit(c(0,1,1.2,0),"lines"))

library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p0)

grid.arrange(arrangeGrob(p2,legend,ncol=2,widths=c(3,1)),
             arrangeGrob(p1,p3,ncol=2,widths=c(3,1)),
             heights=c(1,3))
