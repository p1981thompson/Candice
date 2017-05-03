### Figure 2 reproduction

#04-04-2017

#install.packages(c("ggplot2","grid","gridExtra","fGarch"))

library(ggplot2)
library(grid)
library(gridExtra)
library(fGarch)

##############################################################################################
# FIGURE 2
##############################################################################################

thres <- runif(100, 0, 7)

data.sim<-data.frame(LI=jitter(plogis(-4 + 1.5*thres),250),thres=thres)

ggplot(data.sim,aes(x=thres,y=LI))+geom_smooth(span=0.05, se = FALSE,color="black",size=1.5)+ylab("Laterality Index (LI)") +xlab("t-value threshold (t)")+theme_bw(base_size = 16)+scale_y_continuous(breaks=seq(0, 1, 0.1))+scale_x_continuous(breaks=seq(0, 7, 1))


######################################################################################################
# FIGURE 3
##############################################################################################

###Uncomment the section below if you ant to redigitize, otherwise load the saved data. May need to adjust path for your operating system and name.

# require(digitize)
# 
# cal_new=ReadAndCal("C:\\Users\\pthompson\\Dropbox\\project A1\\Figures Methods review\\Figure_3.jpg") #read in relevant png or jpg
# #then click on 2 pts on x axis then 2 pts on y axis to calibrate
# # after 4 clicks, you see blue crosses corresponding to x and y axis ends
# 
# # cal contains coords for these points
# 
# points1=DigitData(col='red') # click on data points - will see blue circle for each one
# points2=DigitData(col='green')
# 
# 
# point_cal=Calibrate(points1,cal_new,0,16,-0.1,1)
# 
# point_cal2=Calibrate(points2,cal_new,0,16,-0.1,1)
# 
# point_cal$x<-0:16
# point_cal2$x<-0:15
# 
# final.points<-rbind(point_cal,point_cal2)
# 
# final.points$group1<-as.factor(c(rep(1,17),rep(2,16)))
# 
# 
# write.csv(final.points,"C:\\Users\\pthompson\\Dropbox\\project A1\\Figures Methods review\\plot repros in R\\figure3_data.csv")

final.points<-read.csv("C:\\Users\\pthompson\\Dropbox\\project A1\\Figures Methods review\\plot repros in R\\figure3_data.csv")


final.points$group1<-as.factor(final.points$group1)
ggplot(final.points,aes(x=x,y=y,group=group1,linetype=group1))+geom_point()+geom_line(size=0.75)+labs(y=expression(LI[e]),x=expression(-log(p))) +theme_bw(base_size = 16)+scale_y_continuous(breaks=seq(0, 1, 0.2))+scale_x_continuous(breaks=seq(0, 17, 2))  + theme(legend.position="none") + theme(axis.title.x = element_text(hjust=1),axis.title.y = element_text(hjust=1,angle=0))

##############################################################################################
#FIGURE 4
##############################################################################################


#### plot1 ####
x <- seq(0,20,length=40)
y1 <- jitter(dnorm(x,mean=8, sd=3.5),350)*1000
y2 <- jitter(dnorm(x,mean=5, sd=2.5),500)*1000

data4<-data.frame(x=c(x,x),y=c(y1,y2),group=as.factor(c(rep("LH",40),rep("RH",40))))

p1<-ggplot(data4,aes(y=y,x=x,group=group,linetype=group))+geom_point()+geom_line()+labs(title="IFG Histogram",y="voxel count",x="T-score") + theme_bw(base_size = 12)+scale_y_continuous(breaks=seq(0, 200, 100))+scale_x_continuous(breaks=seq(0, 20, 10))+ theme(plot.title = element_text(hjust = 0.5),legend.title=element_blank(),legend.background = element_rect(colour="black", size=.5),legend.justification=c(1,1), legend.position=c(1,1),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),plot.margin = unit(c(2,2,2,2), "lines"))

#### plot2 ####
x <- seq(0,20,length=40)
y <- x^2


data4B<-data.frame(x=x,y=y)

p2<-ggplot(data4B,aes(y=y,x=x))+geom_point(shape=95,size=3)+geom_line()+labs(title="T-score Weighting Function",y="T-score weight",x="T-score") + theme_bw(base_size = 12)+scale_y_continuous(breaks=seq(0, 400, 200))+scale_x_continuous(breaks=seq(0, 20, 10))+theme(plot.title = element_text(hjust = 0.5),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),plot.margin = unit(c(2,2,2,2), "lines"))


#### plot3 ####
x <- seq(0,20,length=40)
y1 <- jitter(dnorm(x,mean=10, sd=3.5),50)*16000*8
y2 <- jitter(dnorm(x,mean=7, sd=2.5),50)*6000*8

y2[6:25]<-jitter(y2[6:25],amount=1000)

data4C<-data.frame(x=c(x,x),y=c(y1,y2),group=as.factor(c(rep("LH",40),rep("RH",40))))

p3<-ggplot(data4C,aes(y=y,x=x,group=group,linetype=group,shape=group))+geom_point()+geom_smooth(span=0.1, se = FALSE,color="black")+labs(title="Weighted IFG Distribution",y="weighted voxel count",x="T-score") + theme_bw(base_size = 12)+scale_y_continuous(breaks=seq(0, 16000, 8000))+scale_x_continuous(breaks=seq(0, 20, 10))+ theme(plot.title = element_text(hjust = 0.5),legend.title=element_blank(),legend.background = element_rect(colour="black", size=.5),legend.justification=c(1,1), legend.position=c(1,1),axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),panel.margin = unit(c(2,2,2,2), "lines"))

###Arrange plots 


grid.arrange(arrangeGrob(p1,p2,p3,ncol=1))

grid.text("X", x = unit(0.55, "npc"), 
          y = unit(.66, "npc"),gp = gpar(fontsize=20, vjust=1))
grid.text("=", x = unit(0.55, "npc"), 
          y = unit(.35, "npc"),gp = gpar(fontsize=30, vjust=1))


##############################################################################################
#FIGURE 5
##############################################################################################

data5<- rsnorm(n = 4000,mean=0.7,sd=0.2,xi=-3)
for(i in 1:4000){data5[i]<-ifelse(data5[i]>1,1,data5[i])}
data5<-as.data.frame(data5)

ggplot(data5, aes(data5)) + annotate("rect",xmin=0.6, xmax=0.87,  ymin=-Inf, ymax=Inf, fill="black", alpha=0.2)+ annotate("text", x = -.65, y = 92, label = "Mean:         .63+/- .29 \n Trimmed Mean: .62+/- .11") +annotate("rect",xmin=-1, xmax=-0.25,  ymin=85, ymax=100, color="black",alpha=0)+geom_histogram(binwidth=0.01, colour="black", fill="darkgray")+theme_bw()+labs(x="lateralization Index",y="Frequency of occurrence")+ xlim(c(-1,1))+scale_y_continuous(breaks=seq(0, 120, 20))


##############################################################################################
#FIGURE 7
##############################################################################################

x <- seq(0, 5, length.out=20)
dat <- data.frame(x=x, px=dexp(x, rate=0.5),px.u=dexp(x, rate=0.5)+dexp(x, rate=0.5)*0.4,px.l=dexp(x, rate=0.5)-dexp(x, rate=0.5)*0.4,px4=jitter(dat$px-dat$px*0.6,amount=0.007))
dat$px4<-jitter(dat$px4,0.1)

ggplot(dat, aes(x=x, y=px)) + geom_line()+labs(y="Laterality Index (LI)", x="Active voxel Threshold (N)")+ theme_bw(base_size = 12)+scale_y_continuous(breaks=seq(0, 1, 0.1))+scale_x_continuous(breaks=seq(0, 5, 1))+geom_ribbon(aes(x = x, ymin = px.l, ymax = px.u),fill = "grey", alpha = 0.4)+ylim(c(0,1))+ geom_smooth(aes(x=x, y=px4),span=0.05)


