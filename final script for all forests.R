################################################################################
#
#
#
################################################################################

#08/03/2017

#load required packages
require("XLConnect")
require(ggplot2)
library(grid)
library(gridExtra)
library(stringr)

#Load main data file from Excel. Located in R project "Candice" (my H:// drive)
mydat<-paste(getwd(),"Forest plot data for R script.xlsx",sep='/')
abbie.data <- loadWorkbook(mydat, create = FALSE)

#Load individual sheets

abi.names<-paste0(gsub('([[:punct:]])|\\s+','_',getSheets(abbie.data)),"_data")

for(i in 1:length(getSheets(abbie.data)))
{
  assign(abi.names[i],readWorksheet(abbie.data,getSheets(abbie.data)[i]))
}

#############################################################################
#Clean up sheets and recode some information.
#############################################################################


cleandat.FP<-function(h)
{data<-get(abi.names[h])
data$ci.low<-data$LI-data$d_SE
data$ci.high<-data$LI+data$d_SE
levels(data$Region)<-c("Frontal", "Temporoparietal", "Combined frontal and temporoparietal", "Global")
levels(data)<-rev(levels(data$Author))
data$Author<-as.factor(ifelse(is.na(data$ci.low)==TRUE,paste0("*",data$Author,", [N=",data$N,"]"),paste0(data$Author,", [N=",data$N,"]")))
data$Region = str_wrap(data$Region, width = 10)
data$Task = str_wrap(data$Task, width = 10)
return(data)
}

for(h in 1:length(abi.names))
{
  assign(abi.names[h],cleandat.FP(h))
}



#Function to generate Forest plots. ##########################################################


labeller.FP<-function(j)
{
  temp<-vector(mode="character",length=length(grepl("\\^",get(abi.names[j])$Author)))
  for(i in 1:length(grepl("\\^",get(abi.names[j])$Author)))
  {
    temp[i]<-gsub(" ", "~", get(abi.names[j])$Author[i])
  }
  return(temp)
}

###
# labeller.FP<-function(j)
# {
#   temp<-vector(mode="character",length=length(grepl("\\^",get(abi.names[j])$Author)))
#   for(i in 1:length(grepl("\\^",get(abi.names[j])$Author)))
#   {
#     temp[i]<-gsub(" ", "~", get(abi.names[j])$Author[i])
#     
#   }
#   temp2<-noquote(paste("c(expression(",paste0(temp,collapse=","),"))",sep=""))
#   
#   return(temp2)
# }
###

plot.FP <- function(FPdata)
{
  
  p <- ggplot(get(abi.names[FPdata]), aes(y = Author, x = LI,group=group, xmin = ci.low, xmax = ci.high, shape=Method)) +
    geom_point(aes(color=Baseline,fill=Baseline),position=position_dodgev(height = 1),size=3)+
    geom_errorbarh(aes(height = .1, linetype=Spread),position=position_dodgev(height=1)) +
    scale_x_continuous(limits=c(round(min(c(get(abi.names[FPdata])$ci.low,get(abi.names[FPdata])$LI),na.rm =T)-0.1,2),round(max(c(get(abi.names[FPdata])$ci.high,get(abi.names[FPdata])$LI),na.rm =T)+0.1,2)),breaks=round(seq(round(min(c(get(abi.names[FPdata])$ci.low,get(abi.names[FPdata])$LI),na.rm =T)-0.1,2),round(max(c(get(abi.names[FPdata])$ci.high,get(abi.names[FPdata])$LI),na.rm =T)+0.1,2),length=10),2))+
    geom_vline(xintercept=0, color="grey60",linetype="dashed")+
    facet_grid(Region~Task, scales = "free", space = "free") +
    theme_bw() +
    theme(strip.text.y = element_text(angle = 0,face="bold"),strip.text.x = element_text(angle = 0,face="bold"),strip.background = element_rect(colour="black", fill="grey"),legend.position="top",legend.title = element_text(face="bold"),legend.text=element_text(size=8),axis.text.y  = element_text(face="bold"),axis.text.x  = element_text(angle=90, vjust=0.5, size=8))+
    scale_shape_manual(values=c(21:25,7)) #+ scale_y_discrete("Author", labels = parse(text=labeller.FP(j=FPdata)))
  grid.newpage()
  footnote <- "* No value for measure of spread given (SD, SE)."
  g <- arrangeGrob(p, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 8)))
  grid.draw(g)
}

#windows(record=T,width = 16, height = 12)
for(k in 1:length(abi.names))
{
  tiff(file = paste0(abi.names[k],".tiff"), width = 8, height = 8, units = "in", res = 300)
  plot.FP(FPdata=k)
  dev.off()
}


