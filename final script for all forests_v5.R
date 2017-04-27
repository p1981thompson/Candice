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
library(scales)
library(car)

#Load main data file from Excel. Located in R project "Candice" (my H:// drive)
mydat<-paste(getwd(),"Forest plot data for R script4.xlsx",sep='/')

#my.dat<-"C:/Users/pthompson/Dropbox/project A1/Forest Plots/Forest plot data for R script3.xlsx"
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
for(i in 1:length(data[,1]))
{data$d_SE[i]<-ifelse(data$Spread[i]=="SD", data$d_SE[i]/sqrt(data$N[i]), data$d_SE[i])}
data$ci.low<-data$LI-1.96*data$d_SE
data$ci.high<-data$LI+1.96*data$d_SE
data$ci.high<-ifelse(data$ci.high>1,1,data$ci.high)
data$Method<-as.factor(data$Method)
if(h==1){data$Method<-factor(data$Method,levels=c("Voxel count", "t-weighted", "LI ratio", "Voxel magnitude"))}
if(h==2){data$Method<-factor(data$Method,levels=c( "Voxel count", "LI ratio", "Flip method"))}
if(h==3){data$Method<-factor(data$Method,levels=c( "Voxel count", "t-weighted", "Voxel magnitude","Bootstrapping"))}
#data$Region<-factor(data$Region,levels=c("Frontal", "Temporoparietal", "Combined frontal and temporoparietal", "Global"))
#levels=rev(levels(data$Author))
data$Author<-ifelse(is.na(data$ci.low)==TRUE,paste0("*",data$Author,", [N=",data$N,"]"),paste0(data$Author,", [N=",data$N,"]"))
data$Author<-ifelse(is.na(data$sub)==FALSE,paste0("**",data$Author),data$Author)
data$Author<-as.factor(data$Author)
data$Region = as.factor(str_wrap(data$Region, width = 10))
data$Region<-factor(data$Region,levels=c("Frontal", "Temporoparietal", "Combined\nfrontal\nand\ntemporoparietal", "Global"))
data$Task = as.factor(str_wrap(data$Task, width = 10))
if(h==1){data$Task<-factor(data$Task,levels=c("Antonym\ngeneration\n(auditory)", "Antonym\ngeneration\n(visual)", "Verb\ngeneration", "Semantic\nfluency", "Phonemic\nfluency"))}
if(h==3){data$Task<-factor(data$Task,levels=c("Category\nmembership\ndecision\n(auditory)", "Category\nmembership\ndecision\n(visual)", "Semantic\nrelatedness\ndecision\n(visual)", "Text\nreading", "Passive\nspeech\nlistening"))}
#

data$Spread<-recode(data$Spread,"'SD'='C.I.';'SE'='C.I.'")
data$Spread<-as.factor(data$Spread)

data$Spread<-factor(data$Spread,levels= c("C.I.", "Unknown"))

return(data)
}

for(h in 1:length(abi.names))
{assign(abi.names[h],cleandat.FP(h))}



#Function to generate Forest plots. ##########################################################


labeller.FP<-function(j)
{
  temp<-vector(mode="character",length=length(grepl("\\^",get(abi.names[j])$Author)))
  for(i in 1:length(grepl("\\^",get(abi.names[j])$Author)))
  {temp[i]<-gsub(" ", "~", get(abi.names[j])$Author[i])}
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
  if(FPdata==1){shape_vals<-c(21,22,23,24)}
  if(FPdata==2){shape_vals<-c(21,23,25)}
  if(FPdata==3){shape_vals<-c(21,22,24,7)}
  
  p <- ggplot(get(abi.names[FPdata]), aes(y = Author, x = LI,group=group, xmin = ci.low, xmax = ci.high, shape=Method)) +
    guides(colour = guide_legend(nrow = 2), shape=guide_legend(order=1,nrow = 2)) +
    geom_point(aes(color=Baseline,fill=Baseline),position=position_dodgev(height = 1),size=2.5)+
    geom_errorbarh(aes(height = .1),position=position_dodgev(height=1)) +
    #scale_x_continuous(limits=c(round(min(c(get(abi.names[FPdata])$ci.low,get(abi.names[FPdata])$LI),na.rm =T)-0.1,2),round(max(c(get(abi.names[FPdata])$ci.high,get(abi.names[FPdata])$LI),na.rm =T)+0.1,2)),breaks=round(seq(round(min(c(get(abi.names[FPdata])$ci.low,get(abi.names[FPdata])$LI),na.rm =T)-0.1,2),round(max(c(get(abi.names[FPdata])$ci.high,get(abi.names[FPdata])$LI),na.rm =T)+0.1,2),length=6),2))+
    scale_x_continuous(limits=c(-0.8,1))+
    geom_vline(xintercept=0, color="grey60",linetype="dashed")+
    #scale_linetype_manual("Spread",values=c(1,3,2,6),drop=FALSE)+
    #scale_color_manual("Baseline",values=c("gray40","gray70"),drop=FALSE)+
    facet_grid(Region~Task, scales = "free", space = "free") +
    theme_bw() +
    scale_shape_manual(values=shape_vals)+
    theme(strip.text.y = element_text(angle = 0,face="bold"),strip.text.x = element_text(angle = 0,face="bold"),strip.background = element_rect(colour="black", fill="grey"),legend.position="top",legend.title = element_text(face="bold"),legend.text=element_text(size=8),axis.text.y  = element_text(face="bold"),axis.text.x  = element_text(angle=90, vjust=0.5, size=8),axis.title.y=element_blank(),axis.title.x=element_text("Laterality Index (LI)", face="bold"))  
  
  grid.newpage()
  
  if(FPdata==1){footnote <- "* No value for measure of spread given, so Confidence Interval not shown.\n** LI values at different thresholds: z = 5.3 (Top), z = 2.3 (Bottom)"}
  
  if(FPdata==2){footnote <- "* No value for measure of spread given, so Confidence Interval not shown.\n** LI values at different thresholds: t = 5 (Top), t = 4 (Middle), t = 3 (Bottom)"}
  
  if(FPdata==3){footnote <- "* No value for measure of spread given, so Confidence Interval not shown."}
  
  title.grob <- textGrob(
    label = LETTERS[FPdata],
    x = unit(1, "lines"),
    y = unit(0, "lines"),
    hjust = 0, vjust = 0,
    gp = gpar(fontsize = 16))
  
  g <- arrangeGrob(p, top = title.grob, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 8)))
  grid.draw(g)
}

############################################################################
#Addtional functions to allow 'vertical dodge' from: "https://github.com/jaredlander/coefplot/blob/master/R/position.r"

# Detect and prevent collisions.
# Powers dodging, stacking and filling.
collidev <- function(data, height = NULL, name, strategy, check.height = TRUE) {
  # Determine height
  if (!is.null(height)) {
    # height set manually
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y - height / 2
      data$ymax <- data$y + height / 2
    }
  } else {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y
      data$ymax <- data$y
    }
    
    # height determined from data, must be floating point constant
    heights <- unique(data$ymax - data$ymin)
    heights <- heights[!is.na(heights)]
    
    #   # Suppress warning message since it's not reliable
    #     if (!zero_range(range(heights))) {
    #       warning(name, " requires constant height: output may be incorrect",
    #         call. = FALSE)
    #     }
    height <- heights[1]
  }
  
  # Reorder by x position, relying on stable sort to preserve existing
  # ordering, which may be by group or order.
  data <- data[order(data$ymin),]
  
  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
  intervals <- intervals[!is.na(intervals)]
  
  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping y intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }
  
  if (!is.null(data$xmax)) {
    plyr::ddply(data, "ymin", strategy, height = height)
  } else if (!is.null(data$x)) {
    data$xmax <- data$x
    data <- plyr::ddply(data, "ymin", strategy, height = height)
    data$x <- data$xmax
    data
  } else {
    stop("Neither x nor xmax defined")
  }
}

# Stack overlapping intervals.
# Assumes that each set has the same horizontal position
pos_stackv <- function(df, height) {
  if (nrow(df) == 1) return(df)
  
  n <- nrow(df) + 1
  x <- ifelse(is.na(df$x), 0, df$x)
  if (all(is.na(df$y))) {
    heights <- rep(NA, n)
  } else {
    heights <- c(0, cumsum(x))
  }
  
  df$xmin <- heights[-n]
  df$xmax <- heights[-1]
  df$x <- df$xmax
  df
}

# Stack overlapping intervals and set height to 1.
# Assumes that each set has the same horizontal position.
pos_fillv <- function(df, height) {
  stacked <- pos_stackv(df, height)
  stacked$xmin <- stacked$xmin / max(stacked$xmax)
  stacked$xmax <- stacked$xmax / max(stacked$xmax)
  stacked$x <- stacked$xmax
  stacked
}

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_dodgev <- function(df, height) {
  n <- length(unique(df$group))
  if (n == 1) return(df)
  
  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }
  
  d_height <- max(df$ymax - df$ymin)
  
  # df <- data.frame(n = c(2:5, 10, 26), div = c(4, 3, 2.666666,  2.5, 2.2, 2.1))
  # ggplot(df, aes(n, div)) + geom_point()
  
  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidy <- match(df$group, sort(unique(df$group)))
  
  # Find the center for each group, then use that to calculate xmin and xmax
  df$y <- df$y + height * ((groupidy - 0.5) / n - .5)
  df$ymin <- df$y - d_height / n / 2
  df$ymax <- df$y + d_height / n / 2
  
  df
}


#' Adjust position by dodging overlaps to the side.
#'
#' @inheritParams ggplot2::position_identity
#' @param height Dodging height, when different to the height of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples for a use case.
#' @family position adjustments
#' @export
#' @examples
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
#'   geom_bar(position = "dodge")
#' \donttest{
#' ggplot(diamonds, aes(price, fill = cut)) +
#'   geom_histogram(position="dodge")
#' # see ?geom_boxplot and ?geom_bar for more examples
#'
#' # To dodge items with different heights, you need to be explicit
#' df <- data.frame(x=c("a","a","b","b"), y=2:5, g = rep(1:2, 2))
#' p <- ggplot(df, aes(x, y, group = g)) +
#'   geom_bar(
#'     stat = "identity", position = "dodge",
#'     fill = "grey50", colour = "black"
#'   )
#' p
#'
#' # A line range has no height:
#' p + geom_linerange(aes(ymin = y-1, ymax = y+1), position = "dodge")
#' # You need to explicitly specify the height for dodging
#' p + geom_linerange(aes(ymin = y-1, ymax = y+1),
#'   position = position_dodge(width = 0.9))
#'
#' # Similarly with error bars:
#' p + geom_errorbar(aes(ymin = y-1, ymax = y+1), width = 0.2,
#'   position = "dodge")
#' p + geom_errorbar(aes(ymin = y-1, ymax = y+1, height = 0.2),
#'   position = position_dodge(width = 0.90))
#' }
require(proto)
position_dodgev <- function(height = NULL) {
  ggproto(NULL, PositionDodgeV, height = height)
}



PositionDodgeV <- ggproto("PositionDodgeV", Position,
                          required_aes = "y",
                          height = NULL,
                          setup_params = function(self, data) {
                            if (is.null(data$ymin) && is.null(data$ymax) && is.null(self$height)) {
                              warning("height not defined. Set with `position_dodgev(height = ?)`",
                                      call. = FALSE)
                            }
                            list(height = self$height)
                          },
                          
                          compute_panel = function(data, params, scales) {
                            collidev(data, params$height, "position_dodgev", pos_dodgev, check.height = FALSE)
                          }
)


############################################################################



windows(record=T,width = 8, height = 7)
for(k in 1:length(abi.names))
{
  #tiff(file = paste0(abi.names[k],".tiff"), width = 11, height = 8, units = "in", res = 300)
  plot.FP(FPdata=k)
  #dev.off()
}

# for(k in 1:length(abi.names))
# {
#   tiff(file = paste0(abi.names[k],"_v2.tiff"), width = 11, height = 8, units = "in", res = 300)
#   plot.FP(FPdata=k)
#   dev.off()
# }

tiff(file = paste0(abi.names[1],"_v3.tiff"), width = 8, height = 7, units = "in", res = 300)
plot.FP(FPdata=1)
dev.off()

tiff(file = paste0(abi.names[2],"_v3.tiff"), width = 9, height = 8, units = "in", res = 300)
plot.FP(FPdata=2)
dev.off()

tiff(file = paste0(abi.names[3],"_v3.tiff"), width = 8, height = 7, units = "in", res = 300)
plot.FP(FPdata=3)
dev.off()


