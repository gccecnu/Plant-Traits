graphics.off()
remove(list=ls()) # Remove all variables from memory
library(dplyr)
library(tidyr)
library(vegan)  # Alpha diversity
library(sciplot)
library(multcompView)
require(ggplot2)
library(plyr)
library(ggpubr)
library("car")
library(RColorBrewer)
library("smatr")

# stderr <- function(x) sqrt(var(x,na.rm=T)/length(which(!is.na(x))))
# normalize <- function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
pic_height = 8
pic_width = 11

# OpenWindow = function (WinWidth,WinHeight) {
#   if (Sys.info()["sysname"]=="Darwin"){ # (Darwin stands for a Mac computer)
#     quartz(width=WinWidth, height=WinHeight,
#            title=" ")
#   } else
#     windows(width = WinWidth, height = WinHeight,
#             title=" ")
# }
# 
# SaveFigure=function(FileName){
#   if (Sys.info()["sysname"]=="Darwin"){ # (Darwin stands for a Mac computer)
#     quartz.save(paste(FileName,'.pdf',sep=''),type = c("pdf"),device = dev.cur())
#   } else
#     savePlot(filename = FileName,type = c("pdf"),device = dev.cur(),restoreConsole = TRUE)
# }
# WinWidth = 20
# WinHeight = 8

# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
#                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette = c("#F8766D",  "#619CFF", "#00BA38", "#F564E3","#B79F00", "#00BFC4")

shape_values = c(15:20)
alphadegree = 0.5
FS = 16

path=getwd()
setwd(path)

# Load data
PlantData = read.csv("../Data/PlantData/TraitsData.csv")
SediData = read.csv("../Data/SedimentData/SedimentData.csv")
CreekNum = substr(SediData$Num,1,1)
site = substr(SediData$Num,6,6)
SediData = cbind(SediData,CreekNum, site)
SP = unique(PlantData$Species)

###### Density-Sediment #####
# Suspend sediment, collected by bottle
Shoots = rowSums(SediData[,19:24], na.rm = T)
SuspendSediment = rowMeans(SediData[,9:11], na.rm = T)  #Suspend sediment g/100ml
SuspendSediment = SuspendSediment/10 # g/L

# Net sediment, collected by plate with diameter of 24cm
PlateProfile = (24/2)^2*pi # Area of plate
NetSediment = rowMeans(SediData[,12:14]/PlateProfile, na.rm = T)  # g/cm^2

# Load sediment, collected by tube with 10cm height, 10cm (W) or 10cm (G) diameter
SediData$TubeType[SediData$TubeType == 'W'] = 10 # 10cm diameter
SediData$TubeType[SediData$TubeType == 'G'] = 11 # 1cm diameter
SediData$TubeType.1[SediData$TubeType.1 == 'W'] = 10 # 10cm diameter
SediData$TubeType.1[SediData$TubeType.1 == 'G'] = 11 # 1cm diameter
TubeHeight = 10  # Tube height is 10cm
TubeVolume1 = (as.numeric(SediData$TubeType)/2)^2*pi*TubeHeight # Volume of Tube
TubeVolume2 = (as.numeric(SediData$TubeType.1)/2)^2*pi*TubeHeight
temp_load = cbind(SediData[,15]/TubeVolume1, SediData[,17]/TubeVolume2)
TotalSediment = rowMeans(temp_load, na.rm = T)  # g/cm^2

# Retention rate
RetentionRate = NetSediment/TotalSediment # cm^-1

# SediData reshape
data = data.frame(Shoots, SuspendSediment, NetSediment, TotalSediment, RetentionRate)
SediData = cbind(SediData, data)

# Plot
data = data[-which(data$Shoots == 0),]

x_breaks = seq(300, 2100, by = 600)  # 根据你的数据调整
x_labels = seq(300, 2100, by = 600)  # 标签也根据需要调整
x_limits = c(300, 2100)

mytheme = theme(
  # legend.position = c(0.25, 0.8),       # 将图例放在右上角内侧
  # legend.title = element_text(size = FS),    # 设置图例标题字体大小
  # legend.text = element_text(size = FS-2),
  panel.background = element_blank(),
  axis.line = element_line(size = 1),    # 修改x轴和y轴的线条粗细
  axis.ticks = element_line(size = 1),   # 修改刻度线的粗细
  axis.ticks.length = unit(0.25, "cm"),
  axis.title.x = element_text(size = FS,vjust = 0.5),
  axis.title.y = element_text(size = FS,vjust = 0.5),
  axis.text.x = element_text(size = FS,color='black'),
  axis.text.y = element_text(size = FS,color='black'),
  strip.text.x = element_text(size = FS,face="bold"),
  strip.text.y = element_text(size = FS,face="bold"),
  plot.margin = margin(0, 10, 0, 0)  # 上、右、下、左
)

hide_x_axis = theme(axis.text.x = element_blank())

pp1 <- ggplot(data, aes(x = Shoots)) +
  geom_point(aes(y = SuspendSediment), colour = cbPalette[1], size = 3, shape = 15, alpha = alphadegree) +   # 蓝色散点图和图例标签
  geom_smooth(aes(y = SuspendSediment), colour = cbPalette[1], method = "loess", se = FALSE) +  # 蓝色拟合线
  xlab("") +
  ylab(bquote(atop("Suspended sediment", "concentration (g/L)"))) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, .005, .01, .015), labels=c("0.0", "0.5", "1.0", "1.5"), limits=c(0, 0.015)) +
  annotate("text", x = 390, y = 0.0135, label = bquote(" " %*% "10"^"-2"), size = 6) +
  # scale_x_log10() +
  # scale_y_log10() +
  mytheme +
  hide_x_axis
pp1

pp2 = ggplot(data, aes(x = Shoots)) +
  geom_point(aes(y = NetSediment), colour = cbPalette[2], size = 3, shape = 16, alpha = alphadegree) +   # 红色散点图和图例标签
  geom_smooth(aes(y = NetSediment), colour = cbPalette[2], method = "loess", se = FALSE) +  # 红色拟合线
  xlab("") +
  ylab(bquote(atop("Net sediment", "(g/cm"^2*")"))) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, .05, .1, .15), labels=c("0.0", "0.5", "1.0", "1.5"), limits=c(0, 0.15)) +
  annotate("text", x = 390, y = 0.135, label = bquote(" " %*% "10"^"-1"), size = 6) +
  # scale_x_log10() +
  # scale_y_log10() +
  mytheme +
  hide_x_axis
pp2

pp3 = ggplot(data, aes(x = Shoots)) +
  geom_point(aes(y = TotalSediment), colour = cbPalette[3], size = 3, shape = 17, alpha = alphadegree) +   # 蓝色散点图和图例标签
  geom_smooth(aes(y = TotalSediment), colour = cbPalette[3], method = "loess", se = FALSE) +  # 蓝色拟合线
  xlab("") +
  ylab(bquote(atop("Total sediment", "(g/cm"^3*")"))) + 
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, 0.08), labels=c("0.0", "2.0", "4.0", "6.0", "8.0")) +
  annotate("text", x = 390, y = .075, label = bquote(" " %*% "10"^"-2"), size = 6) +
  # scale_x_log10() +
  # scale_y_log10() +
  mytheme +
  hide_x_axis
pp3

pp4 = ggplot(data, aes(x = Shoots)) +
  geom_point(aes(y = RetentionRate), colour = cbPalette[4], size = 3, shape = 17, alpha = alphadegree) +   # 蓝色散点图和图例标签
  geom_smooth(aes(y = RetentionRate), colour = cbPalette[4], method = "loess", se = FALSE) +  # 蓝色拟合线
  xlab(n ~''~ (shoots/m^2)) +
  ylab(expression(atop("Retention rate", "(cm)"))) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, 2, 4, 6), labels=c("0.0", "2.0", "4.0", "6.0"), limits=c(0, 6)) +
  # annotate("text", x = 390, y = 0.45, label = bquote(" " %*% "10"^"-1"), size = 6) +
  # scale_x_log10() +
  # scale_y_log10() +
  theme(
    # legend.position = c(0.25, 0.8),       # 将图例放在右上角内侧
    # legend.title = element_text(size = FS),    # 设置图例标题字体大小
    # legend.text = element_text(size = FS-2),
    panel.background = element_blank(),
    axis.line = element_line(size = 1),    # 修改x轴和y轴的线条粗细
    axis.ticks = element_line(size = 1),   # 修改刻度线的粗细
    axis.ticks.length = unit(0.25, "cm"),
    # axis.text.x = element_blank(),
    axis.title.x = element_text(size = FS,vjust = 0.5),
    axis.title.y = element_text(size = FS,vjust = 0.5),
    axis.text.x = element_text(size = FS,color='black'),
    axis.text.y = element_text(size = FS,color='black'),
    strip.text.x = element_text(size = FS,face="bold"),
    strip.text.y = element_text(size = FS,face="bold"),
    plot.margin = margin(0, 10, 0, 0)  # 上、右、下、左
  )
pp4


###### FrontalArea-Sediment ######
d = 0
d = d[-1] # save allometry data for each plot
for (a in 1:length(unique(PlantData$Creek))){
  for (j in 1:length(unique(PlantData$Area))){
    for (i in 1:length(unique(PlantData$Site))){
      SP_temp = PlantData %>%
        filter(PlantData$Creek == unique(PlantData$Creek)[a])
      SP_temp = SP_temp %>%
        filter(SP_temp$Area == unique(PlantData$Area)[j])
      SP_temp = SP_temp %>%
        filter(SP_temp$Site == i)
      
      Sedi_temp = SediData %>%
          filter(SediData$CreekNum == a)
      Sedi_temp = Sedi_temp %>%
        filter(Sedi_temp$Transect == j+1)
      Sedi_temp = Sedi_temp %>%
        filter(Sedi_temp$site == i)

      SuspendSedi = Sedi_temp$SuspendSediment # g/L
      if (length(SuspendSedi) == 0) { SuspendSedi <- NaN  }
      NetSedi = Sedi_temp$NetSediment # g/cm^2
      if (length(NetSedi) == 0) { NetSedi = NaN }
      TotalSedi = Sedi_temp$TotalSediment  # g/cm^2
      if (length(TotalSedi) == 0) { TotalSedi = NaN }
      RetentionRate = Sedi_temp$RetentionRate
      if (length(RetentionRate) == 0) { RetentionRate = NaN }
  
      b = 0
      b = b[-1]
      for (k in 1:length(SP)){
        temp = SP_temp %>%
          filter(SP_temp$Species == SP[k])
        Height_mean = mean(temp$Height, na.rm = T)/100 # 单位换算为米
        Diameter_mean = mean(temp$Diameter, na.rm = T)/100 # 单位换算为米
  
        SD_col = which(colnames(Sedi_temp) == SP[k])
        Density_mean = mean(as.numeric(Sedi_temp[,SD_col]), na.rm = T) # shoots per m^2
  
        # 这里是物种k的迎水面积
        Frontal_area = Height_mean*Diameter_mean*Density_mean/1  # Divide 1 m^3
  
        # 样地[i, j]内所有物种的迎水面积
        b = cbind(b,Frontal_area)
      }
      FrontalArea_mean = mean(b, na.rm = T)
      
      c = cbind(FrontalArea_mean, SuspendSedi, NetSedi, TotalSedi, RetentionRate, a, j+1 ,i)
      d = rbind(d, c)
    }
  }
}
FrontalArea_SediData = d
colnames(FrontalArea_SediData) = c("FrontalArea", "SuspendSediment", "NetSediment", 
                                 "TotalSediment", "RetentionRate", "CreekNum",
                                 "Area", "Site")
data = data.frame(FrontalArea_SediData)

# Plot
x_breaks = seq(5, 25, by = 5)  # 根据你的数据调整
x_labels = seq(5, 25, by = 5)  # 标签也根据需要调整
x_limits = c(4, 25)

pf1 <- ggplot(data, aes(x = FrontalArea)) +
  geom_point(aes(y = SuspendSediment), colour = cbPalette[1], size = 3, shape = 15, alpha = alphadegree) +   # 蓝色散点图和图例标签
  geom_smooth(aes(y = SuspendSediment), colour = cbPalette[1], method = "loess", se = FALSE) +  # 蓝色拟合线
  xlab("") +
  ylab(bquote(atop("Suspended sediment", "concentration (g/L)"))) +
  # scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, .005, .010, .015), labels=c("0.0", "0.5", "1.0", "1.5"), limits = c(0, .015)) +
  annotate("text", x = 5, y = 0.0125, label = bquote(" " %*% "10"^"-2"), size = 6) +
  # scale_x_log10() +
  # scale_y_log10() +
  mytheme +
  hide_x_axis
pf1

pf2 = ggplot(data, aes(x = FrontalArea)) +
  geom_point(aes(y = NetSediment), colour = cbPalette[2], size = 3, shape = 16, alpha = alphadegree) +   # 红色散点图和图例标签
  geom_smooth(aes(y = NetSediment), colour = cbPalette[2], method = "loess", se = FALSE) +  # 红色拟合线
  xlab("") +
  ylab(bquote(atop("Net sediment", "(g/cm"^2*")"))) +
  # scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, .05, .1, .15), labels=c("0.0", "0.5", "1.0", "1.5"), limits=c(0, 0.15)) +
  annotate("text", x = 5, y = 0.135, label = bquote(" " %*% "10"^"-1"), size = 6) +
  # scale_x_log10() +
  # scale_y_log10() +
  mytheme +
  hide_x_axis
pf2

pf3 = ggplot(data, aes(x = FrontalArea)) +
  geom_point(aes(y = TotalSediment), colour = cbPalette[3], size = 3, shape = 17, alpha = alphadegree) +   # 蓝色散点图和图例标签
  geom_smooth(aes(y = TotalSediment), colour = cbPalette[3], method = "loess", se = FALSE) +  # 蓝色拟合线
  xlab("") +
  ylab(bquote(atop("Total sediment", "(g/cm"^3*")"))) +
  # scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, 0.08), labels=c("0.0", "2.0", "4.0", "6.0", "8.0")) +
  annotate("text", x = 5, y = .075, label = bquote(" " %*% "10"^"-2"), size = 6) +
  # scale_x_log10() +
  # scale_y_log10() +
  mytheme +
  hide_x_axis
pf3

pf4 = ggplot(data, aes(x = FrontalArea)) +
  geom_point(aes(y = RetentionRate), colour = cbPalette[4], size = 3, shape = 17, alpha = alphadegree) +   # 蓝色散点图和图例标签
  geom_smooth(aes(y = RetentionRate), colour = cbPalette[4], method = "loess", se = FALSE) +  # 蓝色拟合线
  xlab(Frontal ~''~ area ~''~ (m^-1)) +
  ylab(expression(atop("Retention rate", "(cm)"))) +
  # scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, 2, 4, 6), labels=c("0.0", "2.0", "4.0", "6.0"), limits=c(0, 6)) +
  # annotate("text", x = 5, y = 0.45, label = bquote(" " %*% "10"^"-1"), size = 6) +
  # scale_x_log10() +
  # scale_y_log10() +
  theme(
    # legend.position = c(0.25, 0.8),       # 将图例放在右上角内侧
    # legend.title = element_text(size = FS),    # 设置图例标题字体大小
    # legend.text = element_text(size = FS-2),
    panel.background = element_blank(),
    axis.line = element_line(size = 1),    # 修改x轴和y轴的线条粗细
    axis.ticks = element_line(size = 1),   # 修改刻度线的粗细
    axis.ticks.length = unit(0.25, "cm"),
    # axis.text.x = element_blank(),
    axis.title.x = element_text(size = FS,vjust = 0.5),
    axis.title.y = element_text(size = FS,vjust = 0.5),
    axis.text.x = element_text(size = FS,color='black'),
    axis.text.y = element_text(size = FS,color='black'),
    strip.text.x = element_text(size = FS,face="bold"),
    strip.text.y = element_text(size = FS,face="bold"),
    plot.margin = margin(0, 10, 0, 0)  # 上、右、下、左
  )
pf4

p <- ggarrange(pp1, pf1, pp2, pf2, pp3, pf3, pp4, pf4, ncol = 2, nrow = 4,
               labels = c("A","B","C","D","E","F","G","H"),
               font.label = list(size = FS+2, face = "bold"), # 设置标签字体样式
               widths = c(1,1),
               heights = c(1, 1, 1, 1.2))  # 根据需要调整高度
print(p)
ggsave(p, file = "Fig5.pdf", height = 13, width = 13)
