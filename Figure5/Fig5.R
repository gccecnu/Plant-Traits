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
library(stats)


# stderr <- function(x) sqrt(var(x,na.rm=T)/length(which(!is.na(x))))
normalize <- function(x) (x-min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))

fit_gaussian_curve <- function(x, y, smooth_points = 200) {
  # ------------------------------
  # 功能：对 x-y 数据进行高斯（正态分布型）拟合，并输出平滑预测结果
  # 参数：
  #   x, y : 数值向量
  #   smooth_points : 输出曲线的平滑点数（默认 200）
  # 返回：
  #   data.frame(x_smooth, y_pred, model)
  # ------------------------------
  
  # 检查输入有效性
  if (length(x) != length(y)) stop("x 和 y 长度不一致。")
  if (any(is.na(x)) || any(is.na(y))) {
    message("警告：数据中存在 NA，将自动移除。")
    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]
  }
  
  # 构建数据框
  df <- data.frame(x = x, y = y)
  
  # 初始参数估计
  start_list <- list(
    a = max(y, na.rm = TRUE),
    b = mean(x, na.rm = TRUE),
    c = sd(x, na.rm = TRUE)
  )
  
  # 尝试非线性最小二乘拟合
  fit <- tryCatch({
    nls(y ~ a * exp(-((x - b)^2) / (2 * c^2)),
        data = df,
        start = start_list,
        control = list(maxiter = 500, warnOnly = TRUE))
  }, error = function(e) {
    stop("⚠️ 拟合失败，请检查数据分布或提供更合适的初始参数。")
  })
  
  # 生成平滑预测点
  x_smooth <- seq(min(x), max(x), length.out = smooth_points)
  y_pred <- predict(fit, newdata = data.frame(x = x_smooth))
  
  # 输出结果
  result <- data.frame(
    x_smooth = x_smooth,
    y_pred = y_pred
  )
  
  attr(result, "model") <- fit   # 把模型对象也附带返回
  return(result)
}

# Figure settings
pic_height = 8
pic_width = 11

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
SuspendSediment = SuspendSediment*10 # g/L

# Net sediment, collected by plate with diameter of 24cm
PlateProfile = (24/2)^2*pi # Area of plate
NetSediment = rowMeans(SediData[,12:14]/PlateProfile, na.rm = T)  # g/cm^2

# Load sediment, collected by tube with 10cm height, 10cm (W) or 10cm (G) diameter
SediData$TubeType[SediData$TubeType == 'W'] = 10 # 10cm diameter
SediData$TubeType[SediData$TubeType == 'G'] = 11 # 1cm diameter
SediData$TubeType.1[SediData$TubeType.1 == 'W'] = 10 # 10cm diameter
SediData$TubeType.1[SediData$TubeType.1 == 'G'] = 11 # 1cm diameter
# TubeHeight = 10  # Tube height is 10cm
# TubeVolume1 = (as.numeric(SediData$TubeType)/2)^2*pi*TubeHeight # Volume of Tube
# TubeVolume2 = (as.numeric(SediData$TubeType.1)/2)^2*pi*TubeHeight
TubeSquare1 = (as.numeric(SediData$TubeType)/2)^2*pi # Square of Tube
TubeSquare2 = (as.numeric(SediData$TubeType.1)/2)^2*pi

temp_load = cbind(SediData[,15]/TubeSquare1, SediData[,17]/TubeSquare2)
TotalSediment = rowMeans(temp_load, na.rm = T)  # g/cm^2

# Retention rate
RetentionRate = NetSediment/TotalSediment 

# SediData reshape
data1 = data.frame(Shoots, SuspendSediment, NetSediment, TotalSediment, RetentionRate)
SediData = cbind(SediData, data1)
data1 = data1[-which(data1$NetSediment == max(data1$NetSediment, na.rm = TRUE)), ]

# Plot
data1 = data1[-which(data1$Shoots == 0),]

x_breaks = seq(300, 2100, by = 600) 
x_labels = seq(300, 2100, by = 600) 
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

# plot A
x = data1$Shoots
y = data1$SuspendSediment
pred = fit_gaussian_curve(x,y, smooth_points = 200)

pp1 <- ggplot(data1, aes(x = Shoots)) +
  geom_point(aes(y = SuspendSediment), colour = cbPalette[1], size = 3, shape = 15, alpha = alphadegree) +   # 蓝色散点图和图例标签
  geom_line(data = pred, aes(x = x_smooth, y = y_pred), color = cbPalette[1], size = 1.2, alpha = alphadegree) +
  # geom_smooth(aes(y = SuspendSediment), colour = cbPalette[1], method = "gam", se = FALSE) +  # 蓝色拟合线
  xlab("") +
  ylab(bquote(atop("Suspended sediment", "concentration (g/L)"))) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  # scale_y_continuous(breaks = c(0, .5, 1.0, 1.5), labels=c("0.0", "0.5", "1.0", "1.5"), limits=c(0, 1.5)) +
  # annotate("text", x = 390, y = 1.35, label = bquote(" " %*% "10"^"-2"), size = 6, family = "Times New Roman Itali") +
  mytheme +
  hide_x_axis
pp1

x = data1$Shoots
y = data1$NetSediment
pred = fit_gaussian_curve(x,y, smooth_points = 200)

pp2 = ggplot(data1, aes(x = Shoots)) +
  geom_point(aes(y = NetSediment), colour = cbPalette[2], size = 3, shape = 16, alpha = alphadegree) +   # 红色散点图和图例标签
  geom_line(data = pred, aes(x = x_smooth, y = y_pred), color = cbPalette[2], size = 1.2, alpha = alphadegree) +
  # geom_smooth(aes(y = NetSediment), colour = cbPalette[4], method = "loess", se = FALSE) +  # 蓝色拟合线
  xlab("") +
  ylab(bquote(atop("Net sediment", "(g/cm"^2*")"))) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = seq(0, 0.05, by = 0.01), labels = c("0.0", "1.0", "2.0", "3.0", "4.0", "5.0"), limits=c(0, 0.05)) +
  annotate("text", x = 390, y = 0.05, label = "x10^{-2}", size = 6,  parse = TRUE) +
  # scale_x_log10() +
  # scale_y_log10() +
  mytheme +
  hide_x_axis
pp2

# x = data1$Shoots
# y = data1$TotalSediment
# pred = fit_gaussian_curve(x,y, smooth_points = 200)

pp3 = ggplot(data1, aes(x = Shoots)) +
  geom_point(aes(y = TotalSediment), colour = cbPalette[3], size = 3, shape = 17, alpha = alphadegree) +   # 蓝色散点图和图例标签
  # geom_line(data = pred, aes(x = x_smooth, y = y_pred), color = cbPalette[3], size = 1.2, alpha = alphadegree) +
  geom_smooth(aes(y = TotalSediment), colour = cbPalette[3], method = "lm", se = FALSE, alpha = alphadegree) +  # 蓝色拟合线
  xlab("") +
  ylab(bquote(atop("Gross sediment", "(g/cm"^2*")"))) + 
  xlab(n ~''~ (shoots/m^2)) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, 0.8), labels=c("0.0", "2.0", "4.0", "6.0", "8.0")) +
  annotate("text", x = 390, y = .75, label = "x10^{-1}", size = 6, parse = TRUE) +
  # scale_x_log10() +
  # scale_y_log10() +
  mytheme 
pp3

x = data1$Shoots
y = data1$RetentionRate
pred = fit_gaussian_curve(x,y, smooth_points = 200)

pp4 = ggplot(data1, aes(x = Shoots)) +
  geom_point(aes(y = RetentionRate), colour = cbPalette[4], size = 3, shape = 17, alpha = alphadegree) +   # 蓝色散点图和图例标签
  geom_line(data = pred, aes(x = x_smooth, y = y_pred), color = cbPalette[4], size = 1.2, alpha = alphadegree) +
  # xlab(expression(atop("Plant density (Shoots/m"^2*")"))) +
  xlab(n ~''~ (shoots/m^2)) +
  ylab(expression(atop("Retention rate", " "))) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = x_limits) +
  # scale_y_continuous(breaks = c(0, .2, .4, .6), labels=c("0.0", "2.0", "4.0", "6.0"), limits=c(0, .6)) +
  # annotate("text", x = 390, y = 0.55, label = bquote(" " %*% "10"^"-1"), size = 6, family = "Times New Roman Itali") +
  theme(
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

p <- ggarrange(pp1, pp2, pp3, pp4, ncol = 2, nrow = 2,
               labels = c("A","B","C","D"),
               font.label = list(size = FS+2, face = "bold"), # 设置标签字体样式
               widths = c(1,1),
               heights = c(1, 1.2))  # 根据需要调整高度
print(p)
ggsave(p, file = "Fig5_V4.pdf", height = 8, width = 10)

