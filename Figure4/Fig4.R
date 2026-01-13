graphics.off()
rm(list=ls())
require(ggplot2)
library(plyr)
library(dplyr)
library(ggpubr)
library("car")
library(RColorBrewer)
library(tidyr)
library("smatr")
library(grid)
library(broom)
library(showtext)

path=getwd()
setwd(path)
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
#                "#D55E00", "#CC79A7")
cbPalette = c("#F8766D", "#00BA38", "#619CFF", "#F564E3","#B79F00", "#00BFC4")
textcol = cbPalette
shape_values = c(15:20)
alphadegree = 0.2
FS = 16
pic_height = 4
pic_width = 5

mytheme = theme(
  legend.position = c(0.8, 0.25),       # 将图例放在右上角内侧
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = FS),  # 设置图例字体为斜体
  panel.grid.major=element_line(colour=NA),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(size = 1),    # 修改x轴和y轴的线条粗细
  axis.ticks = element_line(size = 1),   # 修改刻度线的粗细
  axis.ticks.length = unit(0.20, "cm"),
  axis.ticks.x = element_blank(),  # 移除x轴刻度线
  axis.ticks.y = element_blank(),   # 移除y轴刻度线
  axis.title.x = element_text(size = FS,vjust = 0.5),
  axis.title.y = element_text(size = FS,vjust = 0.5),
  axis.text.x = element_text(size = FS,color='black'),
  axis.text.y = element_text(size = FS,color='black'),
  strip.text.x = element_text(size = FS,face="bold"),
  strip.text.y = element_text(size = FS,face="bold")
)

x_breaks = c( 1, 10, 100, 200)
x_labels = c("1", "10", "100", "200")
x_limit = c(.7, 200)
y_breaks = c( 1, 10, 100, 1000, 10000)
y_labels = c( "1", "10", "100", "1000", "10000")
y_limits = c(5, 10000)

# Data load
data = read.csv("../Data/PlantData/HD_total.csv") #读取数据
data$xDistance <- factor(data$xDistance, levels = c("0","15","30"),
                           labels = c("1","5","25"))
data$num = as.numeric(levels(data$xDistance)[data$xDistance])
data$xDistance <- factor(data$xDistance, levels = c("1","5","25"),
                           labels = c("0 m","15 m","30 m"))
data$Height_trans = data$Height * data$num
data$Diameter_trans = data$Diameter * data$num

######## 1.1 ST #########
data_ST = data[data$Species == "ST",]
dat_Plot_ST = data.frame(data_ST$Height_trans, data_ST$Diameter_trans, data_ST$xDistance, data_ST$num)
colnames(dat_Plot_ST) = c("Height_trans", "Diameter_trans", "Dis2Creek", "num")
dat_Plot_ST = na.omit(dat_Plot_ST)

ft1 = sma(Height_trans ~ Diameter_trans * Dis2Creek, log = "xy", shift = TRUE, data = dat_Plot_ST)
a_values <- coef(ft1)[1:3,2] # Slope
b_values <- coef(ft1)[1:3,1] # Elevation
labels <- paste("a =", formatC(a_values, format = "f", digits = 2),
                ", b =", formatC(b_values, format = "f", digits = 2))

# Fit function: H = b*D^a

range_data <- dat_Plot_ST %>%
  group_by(Dis2Creek) %>%
  summarize(min_diam = min(Diameter_trans), max_diam = max(Diameter_trans))

# 绘图
p1 = ggplot(dat_Plot_ST, aes(x = Diameter_trans, y = Height_trans, color = Dis2Creek)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_x_log10(breaks = x_breaks, labels = x_labels, limits = x_limit) +
  scale_y_log10(breaks = y_breaks, labels = y_labels, limits = y_limits) +
  geom_segment(data = data.frame(range_data[1, ]),
               aes(x = range_data$min_diam[1], xend = range_data$max_diam[1],
                   y = 10^(b_values[1] + a_values[1] * log10(range_data$min_diam[1])),
                   yend = 10^(b_values[1] + a_values[1] * log10(range_data$max_diam[1]))),
               color = cbPalette[1], linewidth = 1) +
  geom_segment(data = data.frame(range_data[2, ]),
               aes(x = range_data$min_diam[2], xend = range_data$max_diam[2],
                   y = 10^(b_values[2] + a_values[2] * log10(range_data$min_diam[2])),
                   yend = 10^(b_values[2] + a_values[2] * log10(range_data$max_diam[2]))),
               color = cbPalette[2], linewidth = 1) +
  geom_segment(data = data.frame(range_data[3, ]),
               aes(x = range_data$min_diam[3], xend = range_data$max_diam[3],
                   y = 10^(b_values[3] + a_values[3] * log10(range_data$min_diam[3])) ,
                   yend = 10^(b_values[3] + a_values[3] * log10(range_data$max_diam[3]))),
               color = cbPalette[3], linewidth = 1) +
  annotation_logticks(sides="lb", short = unit(0.20, "cm"), mid = unit(0.25, "cm"),
                      long = unit(0.3, "cm"),size = 0.5)+
  labs(x = 'Diameter (mm)', y = 'Height (mm)') +
  # theme_minimal() +
  annotate("text", x = .7, y = 10000, label = "Scirpus triqueter",
           size = 6, hjust = 0, fontface = "italic") +
  annotate("text", x = c(.7, .7, .7), y = c(5500, 3000, 1600), label = labels,
           color = cbPalette[1:3], size = 6, hjust = 0) +
  mytheme
p1
ggsave(p1, file = "Fig4a.pdf", height = pic_height, width = pic_width)

  ######## 1.2 SM #########
data_SM = data[data$Species == "SM",]
dat_Plot_SM = data.frame(data_SM$Height_trans, data_SM$Diameter_trans, data_SM$xDistance, data_SM$num)
colnames(dat_Plot_SM) = c("Height_trans", "Diameter_trans", "Dis2Creek", "num")
dat_Plot_SM = na.omit(dat_Plot_SM)

# 拟合模型
ft1 <- sma(Height_trans ~ Diameter_trans * Dis2Creek, log = "xy", shift = TRUE, data = dat_Plot_SM)
a_values <- coef(ft1)[1:3,2]  # Slope
b_values <- coef(ft1)[1:3,1]  # intercept
labels <- paste("a =", formatC(a_values, format = "f", digits = 2), 
                ", b =", formatC(b_values, format = "f", digits = 2))

range_data <- dat_Plot_SM %>%
  group_by(Dis2Creek) %>%
  summarize(min_diam = min(Diameter_trans), max_diam = max(Diameter_trans))

# 绘图
p2 = ggplot(dat_Plot_SM, aes(x = Diameter_trans, y = Height_trans, color = Dis2Creek)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_x_log10(breaks = x_breaks, labels = x_labels, limit = x_limit) +
  scale_y_log10(breaks = y_breaks, labels = y_labels, limit = y_limits) +
  geom_segment(data = data.frame(range_data[1, ]),
               aes(x = range_data$min_diam[1], xend = range_data$max_diam[1],
                   y = 10^(b_values[1] + a_values[1] * log10(range_data$min_diam[1])),
                   yend = 10^(b_values[1] + a_values[1] * log10(range_data$max_diam[1]))),
               color = cbPalette[1], linewidth = 1) +
  geom_segment(data = data.frame(range_data[2, ]),
               aes(x = range_data$min_diam[2], xend = range_data$max_diam[2],
                   y = 10^(b_values[2] + a_values[2] * log10(range_data$min_diam[2])),
                   yend = 10^(b_values[2] + a_values[2] * log10(range_data$max_diam[2]))),
               color = cbPalette[2], linewidth = 1) +
  geom_segment(data = data.frame(range_data[3, ]),
               aes(x = range_data$min_diam[3], xend = range_data$max_diam[3],
                   y = 10^(b_values[3] + a_values[3] * log10(range_data$min_diam[3])) ,
                   yend = 10^(b_values[3] + a_values[3] * log10(range_data$max_diam[3]))),
               color = cbPalette[3], linewidth = 1) +
  annotation_logticks(sides="lb", short = unit(0.20, "cm"), mid = unit(0.25, "cm"),
                      long = unit(0.3, "cm"),size = 0.5)+
  labs(x = 'Diameter (mm)', y = 'Height (mm)') +
  # theme_minimal() +
  annotate("text", x = .7, y = 10000, label = "Scirpus mariqueter", size = 6, hjust = 0, fontface = "italic") +
  annotate("text", x = c(.7, .7, .7), y = c(5500, 3000, 1600), label = labels,
           color = cbPalette[1:3], size = 6, hjust = 0) +
  mytheme
p2
ggsave(p2, file = "Fig4b.pdf", height = pic_height, width = pic_width)

######## 1.3 CD #############
data_CD = data[data$Species == "CD",]
dat_Plot_CD = data.frame(data_CD$Height_trans, data_CD$Diameter_trans, data_CD$xDistance, data_CD$num)
colnames(dat_Plot_CD) = c("Height_trans", "Diameter_trans", "Dis2Creek", "num")
dat_Plot_CD = na.omit(dat_Plot_CD)

# 拟合模型
ft1 <- sma(Height_trans ~ Diameter_trans * Dis2Creek, log = "xy", shift = TRUE, data = dat_Plot_CD)
a_values <- coef(ft1)[1:3,2]
b_values <- coef(ft1)[1:3,1]
labels <- paste("a =", formatC(a_values, format = "f", digits = 2), 
                ", b =", formatC(b_values, format = "f", digits = 2))

range_data <- dat_Plot_CD %>%
  group_by(Dis2Creek) %>%
  summarize(min_diam = min(Diameter_trans), max_diam = max(Diameter_trans))

# 绘图
p3 = ggplot(dat_Plot_CD, aes(x = Diameter_trans, y = Height_trans, color = Dis2Creek)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_x_log10(breaks = x_breaks, labels = x_labels, limit = x_limit) +
  scale_y_log10(breaks = y_breaks, labels = y_labels, limit = y_limits) +
  geom_segment(data = data.frame(range_data[1, ]),
               aes(x = range_data$min_diam[1], xend = range_data$max_diam[1],
                   y = 10^(b_values[1] + a_values[1] * log10(range_data$min_diam[1])),
                   yend = 10^(b_values[1] + a_values[1] * log10(range_data$max_diam[1]))),
               color = cbPalette[1], linewidth = 1) +
  geom_segment(data = data.frame(range_data[2, ]),
               aes(x = range_data$min_diam[2], xend = range_data$max_diam[2],
                   y = 10^(b_values[2] + a_values[2] * log10(range_data$min_diam[2])),
                   yend = 10^(b_values[2] + a_values[2] * log10(range_data$max_diam[2]))),
               color = cbPalette[2], linewidth = 1) +
  geom_segment(data = data.frame(range_data[3, ]),
               aes(x = range_data$min_diam[3], xend = range_data$max_diam[3],
                   y = 10^(b_values[3] + a_values[3] * log10(range_data$min_diam[3])) ,
                   yend = 10^(b_values[3] + a_values[3] * log10(range_data$max_diam[3]))),
               color = cbPalette[3], linewidth = 1) +
  annotation_logticks(sides="lb", short = unit(0.20, "cm"), mid = unit(0.25, "cm"),
                      long = unit(0.3, "cm"),size = 0.5)+
  labs(x = 'Diameter (mm)', y = 'Height  (mm)') +
  # theme_minimal() +
  annotate("text", x = .7, y = 10000, label = "Carex scabrifolia", size = 6, hjust = 0, fontface = "italic") +
  annotate("text", x = c(.7, .7, .7), y = c(5500, 3000, 1600), label = labels,
           color = cbPalette[1:3], size = 6, hjust = 0) +
  mytheme
p3
ggsave(p3, file = "Fig4c.pdf", height = pic_height, width = pic_width)

######## 1.4 CS ######
data_CS = data[data$Species == "CS",]
dat_Plot_CS = data.frame(data_CS$Height_trans, data_CS$Diameter_trans, data_CS$xDistance, data_CS$num)
colnames(dat_Plot_CS) = c("Height_trans", "Diameter_trans", "Dis2Creek", "num")
dat_Plot_CS = na.omit(dat_Plot_CS)

# 拟合模型
ft1 <- sma(Height_trans ~ Diameter_trans * Dis2Creek, log = "xy", shift = TRUE, data = dat_Plot_CS)
a_values <- coef(ft1)[1:3,2]
b_values <- coef(ft1)[1:3,1]
labels <- paste("a =", formatC(a_values, format = "f", digits = 2), 
                ", b =", formatC(b_values, format = "f", digits = 2))

range_data <- dat_Plot_CS %>%
  group_by(Dis2Creek) %>%
  summarize(min_diam = min(Diameter_trans), max_diam = max(Diameter_trans))

# 绘图
p4 = ggplot(dat_Plot_CS, aes(x = Diameter_trans, y = Height_trans, color = Dis2Creek)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_x_log10(breaks = x_breaks, labels = x_labels, limit = x_limit) +
  scale_y_log10(breaks = y_breaks, labels = y_labels, limit = y_limits) +
  geom_segment(data = data.frame(range_data[1, ]),
               aes(x = range_data$min_diam[1], xend = range_data$max_diam[1],
                   y = 10^(b_values[1] + a_values[1] * log10(range_data$min_diam[1])),
                   yend = 10^(b_values[1] + a_values[1] * log10(range_data$max_diam[1]))),
               color = cbPalette[1], linewidth = 1) +
  geom_segment(data = data.frame(range_data[2, ]),
               aes(x = range_data$min_diam[2], xend = range_data$max_diam[2],
                   y = 10^(b_values[2] + a_values[2] * log10(range_data$min_diam[2])),
                   yend = 10^(b_values[2] + a_values[2] * log10(range_data$max_diam[2]))),
               color = cbPalette[2], linewidth = 1) +
  geom_segment(data = data.frame(range_data[3, ]),
               aes(x = range_data$min_diam[3], xend = range_data$max_diam[3],
                   y = 10^(b_values[3] + a_values[3] * log10(range_data$min_diam[3])) ,
                   yend = 10^(b_values[3] + a_values[3] * log10(range_data$max_diam[3]))),
               color = cbPalette[3], linewidth = 1) +
  annotation_logticks(sides="lb", short = unit(0.20, "cm"), mid = unit(0.25, "cm"),
                      long = unit(0.3, "cm"),size = 0.5)+
  labs(x = 'Diameter (mm)', y = 'Height (mm)') +
  # theme_minimal() +
  annotate("text", x = .7, y = 10000, label = "Juncellus serotinus", size = 6, hjust = 0, fontface = "italic") +
  annotate("text", x = c(.7, .7, .7), y = c(5500, 3000, 1600), label = labels,
           color = cbPalette[1:3], size = 6, hjust = 0) +
  mytheme
p4
ggsave(p4, file = "Fig4d.pdf", height = pic_height, width = pic_width)

 ########1 .5 IC ########
data_IC = data[data$Species == "IC",]
dat_Plot_IC = data.frame(data_IC$Height_trans, data_IC$Diameter_trans, data_IC$xDistance, data_IC$num)
colnames(dat_Plot_IC) = c("Height_trans", "Diameter_trans", "Dis2Creek", "num")
dat_Plot_IC = na.omit(dat_Plot_IC)
# 拟合模型
ft1 <- sma(Height_trans ~ Diameter_trans * Dis2Creek, log = "xy", shift = TRUE, data = dat_Plot_IC)
a_values <- coef(ft1)[1:2,2]
b_values <- coef(ft1)[1:2,1]
labels <- paste("a =", formatC(a_values, format = "f", digits = 2), 
                ", b =", formatC(b_values, format = "f", digits = 2))

range_data <- dat_Plot_IC %>%
  group_by(Dis2Creek) %>%
  summarize(min_diam = min(Diameter_trans), max_diam = max(Diameter_trans))

# 绘图
p5 = ggplot(dat_Plot_IC, aes(x = Diameter_trans, y = Height_trans, color = Dis2Creek)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = cbPalette[1:2]) +  # 添加此行
  scale_x_log10(breaks = x_breaks, labels = x_labels, limit = x_limit) +
  scale_y_log10(breaks = y_breaks, labels = y_labels, limit = y_limits) +
  geom_segment(data = data.frame(range_data[1, ]),
               aes(x = range_data$min_diam[1], xend = range_data$max_diam[1],
                   y = 10^(b_values[1] + a_values[1] * log10(range_data$min_diam[1])),
                   yend = 10^(b_values[1] + a_values[1] * log10(range_data$max_diam[1]))),
               color = cbPalette[1], linewidth = 1) +
  geom_segment(data = data.frame(range_data[2, ]),
               aes(x = range_data$min_diam[2], xend = range_data$max_diam[2],
                   y = 10^(b_values[2] + a_values[2] * log10(range_data$min_diam[2])),
                   yend = 10^(b_values[2] + a_values[2] * log10(range_data$max_diam[2]))),
               color = cbPalette[2], linewidth = 1) +
  # geom_segment(data = data.frame(range_data[3, ]),
  #              aes(x = range_data$min_diam[3], xend = range_data$max_diam[3],
  #                  y = 10^(b_values[3] + a_values[3] * log10(range_data$min_diam[3])) ,
  #                  yend = 10^(b_values[3] + a_values[3] * log10(range_data$max_diam[3]))),
  #              color = cbPalette[3], linewidth = 1) +
  annotation_logticks(sides="lb", short = unit(0.20, "cm"), mid = unit(0.25, "cm"),
                      long = unit(0.3, "cm"),size = 0.5)+
  labs(x = 'Diameter (mm)', y = 'Height (mm)') +
  # theme_minimal() +
  annotate("text", x = .7, y = 10000, label = "Imperata cylindrica", size = 6, hjust = 0, fontface = "italic") +
  annotate("text", x = c(.7, .7 ), y = c(5500, 3000), label = labels,
           color = cbPalette[1:2], size = 6, hjust = 0) +
  mytheme
p5
ggsave(p5, file = "Fig4e.pdf", height = pic_height, width = pic_width)

######## Fig4. F ########## 
Alldata = read.csv("../Data/PlantData/HD_total.csv") #读取数据
SP = unique(Alldata$Species)

c = 0
c = c[-1]
for (j in unique(Alldata$xDistance)){
  temp = Alldata %>% 
    filter(Alldata$xDistance == j)
  b = 0
  b = b[-1]
  for (i in 1:length(unique(Alldata$Species))){
    SP_temp = temp %>% 
      filter(temp$Species == SP[i])
    if (nrow(SP_temp)==0){
      a = NA
    }else{
      ft1 = sma(Height ~ Diameter*xDistance, log = "xy",shift = T,
                data = SP_temp)
      # summary(ft1)
      a = ft1[["coef"]][[1]][["coef(SMA)"]][2]
    }
    b = cbind(b,a)
  }
  c = rbind(c,b)
  # print(c)
}
SP_dis = data.frame(c)
SP_dis = cbind(unique(Alldata$xDistance),SP_dis)
colnames(SP_dis) = c('Dis',"Scirpus mariqueter",  "Scirpus triqueter",
                     "Juncellus serotinus",  "Carex scabrifolia",  "Imperata cylindrica")
data_long <- pivot_longer(SP_dis, cols = -Dis, names_to = "Species", values_to = "Value")

# Get allometry exponent
SP = unique(data_long$Species)
k = 0
k = k[-1]

rsquared = 0
rsquared = rsquared[-1]
for (i in 1:length(unique(data_long$Species))){
  lf_temp = data_long %>% 
    filter(data_long$Species == SP[i])
  fit = lm(Value ~ Dis, data = lf_temp)
  a = summary(fit)
  k[i] = a[["coefficients"]][2,1]
  rsquared[i] = a[["r.squared"]]
}
exp_allometry = data.frame(k, rsquared, SP)

p6 = ggplot(data_long, aes(x = Dis, y = Value, color = Species, shape = Species, group = Species)) +
  # geom_line(size = 1) +  # 绘制折线
  geom_point(size = 4, alpha = 0.6) +  # 添加数据点
  scale_shape_manual(values = shape_values) +
  geom_smooth(method = "lm", se = FALSE) +  # 添加虚线拟合线
  labs(x = "Distance to creek (m)", y = "Allometry exponent (a)") +
  # labs(x = "Dis2Creek(m)", y = "Allometry exponent(a)") +
  scale_color_manual(values = cbPalette) +  # 应用自定义颜色
  scale_x_continuous(breaks = seq(0, 30, by = 10), limits = c(0, 65)) +  # 设置x轴刻度和范围
  scale_y_continuous(breaks = seq(1, 3, by = .5), limits = c(.9, 3.2)) +  # 设置y轴刻度和范围
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA), 
    legend.position = c(0.75, 0.75),       # 将图例放在右上角内侧
    legend.text = element_text(face = "italic",size = FS-2),  # 设置图例字体为斜体
    legend.title = element_text(size = FS),  # 修改图例标题字号
    legend.background = element_blank(),  # 设置图例背景透明
    legend.box.background = element_blank(),  # 设置图例框背景透明
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    # axis.ticks.x = element_blank(),  # 移除x轴刻度线
    # axis.ticks.y = element_blank(),   # 移除y轴刻度线
    axis.title.x = element_text(size = FS,vjust = 0.5),
    axis.title.y = element_text(size = FS,vjust = 0.5, margin = margin(l = 10)),
    strip.text.x = element_text(size = FS,face="bold"),
    strip.text.y = element_text(size = FS,face="bold"),
    axis.text.x = element_text(size = FS,color='black'),
    axis.text.y = element_text(size = FS,color='black'),
    axis.line = element_line(size = 1),   # 修改x轴和y轴的线条粗细
    axis.ticks = element_line(size = 1),   # 修改刻度线的粗细
    axis.ticks.length = unit(0.20, "cm"),
    plot.margin = margin(0, 0, 10, 20)  # 上、右、下、左
  ) 
# p6
j = 1
for(i in c(4,5,3,1,2)){
  text1 <- paste(deparse(bquote(k == .(round(k[i], 2)))), collapse = "")
  text2 <- paste(deparse(bquote(R^2 == .(round(rsquared[i], 2)))), collapse = "")
  
  p6 = p6 +
    annotate("text", x = 34, y = 2.1 - .2*(j), label = text1,
             parse = TRUE, size = 5, color = cbPalette[j], hjust = 0, fontface = "italic") +
    annotate("text", x = 49, y = 2.125 - .2*(j), label = text2,
             parse = TRUE, size = 5, color = cbPalette[j], hjust = 0, fontface = "italic") 
  
  j = j+1
}
p6
ggsave(p6, file = "Fig4f.pdf", height = pic_height, width = pic_width)

p <- ggarrange(p1, p2, p3, p4, p5, p6,
               nrow = 3, ncol = 2,
               labels = c("A","B","C","D","E","F"),
               font.label = list(size = FS+2, face = "bold"), # 设置标签字体样式
               widths = c(1, 1),
               heights = c(1, 1, 1))  # 根据需要调整高度
p

ggsave(p, file = "Fig4.pdf", height = 12, width = 12)
