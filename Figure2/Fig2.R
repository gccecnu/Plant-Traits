rm(list=ls())
# 加载ggplot2包
library(ggplot2)
library(dplyr)
library(ggpubr)
library(showtext)

# font_add('Songti','/System/Library/Fonts/Supplemental/Songti.ttc')
showtext_auto()

cbPalette = c("#F8766D", "#00BA38", "#619CFF", "#F564E3","#B79F00", "#00BFC4")
textcol = cbPalette
shape_values = c(15:20)
alphadegree = 0.8
FS = 16

mytheme = theme(
  # text = element_text('Songti'),
  # legend.position = c(0.2, 0.765),       # 将图例放在右上角内侧
  legend.position = "right",       # 将图例放在右边外侧
  legend.title = element_text(size = FS),    # 设置图例标题字体大小
  legend.text = element_text(size = FS-2),
  panel.grid.major=element_line(colour=NA),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(size = 1),    # 修改x轴和y轴的线条粗细
  axis.ticks = element_line(size = 1),   # 修改刻度线的粗细
  axis.ticks.length = unit(0.25, "cm"),
  axis.title.x = element_text(size = FS,vjust = 0.5),
  axis.title.y = element_text(size = FS,vjust = 0.5),
  axis.text.x = element_text(size = FS,color='black'),
  axis.text.y = element_text(size = FS,color='black'),
  strip.text.x = element_text(size = FS,face="bold"),
  strip.text.y = element_text(size = FS,face="bold")
)

# 读取数据
data = read.csv("../Data/PlantData/newdat.csv")
data$Elevation = data$Elevation - min(data$Elevation)
colnames(data)[5] = "Species"
data$Species = factor(data$Species, levels = c("CD","CS","IC","SM","ST","P"),
                      labels = c("Juncellus serotinus","Carex scabrifolia",
                                 "Imperate cylindrica","Scirpus mariqueter",
                                 "Scirpus triqueter","Phragmites australis"))
data$Biomass = data$Biomass/(0.25*0.25) # g/m^2
data_frame <- data %>% filter(Biomass != 0.0)

# Biomass-Elevation
p1 <- ggplot(data_frame, aes(x=Elevation, y=Biomass, group=Species, color= Species)) +
  geom_point(size = 3, alpha = 0.3, show.legend = FALSE) +
  stat_ellipse(level = 0.9, type = "norm", size = 1, alpha = 0.5) +  # 90%置信椭圆
  scale_x_continuous(breaks=seq(0, 1.5, by=0.5), limits=c(0, 1.6)) +
  scale_y_continuous(breaks=seq(0, 800, by=200), limits=c(0, 800)) +
  scale_color_manual(values = cbPalette) +  # 设置自定义颜色
  labs(x="Elevation (m)",
       y= expression("Dry biomass (g"*m^2*")")) +
  mytheme +
  theme(legend.position = "none")  # 隐藏图例
p1

# Biomass-xDistance
summarized_data <- data %>%
  group_by(Species, xDistance) %>%
  summarise(total_biomass = sum(Biomass, na.rm = T), .groups = 'drop')

p2 <- ggplot(summarized_data, aes(x = xDistance, y = total_biomass, fill = Species)) +
  geom_bar(stat = "identity", alpha = alphadegree) +  # 使用柱状图
  scale_y_continuous(breaks=seq(0, 6000, by=2000), limits=c(0, 6000)) +
  # geom_point(size = 2) +     # 添加数据点
  labs(x = "Distance to tidal creek (m)",
       y = expression("Dry biomass (g/"*m^2*")")) +
  theme_minimal() +
  scale_fill_manual(values = cbPalette) +  # 设置自定义颜色
  mytheme
p2

p = ggarrange(p1,p2,
              labels = c("A","B"),
              font.label = list(size = FS+2, face = "bold"), # 设置标签字体样式
              widths = c(1, 1.5),
              heights = c(1))  # 根据需要调整高度

p
ggsave(p, file='Figure2V2.pdf', width=10, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径
