rm(list=ls())
# 加载ggplot2包
library(ggplot2)
library(dplyr)

cbPalette = c("#F8766D", "#00BA38", "#619CFF", "#F564E3","#B79F00", "#00BFC4")
textcol = cbPalette
shape_values = c(15:20)
alphadegree = 0.2
FS = 18
pic_height = 4
pic_width = 5

mytheme = theme(
  legend.position = c(0.8, 0.25),       # 将图例放在右上角内侧
  legend.title = element_text(size = FS),    # 设置图例标题字体大小
  legend.text = element_text(size = FS-2),
  panel.grid.major=element_line(colour=NA),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(size = 1),    # 修改x轴和y轴的线条粗细
  axis.ticks = element_line(size = 1),   # 修改刻度线的粗细
  axis.ticks.length = unit(0.20, "cm"),
  # axis.ticks.x = element_blank(),  # 移除x轴刻度线
  # axis.ticks.y = element_blank(),   # 移除y轴刻度线
  axis.title.x = element_text(size = FS,vjust = 0.5),
  axis.title.y = element_text(size = FS,vjust = 0.5),
  axis.text.x = element_text(size = FS,color='black'),
  axis.text.y = element_text(size = FS,color='black'),
  strip.text.x = element_text(size = FS,face="bold"),
  strip.text.y = element_text(size = FS,face="bold")
)

# 读取数据
data = read.csv("../../Data/PlantData/newdat.csv")
StatisticResult = c()
Species = unique(data$Species)

#################
data_ST = data[data$Species == Species[1],] #130
# data_CS = data[data$Species == "CS",] #74
# data_CD = data[data$Species == "CD",] #35
# data_SM = data[data$Species == "SM",] #34
# data_IC = data[data$Species == "IC",] #9
summary_data <- data_ST %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Diameter_mean, na.rm = TRUE),
    se = sd(Diameter_mean, na.rm = TRUE) / sqrt(n())
  )
# Linear fit
fit = lm(mean ~ xDistance, data = summary_data)
a = summary(fit)
RSquared = a$r.squared
Pvalue = 1- pf(a$fstatistic[1], a$fstatistic[2], a$fstatistic[3])
SR = cbind(Species[1],RSquared, Pvalue)
StatisticResult = rbind(StatisticResult,SR)

text1 = bquote(p == .(round(Pvalue, 2)))
text2 = bquote(R^2 == .(round(RSquared, 2)))

# Plot
p <- ggplot(summary_data, aes(x = xDistance, y = mean)) +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Distance to creek (m)",
       y = "Mean plant diameter (cm)") +
  # annotate("text", x = 12.5, y = 2.8, label = "Scirpus triqueter", fontface = "italic", size = 6, hjust = 0) +
  # # annotate("text", x = 12.5, y = 2.6, label= expression(italic(paste("P<0.001, ",R^2, " = 0.95"))), size = 6, hjust = 0) +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c(0, 30)) +
  scale_y_continuous(breaks=seq(1.6, 2.8, by=.4), limits=c(1.6, 2.8)) +
  theme_minimal() +
  mytheme
p
file_name = paste("Diameter-Dis2creek_", Species[1], ".pdf", sep = "")
ggsave(p, filename = file_name, width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径


#################
# data_ST = data[data$Species == "ST",] #130
data_CS = data[data$Species == "CS",] #74
# data_CD = data[data$Species == "CD",] #35
# data_SM = data[data$Species == "SM",] #34
# data_IC = data[data$Species == "IC",] #9
summary_data <- data_CS %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Diameter_mean, na.rm = TRUE),
    se = sd(Diameter_mean, na.rm = TRUE) / sqrt(n())
  )
# Linear fit
fit = lm(mean ~ xDistance, data = summary_data)
a = summary(fit)
RSquared = a$r.squared
Pvalue = 1- pf(a$fstatistic[1], a$fstatistic[2], a$fstatistic[3])
SR = cbind(Species[2],RSquared, Pvalue)
StatisticResult = rbind(StatisticResult,SR)

text1 = bquote(p == .(round(Pvalue, 2)))
text2 = bquote(R^2 == .(round(RSquared, 2)))

# Plot
p_CS <- ggplot(summary_data, aes(x = xDistance, y = mean)) +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_smooth(method = "loess", col = "red", se = TRUE) +
  labs(x = "Distance to creek (m)",
       y = "Mean plant diameter (cm)") +
  # annotate("text", x = 12.5, y = 2.8, label = "Carex scabrifolia", fontface = "italic", size = 6, hjust = 0) +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c (0, 30)) +
  scale_y_continuous(breaks=seq(1.8, 2.8, by = .2), limits=c(1.8, 2.8)) +
  theme_minimal() +
  mytheme
p_CS
ggsave(p_CS, file='Diameter-Dis2creek_CS.pdf', width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径

#################
# data_ST = data[data$Species == "ST",] #130
# data_CS = data[data$Species == "CS",] #74
data_CD = data[data$Species == "CD",] #35
# data_SM = data[data$Species == "SM",] #34
# data_IC = data[data$Species == "IC",] #9
summary_data <- data_CD %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Diameter_mean, na.rm = TRUE),
    se = sd(Diameter_mean, na.rm = TRUE) / sqrt(n())
  )
# Linear fit
fit = lm(mean ~ xDistance, data = summary_data)
a = summary(fit)
RSquared = a$r.squared
Pvalue = 1- pf(a$fstatistic[1], a$fstatistic[2], a$fstatistic[3])
SR = cbind(Species[3],RSquared, Pvalue)
StatisticResult = rbind(StatisticResult,SR)
# Plot
p_CD <- ggplot(summary_data, aes(x = xDistance, y = mean)) +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_smooth(method = "loess", col = "red", se = TRUE) +
  labs(x = "Distance to creek (m)",
       y = "Mean plant diameter (cm)") +
  # annotate("text", x = 12.5, y = 7.0, label = "Juncellus serotinus", fontface = "italic", size = 6, hjust = 0 ) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c (0, 30)) +
  scale_y_continuous(breaks=seq(4.5, 7.0, by=.5), limits=c(4.5, 7.0)) +
  theme_minimal() +
  mytheme
p_CD
ggsave(p_CD, file='Diameter-Dis2creek_CD.pdf', width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径

#################
# data_ST = data[data$Species == "ST",] #130
# data_CS = data[data$Species == "CS",] #74
# data_CD = data[data$Species == "CD",] #35
data_SM = data[data$Species == "SM",] #34
# data_IC = data[data$Species == "IC",] #9
summary_data <- data_SM %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Diameter_mean, na.rm = TRUE),
    se = sd(Diameter_mean, na.rm = TRUE) / sqrt(n())
  )
# Linear fit
fit = lm(mean ~ xDistance, data = summary_data)
a = summary(fit)
RSquared = a$r.squared
Pvalue = 1- pf(a$fstatistic[1], a$fstatistic[2], a$fstatistic[3])
SR = cbind(Species[4],RSquared, Pvalue)
StatisticResult = rbind(StatisticResult,SR)
# Plot
p_SM <- ggplot(summary_data, aes(x = xDistance, y = mean)) +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_smooth(method = "loess", col = "red", se = TRUE) +
  labs(x = "Distance to creek (m)",
       y = "Mean plant diameter (cm)") +
  # annotate("text", x = 12.5, y = 2.4, label = "Scirpus mariqueter", fontface = "italic", size = 6, hjust = 0 ) +
  theme_bw() +
  # scale_x_continuous(breaks=seq(0, 30, by=5), limits=c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c (0, 30)) +
  scale_y_continuous(breaks=seq(1.6, 2.4, by=.2), limits=c(1.6, 2.4)) +
  theme_minimal() +
  mytheme
p_SM
ggsave(p_SM, file='Diameter-Dis2creek_SM.pdf', width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径

#################
# data_ST = data[data$Species == "ST",] #130
# data_CS = data[data$Species == "CS",] #74
# data_CD = data[data$Species == "CD",] #35
# data_SM = data[data$Species == "SM",] #34
data_IC = data[data$Species == "IC",] #9
summary_data <- data_IC %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Diameter_mean, na.rm = TRUE),
    se = sd(Diameter_mean, na.rm = TRUE) / sqrt(n())
  )
# Linear fit
fit = lm(mean ~ xDistance, data = summary_data)
a = summary(fit)
RSquared = a$r.squared
Pvalue = 1- pf(a$fstatistic[1], a$fstatistic[2], a$fstatistic[3])
SR = cbind(Species[5],RSquared, Pvalue)
StatisticResult = rbind(StatisticResult,SR)
# Plot
p_IC <- ggplot(summary_data, aes(x = xDistance, y = mean)) +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_smooth(method = "loess", col = "red", se = FALSE) +
  labs(x = "Distance to creek (m)",
       y = "Mean plant diameter (cm)") +
  # annotate("text", x = 12.5, y = 4.0, label = "Imperata cylindrica", fontface = "italic", size = 6, hjust = 0 ) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c (0, 30)) +
  scale_y_continuous(breaks=seq(2.5, 4.0, by=.5), limits=c(2.5, 4.0)) +
  theme_minimal() +
  mytheme
p_IC
ggsave(p_IC, file='Diameter-Dis2creek_IC.pdf', width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径

write.csv(StatisticResult, file = 'Diameter-xDistance.csv', row.names = TRUE)
