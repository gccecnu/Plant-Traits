rm(list=ls())
# 加载ggplot2包
library(ggplot2)
library(dplyr)

# 读取数据
data = read.csv("../Data/PlantData/newdat.csv")
StatisticResult = c()
Species = unique(data$Species)
data_ST = data[data$Species == Species[1],] #130
# data_CS = data[data$Species == "CS",] #74
# data_CD = data[data$Species == "CD",] #35
# data_SM = data[data$Species == "SM",] #34
# data_IC = data[data$Species == "IC",] #9
summary_data <- data_ST %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Mature_rate, na.rm = TRUE),
    se = sd(Mature_rate, na.rm = TRUE) / sqrt(n())
  )
# Linear fit
fit = lm(mean ~ xDistance, data = summary_data)
a = summary(fit)
RSquared = a$r.squared
Pvalue = 1- pf(a$fstatistic[1], a$fstatistic[2], a$fstatistic[3])
SR = cbind(Species[1],RSquared, Pvalue)
StatisticResult = rbind(StatisticResult,SR)
# Plot
p <- ggplot(summary_data, aes(x = xDistance, y = mean)) +
  # geom_line(size=1) + 
  # geom_boxplot() +
  geom_smooth(method = "lm", col = "red") +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Distance to creek (m)",
       y = "Mature rate") +
  annotate("text", x = 20, y = 1.0, label = "Scirpus triqueter", fontface = "italic", size = 6 ) +
  # annotate("text", x = 22.5, y = 0.95, label= expression(italic(paste("P<0.001, ",R^2, " = 0.95"))), size = 6 ) +
  theme_bw() +
  # scale_x_continuous(breaks=seq(0, 30, by=5), limits=c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c(0, 30)) +
  scale_y_continuous(breaks=seq(0.5, 1.0, by=.1), limits=c(0.5, 1.0)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18,vjust = 0.5),
    axis.title.y = element_text(size = 18,vjust = 0.5),
    strip.text.x = element_text(size=18,face="bold"),
    strip.text.y = element_text(size=18,face="bold"),
    axis.text.x = element_text(size=14,color='black'),
    axis.text.y = element_text(size=14,color='black'),
    plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # geom_jitter(width = 0.05, alpha = 1, color = 'black')+
  # scale_x_continuous(breaks=seq(2.4, 3.8, by=0.2), limits=c(2.4, 3.8)) +
  # scale_y_continuous(breaks=seq(0, 45, by=5), limits=c(0, 45)) +
  guides(fill = "none")
p
file_name = paste("Mature-Dis2creek_", Species[1], ".pdf", sep = "")
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
    mean = mean(Mature_rate, na.rm = TRUE),
    se = sd(Mature_rate, na.rm = TRUE) / sqrt(n())
  )
# Linear fit
fit = lm(mean ~ xDistance, data = summary_data)
a = summary(fit)
RSquared = a$r.squared
Pvalue = 1- pf(a$fstatistic[1], a$fstatistic[2], a$fstatistic[3])
SR = cbind(Species[2],RSquared, Pvalue)
StatisticResult = rbind(StatisticResult,SR)
# Plot
p_CS <- ggplot(summary_data, aes(x = xDistance, y = mean)) +
  # geom_line(size=1) + 
  # geom_boxplot() +
  geom_smooth(method = "lm", col = "red") +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Distance to creek (m)",
       y = "Mature rate") +
  # annotate("text", x = 22.5, y = 1, label = "Carex scabrifolia", fontface = "italic", size = 6 ) +
  theme_bw() +
  # scale_x_continuous(breaks=seq(0, 30, by=5), limits=c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c (0, 30)) +
  # scale_y_continuous(breaks=seq(.5, 1.0, by = .1), limits=c(.5, 1.0)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18,vjust = 0.5),
    axis.title.y = element_text(size = 18,vjust = 0.5),
    strip.text.x = element_text(size=18,face="bold"),
    strip.text.y = element_text(size=18,face="bold"),
    axis.text.x = element_text(size=14,color='black'),
    axis.text.y = element_text(size=14,color='black'),
    plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # geom_jitter(width = 0.05, alpha = 1, color = 'black')+
  # scale_x_continuous(breaks=seq(2.4, 3.8, by=0.2), limits=c(2.4, 3.8)) +
  # scale_y_continuous(breaks=seq(0, 45, by=5), limits=c(0, 45)) +
  guides(fill = "none")
p_CS
# ggsave(p_CS, file='Mature-Dis2creek_CS.pdf', width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径

#################
# data_ST = data[data$Species == "ST",] #130
# data_CS = data[data$Species == "CS",] #74
data_CD = data[data$Species == "CD",] #35
# data_SM = data[data$Species == "SM",] #34
# data_IC = data[data$Species == "IC",] #9
summary_data <- data_CD %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Mature_rate, na.rm = TRUE),
    se = sd(Mature_rate, na.rm = TRUE) / sqrt(n())
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
  # geom_line(size=1) + 
  # geom_boxplot() +
  geom_smooth(method = "lm", col = "red") +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Distance to creek (m)",
       y = "Mature rate") +
  annotate("text", x = 22.5, y = .2, label = "Juncellus serotinus", fontface = "italic", size = 6 ) +
  theme_bw() +
  # scale_x_continuous(breaks=seq(0, 30, by=5), limits=c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c (0, 30)) +
  scale_y_continuous(breaks=seq(0, .2, by=.05), limits=c(0, .2)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18,vjust = 0.5),
    axis.title.y = element_text(size = 18,vjust = 0.5),
    strip.text.x = element_text(size=18,face="bold"),
    strip.text.y = element_text(size=18,face="bold"),
    axis.text.x = element_text(size=14,color='black'),
    axis.text.y = element_text(size=14,color='black'),
    plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # geom_jitter(width = 0.05, alpha = 1, color = 'black')+
  # scale_x_continuous(breaks=seq(2.4, 3.8, by=0.2), limits=c(2.4, 3.8)) +
  # scale_y_continuous(breaks=seq(0, 45, by=5), limits=c(0, 45)) +
  guides(fill = "none")
p_CD
# ggsave(p_CD, file='Mature-Dis2creek_CD.pdf', width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径

#################
# data_ST = data[data$Species == "ST",] #130
# data_CS = data[data$Species == "CS",] #74
# data_CD = data[data$Species == "CD",] #35
data_SM = data[data$Species == "SM",] #34
# data_IC = data[data$Species == "IC",] #9
summary_data <- data_SM %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Mature_rate, na.rm = TRUE),
    se = sd(Mature_rate, na.rm = TRUE) / sqrt(n())
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
  # geom_line(size=1) + 
  # geom_boxplot() +
  geom_smooth(method = "lm", col = "red") +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Distance to creek (m)",
       y = "Mature rate") +
  annotate("text", x = 22.5, y = .8, label = "Scirpus mariqueter", fontface = "italic", size = 6 ) +
  theme_bw() +
  # scale_x_continuous(breaks=seq(0, 30, by=5), limits=c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c (0, 30)) +
  scale_y_continuous(breaks=seq(0, .8, by=.2), limits=c(0, .8)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18,vjust = 0.5),
    axis.title.y = element_text(size = 18,vjust = 0.5),
    strip.text.x = element_text(size=18,face="bold"),
    strip.text.y = element_text(size=18,face="bold"),
    axis.text.x = element_text(size=14,color='black'),
    axis.text.y = element_text(size=14,color='black'),
    plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # geom_jitter(width = 0.05, alpha = 1, color = 'black')+
  # scale_x_continuous(breaks=seq(2.4, 3.8, by=0.2), limits=c(2.4, 3.8)) +
  # scale_y_continuous(breaks=seq(0, 45, by=5), limits=c(0, 45)) +
  guides(fill = "none")
p_SM
ggsave(p_SM, file='Mature-Dis2creek_SM.pdf', width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径

#################
# data_ST = data[data$Species == "ST",] #130
# data_CS = data[data$Species == "CS",] #74
# data_CD = data[data$Species == "CD",] #35
# data_SM = data[data$Species == "SM",] #34
data_IC = data[data$Species == "IC",] #9
summary_data <- data_IC %>%
  group_by(xDistance) %>%
  summarise(
    mean = mean(Mature_rate, na.rm = TRUE),
    se = sd(Mature_rate, na.rm = TRUE) / sqrt(n())
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
  # geom_line(size=1) + 
  # geom_boxplot() +
  geom_smooth(method = "lm", col = "red") +
  geom_point(data = summary_data, aes(x = xDistance, y = mean), size = 3) +
  geom_errorbar(data = summary_data, aes(x = xDistance, ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(x = "Distance to creek (m)",
       y = "Mature rate") +
  annotate("text", x = 22.5, y = .2, label = "Imperata cylindrica", fontface = "italic", size = 6 ) +
  theme_bw() +
  # scale_x_continuous(breaks=seq(0, 30, by=5), limits=c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(breaks=seq(0, 30, by=5), limits=c (0, 30)) +
  scale_y_continuous(breaks=seq(0, .2, by=.05), limits=c(0, .2)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18,vjust = 0.5),
    axis.title.y = element_text(size = 18,vjust = 0.5),
    strip.text.x = element_text(size=18,face="bold"),
    strip.text.y = element_text(size=18,face="bold"),
    axis.text.x = element_text(size=14,color='black'),
    axis.text.y = element_text(size=14,color='black'),
    plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # geom_jitter(width = 0.05, alpha = 1, color = 'black')+
  guides(fill = "none")
p_IC
# ggsave(p_IC, file='Mature-Dis2creek_IC.pdf', width=5, height=4) # 可以指定大小，如宽为12cm，高为10cm，需要指定保存路径
