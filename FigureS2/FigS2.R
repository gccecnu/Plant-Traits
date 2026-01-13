# 清空环境并加载必要包
remove(list = ls())

library(openxlsx)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(dplyr)

# 设置颜色和形状
cbPalette <- c("#F8766D", "#619CFF")  # 颜色用于 fill 区分不同距离

# 字体和样式设置
FS <- 16
mytheme <- theme(
  legend.position = c(0.05, 0.95),  # 图例放在图内左上角
  legend.justification = c("left", "top"),
  legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
  legend.title = element_text(size = FS),
  legend.text = element_text(size = FS - 2),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(size = 1),
  axis.ticks = element_line(size = 1),
  axis.ticks.length = unit(0.25, "cm"),
  axis.title.x = element_text(size = FS, vjust = 0.5),
  axis.title.y = element_text(size = FS, vjust = 0.5),
  axis.text.x = element_text(size = FS, color = 'black'),
  axis.text.y = element_text(size = FS, color = 'black'),
  strip.text.x = element_text(size = FS, face = "bold"),
  strip.text.y = element_text(size = FS, face = "bold")
)

# 设置标签
dis_edge <- c("Tidal flat", "Pioneer zone", "Interior marsh")
dis_creek <- c("0 meter", "15 meter")

# 读取数据
dat_GS <- read.xlsx("GrainSize_mean.xlsx")

# 整理变量
GS_data <- dat_GS %>%
  select(Transect, Dis_Creek, `Mean.(μm)`) %>%
  rename(
    GS_Transect = Transect,
    GS_dis_creek = Dis_Creek,
    GS_mean = `Mean.(μm)`
  ) %>%
  filter(!is.na(GS_dis_creek)) %>%
  mutate(
    GS_Transect = factor(GS_Transect, labels = dis_edge),
    GS_dis_creek = factor(GS_dis_creek, labels = dis_creek)
  )

# 计算均值、标准误差、样本量
summary_data <- GS_data %>%
  group_by(GS_Transect, GS_dis_creek) %>%
  summarise(
    mean = mean(GS_mean, na.rm = TRUE),
    se = sd(GS_mean, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

y_pad <- 0.03 * diff(range(summary_data$mean + summary_data$se, na.rm = TRUE))

# 绘图
p1 <- ggplot(summary_data, aes(x = GS_Transect, y = mean, fill = GS_dis_creek)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.7),
           color = "black",
           width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2,
                position = position_dodge(width = 0.7),
                size = 0.8) +
  geom_text(data = summary_data,
            aes(x = GS_Transect,
                y = mean + se + y_pad,                # 在误差线上方
                label = paste0("n = ", n),
                group = GS_dis_creek),                # 与填充分组一致
            position = position_dodge(width = 0.7),   # 与柱子的 dodge 一致
            vjust = 0,                                # 文字顶在指定 y 之上
            size = 5, fontface = "italic"
  ) +
  scale_y_continuous(limits = c(0, 35), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "",
    y = expression("Mean grain size ("*mu*"m)"),
    fill = "Distance to creek"
  ) +
  scale_fill_manual(values = cbPalette) +
  theme_minimal() +
  mytheme

# 显示图形
print(p1)

# 保存高质量图像
ggsave("FigureS2.pdf", plot = p1, width = 7, height = 6, dpi = 300)
