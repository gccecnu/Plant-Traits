rm(list=ls())
# The species changes with distance from shoreline and distance from creek
require(ggplot2)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(Hmisc)
library(ggsignif)
library(ggpubr)
library(ggtext)
library(agricolae)  # Significant analyze and plot

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")
pic_height = 4
pic_width = 5
# Load data
mydata = read.csv("../Data/PlantData/newdat.csv")
dat = mydata
dat = dat[-386,] # delete singular value
dat = dat[-204,]
dat = dat[-71,]
dat$xDistance = factor(dat$xDistance, levels = c("0","5","10","15",
                                                 "20","25","30"),
                       labels = c("0","5","10","15",
                                  "20","25","30"))

dat$yDistance = factor(dat$yDistance, levels = c("0","50","100",
                                                 "150","200","250"),
                       labels = c("0","50","100",
                                  "150","200","250"))
my_comparisons_x <- list(c("0","15"),
                         c("15","30"),
                         c("0","30"))
my_comparisons_y <- list(c("0","100"),
                         c("150","250"),
                         c("0","250"))
######## PH ########
## xDistance
p1 <- ggplot(dat, aes(xDistance, PH))+ 
  scale_fill_manual(values = heat.colors(7))+
  geom_boxplot(aes(fill = xDistance),position=position_dodge(0.3),
               width=0.5,outlier.color = NA)+
  ylim(7.5,9) +
  geom_jitter(width = 0.01, alpha = 1, color = 'black')+
  labs(x="Distance from creek (m)",y=c("PH"))+
  # ggtitle("PH changes with distance from creek")+
  ggtitle("")+
  theme_bw()+
  theme(plot.title = element_text(size=18,hjust=0.5),
        legend.text = element_text(size=18,hjust=0.5),
        legend.title = element_blank(),
        legend.justification = c(0.95, 0.95),
        legend.position = c(0.95, 0.95),
        legend.key = element_rect(fill='NA'),
        axis.title.x = element_text(size = 18,vjust = 0.5),
        axis.title.y = element_text(size = 18,vjust = 0.5),
        strip.text.x = element_text(size=18,face="bold"),
        strip.text.y = element_text(size=18,face="bold"),
        axis.text.x = element_text(size=14,color='black'),
        axis.text.y = element_text(size=14,color='black'),
        plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm")
  )+
  guides(fill = "none") 
  # geom_signif(comparisons = my_comparisons_y,step_increase = 0.1,
  #             map_signif_level = T,test = t.test)

# Significant analyze and plot
model = aov(-PH ~ xDistance, data = dat)
out = duncan.test(model, "xDistance", main = NULL, group = TRUE)
# out = duncan.test(model, "xDistance", main = NULL, group = FALSE)

out$groups$xDistance = as.numeric(rownames(out$groups))
out$groups$Max = out$means$Max
out$groups = out$groups[order(out$groups[,"xDistance"]),]

p1 = p1 +
  annotate('text', x = 1:7, y = 8.8*array(1,7),
           label = as.character(out$groups$groups),
           size = 6, color = "black")
p1

## yDistance
p2 <- ggplot(dat, aes(yDistance,PH))+ 
  scale_fill_manual(values = heat.colors(7))+
  geom_boxplot(aes(fill = yDistance),position=position_dodge(0.3),
               width=0.5,outlier.colour = NA)+
  ylim(7.5,9) +
  geom_jitter(width = 0.01, alpha = 1, color = 'black')+
  labs(x="Distance from shoreline (m)",y=c("PH"))+
  ggtitle("")+
  theme_bw()+
  theme(plot.title = element_text(size=18,hjust=0.5),
        legend.text = element_text(size=18,hjust=0.5),
        legend.title = element_blank(),
        legend.justification = c(0.95, 0.95),
        legend.position = c(0.95, 0.95),
        legend.key=element_rect(fill='NA'),
        axis.title.x = element_text(size = 18,vjust = 0.5),
        axis.title.y = element_text(size = 18,vjust = 0.5),
        strip.text.x = element_text(size=18,face="bold"),
        strip.text.y = element_text(size=18,face="bold"),
        axis.text.x = element_text(size=14,color='black'),
        axis.text.y = element_text(size=14,color='black'),
        plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm")
  )+
  guides(fill = "none") 
  # geom_signif(comparisons = my_comparisons_y,step_increase = 0.1,
  #             map_signif_level = T,test = t.test)
  
# Significant analyze and plot
model = aov(-PH ~ yDistance, data = dat)
out = duncan.test(model, "yDistance", main = NULL, group = TRUE)
out$groups$yDistance = as.numeric(rownames(out$groups))
out$groups = out$groups[order(out$groups[,"yDistance"]),]

p2 = p2 +
  annotate('text', x = 1:6, y = 8.8*array(1,6),
           label = as.character(out$groups$groups),
           size = 6, color = "black")
p2

####### Water content #######
# Distance from xDistance (creek)
p3 <- ggplot(dat, aes(xDistance,WC))+ 
  scale_fill_manual(values = cm.colors(7))+
  geom_boxplot(aes(fill = xDistance),position=position_dodge(0.3),
               width=0.5,outlier.colour = NA)+
  ylim(.15,.35) +
  geom_jitter(width = 0.01, alpha = 1, color = 'black')+
  labs(x="Distance from creek (m)",y=c("Water content"))+
  ggtitle("")+
  theme_bw()+
  theme(plot.title = element_text(size=18,hjust=0.5),
        legend.text = element_text(size=18,hjust=0.5),
        legend.title = element_blank(),
        legend.justification = c(0.95, 0.95),
        legend.position = c(0.95, 0.95),
        legend.key=element_rect(fill='NA'),
        axis.title.x = element_text(size = 18,vjust = 0.5),
        axis.title.y = element_text(size = 18,vjust = 0.5),
        strip.text.x = element_text(size=18,face="bold"),
        strip.text.y = element_text(size=18,face="bold"),
        axis.text.x = element_text(size=14,color='black'),
        axis.text.y = element_text(size=14,color='black'),
        plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm")
  )+
  guides(fill = "none")
  # geom_signif(comparisons = my_comparisons_x,step_increase = 0.1,
  #             map_signif_level = T,test = t.test)
model = aov(-WC ~ xDistance, data = dat)
out = duncan.test(model, "xDistance", main = NULL, group = TRUE)
# out = duncan.test(model, "xDistance", main = NULL, group = FALSE)

# Significant analyze and plot
out$groups$xDistance = as.numeric(rownames(out$groups))
out$groups$Max = out$means$Max
out$groups = out$groups[order(out$groups[,"xDistance"]),]

p3 = p3 +
  annotate('text', x = 1:7, y = .32*array(1,7),
           label = as.character(out$groups$groups),
           size = 6, color = "black")
p3
# ggsave(p3, file = "WC_xDistance.pdf",height = pic_height, width = pic_width)
#
## Distance from yDistance(shoreline)
p4 <- ggplot(dat, aes(yDistance,WC))+ 
  scale_fill_manual(values = cm.colors(7))+
  geom_boxplot(aes(fill = yDistance),position=position_dodge(0.3),
               width=0.5,outlier.colour = NA)+
  ylim(.15,.35) +
  geom_jitter(width = 0.01, alpha = 1, color = 'black')+
  labs(x="Distance from shoreline (m)",y=c("Water content"))+
  ggtitle("")+
  theme_bw()+
  theme(plot.title = element_text(size=18,hjust=0.5),
        legend.text = element_text(size=18,hjust=0.5),
        legend.title = element_blank(),
        legend.justification = c(0.95, 0.95),
        legend.position = c(0.95, 0.95),
        legend.key=element_rect(fill='NA'),
        axis.title.x = element_text(size = 18,vjust = 0.5),
        axis.title.y = element_text(size = 18,vjust = 0.5),
        strip.text.x = element_text(size=18,face="bold"),
        strip.text.y = element_text(size=18,face="bold"),
        axis.text.x = element_text(size=14,color='black'),
        axis.text.y = element_text(size=14,color='black'),
        plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm")
  ) +
  guides(fill = "none")
  # geom_signif(comparisons = my_comparisons_y,step_increase = 0.1,
  #             map_signif_level = T,test = t.test)

# Significant analyze and plot
model = aov(-WC ~ yDistance, data = dat)
out = duncan.test(model, "yDistance", main = NULL, group = TRUE)
out$groups$yDistance = as.numeric(rownames(out$groups))
out$groups = out$groups[order(out$groups[,"yDistance"]),]

p4 = p4 +
  annotate('text', x = 1:6, y = .32*array(1,6),
           label = as.character(out$groups$groups),
           size = 6, color = "black")

p4

####### Elevation #######
## xDistance
p5 <- ggplot(dat, aes(xDistance,Elevation))+ 
  scale_fill_manual(values = terrain.colors(7))+
  geom_boxplot(aes(fill = xDistance),position=position_dodge(0.3),
               width=0.5,outlier.colour = NA)+
  ylim(2.5,4.5) +
  geom_jitter(width = 0.01, alpha = 1, color = 'black')+
  labs(x="Distance from creek (m)",y=c("Elevation (m)"))+
  ggtitle("")+
  theme_bw()+
  theme(plot.title = element_text(size=18,hjust=0.5),
        legend.text = element_text(size=18,hjust=0.5),
        legend.title = element_blank(),
        legend.justification = c(0.95, 0.95),
        legend.position = c(0.95, 0.95),
        legend.key=element_rect(fill='NA'),
        axis.title.x = element_text(size = 18,vjust = 0.5),
        axis.title.y = element_text(size = 18,vjust = 0.5),
        strip.text.x = element_text(size=18,face="bold"),
        strip.text.y = element_text(size=18,face="bold"),
        axis.text.x = element_text(size=14,color='black'),
        axis.text.y = element_text(size=14,color='black'),
        plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm")
  ) +
  guides(fill = "none")
  # geom_signif(comparisons = my_comparisons_x,step_increase = 0.1,
  #             map_signif_level = T,test = t.test)
model = aov(-Elevation ~ xDistance, data = dat)
out = duncan.test(model, "xDistance", main = NULL, group = TRUE)
# out = duncan.test(model, "xDistance", main = NULL, group = FALSE)

# Significant analyze and plot
out$groups$xDistance = as.numeric(rownames(out$groups))
out$groups$Max = out$means$Max
out$groups = out$groups[order(out$groups[,"xDistance"]),]

p5 = p5 +
  annotate('text', x = 1:7, y = 4.2*array(1,7),
           label = as.character(out$groups$groups),
           size = 6, color = "black")
p5
# ggsave(p5, file = "Ele_xDistance.pdf", height = pic_height, width = pic_width)

## yDistance
p6 <- ggplot(dat, aes(yDistance,Elevation))+ 
  scale_fill_manual(values = terrain.colors(7))+
  geom_boxplot(aes(fill = yDistance),position=position_dodge(0.3),
               width=0.5,outlier.colour = NA)+
  ylim(2.5,4.5) +
  geom_jitter(width = 0.01, alpha = 1, color = 'black')+
  labs(x="Distance from shoreline (m)",y=c("Elevation (m)"))+
  ggtitle("")+
  theme_bw()+
  theme(plot.title = element_text(size=18,hjust=0.5),
        legend.text = element_text(size=18,hjust=0.5),
        legend.title = element_blank(),
        legend.justification = c(0.95, 0.95),
        legend.position = c(0.95, 0.95),
        legend.key=element_rect(fill='NA'),
        axis.title.x = element_text(size = 18,vjust = 0.5),
        axis.title.y = element_text(size = 18,vjust = 0.5),
        strip.text.x = element_text(size=18,face="bold"),
        strip.text.y = element_text(size=18,face="bold"),
        axis.text.x = element_text(size=14,color='black'),
        axis.text.y = element_text(size=14,color='black'),
        plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm")
  ) +
  guides(fill = "none") 
  # geom_signif(comparisons = my_comparisons_y,step_increase = 0.1,
  #             map_signif_level = T,test = t.test)

# Significant analyze and plot
model = aov(-Elevation ~ yDistance, data = dat)
out = duncan.test(model, "yDistance", main = NULL, group = TRUE)
out$groups$yDistance = as.numeric(rownames(out$groups))
out$groups = out$groups[order(out$groups[,"yDistance"]),]

p6 = p6 +
  annotate('text', x = 1:6, y = 4.2*array(1,6),
           label = as.character(out$groups$groups),
           size = 6, color = "black")
p6

####### Combine Multiple plots #######
p = ggarrange(p1,p3,p5,p2,p4,p6, labels = c("A", "B", "C","D","E","F"),ncol = 3, nrow = 2)
p
ggsave(p, filename = "FigureS2.pdf", width = 12, height = 8)
