rm(list=ls())
library("rnaturalearth")
library("rnaturalearthdata")
# library("rnaturalearthhires")
library("ggspatial")
library("ggplot2")

OpenWindow = function (Width,Height) {
  if (Sys.info()["sysname"]=="Darwin"){  # (Darwin stands for a Mac computer)
    quartz(width=Width, height=Height)           
  } else {
    windows(width = Width, height = Height)}
}

SaveFigure=function(FileName){
  if (Sys.info()["sysname"]=="Darwin"){ # (Darwin stands for a Mac computer)
    quartz.save(paste(FileName,'.pdf',sep=''),type = c("pdf"),device = dev.cur())
  } else
    savePlot(filename = FileName,type = c("pdf"),device = dev.cur(),restoreConsole = TRUE) 
}
OpenWindow(12,12)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
dataxy = read.csv("sitexy.csv")
FS = 16
# par(mar=c(3.2, 5, 2.2, 0.1) + 0.1)
ggplot(data = world) +
  geom_sf()+ 
  theme_bw()+
  theme(plot.margin=unit(c(0.9,1.1,0.9,0.9),'lines'),
        axis.text.x = element_text(size = 44),
        axis.text.y = element_text(size = 44),
        axis.ticks.x = element_line(size = 2, lineend = 'round'),
        axis.ticks.y = element_line(size = 2, lineend = 'round'),
        legend.text = element_text(size = 44,color='black'),
        legend.title = element_text(size = 44, face = "plain"),
        legend.position = "none", 
        panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(size = 4),
        panel.grid = element_blank()
        ) +
  # 
  # annotate(geom = "text", x = 121.77, y = 41.59, label = "Liaohe Estuary (b)",
  #          fontface = "italic", color = "grey22", size = FS)+
  # annotate(geom = "text", x = 121.19, y = 37.05, label = "Yellow River Estuary (c)",
  #          fontface = "italic", color = "grey22", size = FS) +
  annotate(geom = "text", x = 121.95, y = 32.17, label = "Yangtze River Estuary",
           fontface = "italic", color = "grey22", size = FS) +
  # annotate(geom = "text", x = 121.15, y = 29.65, label = "Hangzhou Bay (e)",
  #          fontface = "italic", color = "grey22", size = FS) +

  annotation_scale(location = "bl", width_hint = 0.25, 
                   pad_x = unit(1, "cm"), pad_y = unit(1,"cm"),
                   text_cex = 3.7) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         height = unit(4, "cm"), width = unit(4, "cm"),
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering(text_size = 44)) +
  
  # coord_sf(xlim = c(115,132), ylim = c(25,45)) +
  geom_point(data=dataxy, aes(x=Longitude[3], y=Latitude[3]),
             size = 5, shape = 15, alpha = 0.8) +
  guides(shape = guide_legend(nrow = 6, byrow = F)) +
  scale_x_continuous(limits = c(115,130), breaks = seq(115, 130, 5)) +
  scale_y_continuous(limits = c(25,40), breaks = seq(25, 40, 5)) +
  # geom_point(aes(121.93,31.46),size = 3,shape = 17)+
  # geom_point(aes(121.54,30.81),size = 3,shape = 17)+
  labs(x="",y="")
SaveFigure("fig1a")
