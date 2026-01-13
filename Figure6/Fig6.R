# =========================
# 环境准备
# =========================
rm(list = ls())

# 核心包
library(tidyverse)
library(rstatix)    # kruskal_test, dunn_test, add_significance
library(ggtext)
library(patchwork)  # 组合图
library(glue)
library(scales)

# 主题
FS <- 14
my_theme <- theme_bw(base_size = FS) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = FS),
    axis.text  = element_text(size = FS-2, color = "black"),
    strip.text = element_text(size = FS, face = "bold"),
    legend.position = "none",
    plot.margin = margin(t = 4, r = 6, b = 4, l = 6)
  )

# =========================
# 读数 + 整理
# =========================
dat <- read.csv("../Data/PlantData/newdat.csv", check.names = FALSE, stringsAsFactors = FALSE)

# 列名规范：如果是小写 `cond` 则统一为 `Cond`
if ("cond" %in% names(dat) && !("Cond" %in% names(dat))) {
  names(dat)[names(dat) == "cond"] <- "Cond"
}

# 高程标准化（相对值，最小值为0）
dat$Elevation <- dat$Elevation - min(dat$Elevation, na.rm = TRUE)

# 因子与排序
dat <- dat %>%
  mutate(
    creek     = factor(creek, levels = c("a","b","c"), labels = c("Creek A","Creek B","Creek C")),
    xDistance = factor(xDistance, levels = c(0,5,10,15,20,25,30)),
    yDistance = factor(yDistance, levels = c(0,50,100,150,200,250))
  )

# 目标变量与标签
vars   <- c("PH","WC","Elevation","Cond")
v_labs <- c(
  PH        = "Soil pH",
  WC        = "Water content",
  Elevation = "Relative elevation (m)",
  Cond      = "Soil conductivity"
)

# =========================
# 实用函数：分沟统计检验
# =========================
kruskal_by <- function(df, value, group_by_var) {
  # 按潮沟分别做 Kruskal–Wallis；若显著，再做 Dunn 事后
  out <- df %>%
    group_by(creek) %>%
    kruskal_test(reformulate(group_by_var, response = value)) %>%
    ungroup() %>%
    mutate(var = value, factor_axis = group_by_var)
  
  # 事后检验（仅对显著的部分）
  post <- df %>%
    group_by(creek) %>%
    group_modify(~{
      kw <- kruskal_test(.x, reformulate(group_by_var, response = value))
      if (kw$p < 0.05) {
        dunn_test(.x, reformulate(group_by_var, response = value)) %>%
          adjust_pvalue(method = "BH") %>%
          add_significance("p.adj")
      } else {
        tibble(group1 = character(), group2 = character(), p = numeric(),
               p.adj = numeric(), p.adj.signif = character())
      }
    }) %>% ungroup() %>%
    mutate(var = value, factor_axis = group_by_var)
  
  list(kw = out, dunn = post)
}

# =========================
# 统计：xDistance（跨沟短尺度）与 yDistance（沿岸长尺度）
# =========================
stats_x <- map(vars, ~kruskal_by(dat, .x, "xDistance")); names(stats_x) <- vars
stats_y <- map(vars, ~kruskal_by(dat, .x, "yDistance")); names(stats_y) <- vars

bind_rows(stats_x$PH$kw, stats_x$WC$kw, stats_x$Elevation$kw, stats_x$Cond$kw) %>%
  select(var, factor_axis, creek, statistic, p)
bind_rows(stats_y$PH$kw, stats_y$WC$kw, stats_y$Elevation$kw, stats_y$Cond$kw) %>%
  select(var, factor_axis, creek, statistic, p)

# =========================
# 作图函数
# =========================
p_x_box <- function(df, value, y_limits = NULL) {
  ggplot(df, aes(x = xDistance, y = .data[[value]])) +
    geom_boxplot(width = 0.6, outlier.shape = NA, aes(fill = xDistance)) +
    geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
    facet_wrap(~creek, nrow = 1) +
    labs(x = "Distance from creek (m)", y = v_labs[[value]]) +
    coord_cartesian(ylim = y_limits) +
    my_theme
}

p_y_box <- function(df, value, y_limits = NULL) {
  ggplot(df, aes(x = yDistance, y = .data[[value]])) +
    geom_boxplot(width = 0.6, outlier.shape = NA, aes(fill = yDistance)) +
    geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
    facet_wrap(~creek, nrow = 1) +
    labs(x = "Distance from shoreline (m)", y = v_labs[[value]]) +
    coord_cartesian(ylim = y_limits) +
    my_theme
}

# =========================
# 出图（主文：2 列 × 4 行；上两行 xDistance，下两行 yDistance）
# =========================

px1 <- p_x_box(dat, "PH")
px2 <- p_x_box(dat, "WC", y_limits = c(0.20, 0.30))
px3 <- p_x_box(dat, "Elevation")
px4 <- p_x_box(dat, "Cond")

py1 <- p_y_box(dat, "PH")
py2 <- p_y_box(dat, "WC", y_limits = c(0.20, 0.30))
py3 <- p_y_box(dat, "Elevation")
py4 <- p_y_box(dat, "Cond")

# 2 列 × 4 行： (A B) / (C D) / (E F) / (G H)
p_all <- (px1 | px2) / (px3 | px4) / (py1 | py2) / (py3 | py4)
p_all <- p_all + plot_annotation(tag_levels = "A")

# 保存主图
ggsave("Figure 6.png", p_all, width = 14, height = 14, dpi = 300)
# ggsave("Figure 6.pdf", p_all, width = 14, height = 14, dpi = 300)
