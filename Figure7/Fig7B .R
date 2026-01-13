# =========================
# 0) 准备环境
# =========================
rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(ggrepel)
library(scales)
library(corrplot)    # 如果没有：install.packages("corrplot")

path_csv <- "../Data/PlantData/newdat.csv"

# =========================
# 1) 读取并重命名变量
# =========================
dat <- read.csv(path_csv, check.names = FALSE, stringsAsFactors = FALSE)

name_map <- c(
  "Height_mean"   = "Mean plant height",
  "Diameter_mean" = "Mean stem diameter",
  "Biomass"       = "Aboveground biomass",
  "Mature_rate"   = "Seed rate",
  "Elevation"     = "Elevation",
  "PH"            = "Soil pH",
  "Cond"          = "Electrical conductivity",
  "WC"            = "Soil moisture content",
  "Pwc"           = "Leaf relative water content"
)

present <- intersect(names(name_map), names(dat))
dat_renamed <- dat
names(dat_renamed)[match(present, names(dat_renamed))] <- name_map[present]

pca_vars <- unname(name_map[present])
X_raw <- dat_renamed[, pca_vars, drop = FALSE]

# 若数据中没有 Species，创建默认分组；否则使用原列
if (!"Species" %in% colnames(dat_renamed)) {
  dat_renamed$Species <- rep("GroupA", nrow(dat_renamed))
}

# =========================
# 2) 缺失值处理与标准化基准数据
# =========================
X_complete <- na.omit(X_raw)
# 与 X_complete 行一致的 Species（用原始行号对齐）
species_vec <- dat_renamed$Species[as.integer(rownames(X_complete))]

# 输出样本量与变量数
n_obs  <- nrow(X_complete)
n_vars <- ncol(X_complete)
cat(sprintf("[INFO] N obs used: %d; N vars: %d\n", n_obs, n_vars))

# =========================
# 3) 原始 PCA（基准）
# =========================
pca_raw <- prcomp(X_complete, center = TRUE, scale. = TRUE)

std_dev_raw <- pca_raw$sdev
prop_var_raw <- std_dev_raw^2 / sum(std_dev_raw^2)
cum_prop_raw <- cumsum(prop_var_raw)

variance_raw <- data.frame(
  PC = paste0("PC", seq_along(std_dev_raw)),
  `Standard deviation` = std_dev_raw,
  `Proportion of Variance` = prop_var_raw,
  `Cumulative Proportion` = cum_prop_raw,
  check.names = FALSE
)

loadings_raw <- as.data.frame(pca_raw$rotation)
scores_raw   <- as.data.frame(pca_raw$x)
# 合并分组（原始）
scores_raw$Species <- species_vec

# =========================
# 4) 相关性筛选 + 变量变换 + 优化 PCA（稳健）
# =========================

# ---- 4.1 计算相关矩阵并筛选弱相关变量 ----
cor_mat <- cor(X_complete, use = "pairwise.complete.obs")
abs_cor <- abs(cor_mat)
mean_abs_cor <- colMeans(abs_cor, na.rm = TRUE)

threshold <- 0.25  # 弱相关阈值，可调
weak_vars <- names(mean_abs_cor[mean_abs_cor < threshold])
strong_vars <- setdiff(colnames(X_complete), weak_vars)

message(sprintf("[INFO] 平均相关度阈值 = %.2f", threshold))
message(sprintf("[INFO] 弱相关变量（已剔除）: %s",
                ifelse(length(weak_vars) > 0, paste(weak_vars, collapse = ", "), "无")))
message(sprintf("[INFO] 保留变量用于优化 PCA: %s",
                paste(strong_vars, collapse = ", ")))

# 提取强相关变量子集（行未变）
X_strong <- X_complete[, strong_vars, drop = FALSE]

# ---- 4.2 对偏态变量进行 log1p 变换（安全）----
cand <- grep("biomass|diameter|conductivity", names(X_strong), value = TRUE, ignore.case = TRUE)
cand <- intersect(cand, names(X_strong))
cand <- cand[sapply(cand, function(v) is.numeric(X_strong[[v]]))]

if (length(cand) > 0) {
  # 安全检查：避免 <= -1 的值导致 log1p 域错误
  bad_cols <- cand[sapply(cand, function(v) any(X_strong[[v]] <= -1, na.rm = TRUE))]
  if (length(bad_cols) > 0) {
    message(sprintf("[WARN] 下列列含有 <= -1 的值，跳过 log1p: %s",
                    paste(bad_cols, collapse = ", ")))
    cand <- setdiff(cand, bad_cols)
  }
  if (length(cand) > 0) {
    for (v in cand) X_strong[[v]] <- log1p(X_strong[[v]])
    message(sprintf("[INFO] 已执行 log1p 变换的列: %s", paste(cand, collapse = ", ")))
  } else {
    message("[INFO] 无可安全 log1p 的列。")
  }
} else {
  message("[INFO] 无需 log1p 变换的列（可能已被筛除或均为非数值型）。")
}

message(sprintf("[INFO] X_strong 当前变量: %s", paste(names(X_strong), collapse = ", ")))

# ---- 4.3 优化 PCA ----
pca_opt <- prcomp(X_strong, center = TRUE, scale. = TRUE)

std_dev_opt <- pca_opt$sdev
prop_var_opt <- std_dev_opt^2 / sum(std_dev_opt^2)
cum_prop_opt <- cumsum(prop_var_opt)

variance_opt <- data.frame(
  PC = paste0("PC", seq_along(std_dev_opt)),
  `Standard deviation` = std_dev_opt,
  `Proportion of Variance` = prop_var_opt,
  `Cumulative Proportion` = cum_prop_opt,
  check.names = FALSE
)

loadings_opt <- as.data.frame(pca_opt$rotation)
scores_opt   <- as.data.frame(pca_opt$x)

# 合并分组（优化）——先写入，再映射学名
scores_opt$Species <- species_vec

# 如你的 Species 为编码（CD/CS/IC/SM/ST/P），映射为学名：
scores_opt$Species <- factor(scores_opt$Species,
                             levels = c("CD","CS","IC","SM","ST","P"),
                             labels = c("Juncellus serotinus",
                                        "Carex scabrifolia",
                                        "Imperata cylindrica",
                                        "Scirpus mariqueter",
                                        "Scirpus triqueter",
                                        "Phragmites australis")
)

cat("\n[RESULT] 优化后 PCA 方差贡献（前3个PC）:\n")
num_cols <- sapply(variance_opt, is.numeric)
variance_opt_print <- variance_opt
variance_opt_print[num_cols] <- lapply(variance_opt_print[num_cols], round, 4)
print(variance_opt_print[1:3, ])

# =========================
# 5) 原始 vs 优化 贡献率对比表
# =========================
n_pc <- min(nrow(variance_raw), nrow(variance_opt))
compare_df <- data.frame(
  PC = paste0("PC", seq_len(n_pc)),
  Raw_Proportion       = prop_var_raw[seq_len(n_pc)],
  Raw_Cumulative       = cum_prop_raw[seq_len(n_pc)],
  Optimized_Proportion = prop_var_opt[seq_len(n_pc)],
  Optimized_Cumulative = cum_prop_opt[seq_len(n_pc)]
)
compare_df <- compare_df %>% mutate(across(where(is.numeric), round, 4))
print(head(compare_df, 6))

# =========================
# 6) 可视化
# =========================

# 6.1 相关矩阵（原始）
corrplot(abs_cor, method = "color", type = "upper", tl.col = "black",
         tl.srt = 45, title = "Absolute Correlation Matrix (Raw)", mar = c(0,0,2,0))

# 6.2 碎石图（原始 vs 优化）
df_scree_raw <- data.frame(PC = seq_along(prop_var_raw),
                           Prop = prop_var_raw, Model = "Raw")
df_scree_opt <- data.frame(PC = seq_along(prop_var_opt),
                           Prop = prop_var_opt, Model = "Optimized")
df_scree <- rbind(df_scree_raw, df_scree_opt)

ggplot(df_scree, aes(PC, Prop, group = Model)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = df_scree$PC) +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~Model, ncol = 1) +
  labs(x = "Principal Component", y = "Proportion of Variance Explained",
       title = "Scree Plot: Raw vs Optimized")

# 6.3 Biplot（优化版，PC1–PC2）
load2 <- loadings_opt[, c("PC1", "PC2"), drop = FALSE]
load2$var <- rownames(load2)

# 箭头缩放（以样本PC范围缩放）
arrow_scale <- max(abs(scores_opt$PC1), abs(scores_opt$PC2), na.rm = TRUE)
load2$xend <- load2$PC1 * arrow_scale
load2$yend <- load2$PC2 * arrow_scale

# 方差解释百分比（标签）
var_exp <- round(variance_opt$`Proportion of Variance`[1:2] * 100, 1)

# 自定义主题与颜色
cbPalette <- c("#F8766D", "#00BA38", "#619CFF", "#F564E3", "#B79F00", "#00BFC4")
FS <- 16
mytheme <- theme(
  legend.text = element_text(size = FS-2, hjust = 0),
  legend.title = element_text(size = FS),
  legend.position = "right",
  panel.grid = element_line(linetype = "dashed", linewidth = 0.3, colour = "grey80"),
  axis.ticks = element_line(size = 1),
  axis.ticks.length = unit(0.25, "cm"),
  axis.title.x = element_text(size = FS, vjust = 0.5),
  axis.title.y = element_text(size = FS, vjust = 0.5),
  axis.text.x = element_text(size = FS, color='black'),
  axis.text.y = element_text(size = FS, color='black'),
  strip.text.x = element_text(size = FS, face="bold", color='black'),
  strip.text.y = element_text(size = FS, face="bold", color='black'),
  plot.margin = margin(t=0.0, r=0.3, b=0.12, l=0.3, "cm")
)
Species_shapes = c(0,1,2,5,7)

p <- ggplot() +
  geom_point(data = scores_opt,
             aes(x = PC1, y = PC2, color = Species, shape = Species),
             size = 2.8, stroke = 0.9, alpha = 0.7) + 
  geom_segment(data = load2,
               aes(x = 0, y = 0, xend = xend, yend = yend),
               color = "#8B0000", linewidth = 0.8,
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_text_repel(data = load2,
                  aes(x = xend, y = yend, label = var),
                  color = "#8B0000", size = 4.2, segment.color = "grey60",
                  max.overlaps = Inf,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  nudge_x = load2$xend * 0.1,
                  nudge_y = load2$yend * 0.1,
                  direction = "y",
                  force = 2,
                  min.segment.length = 0) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_vline(xintercept = 0, color = "grey40") +
  scale_color_manual(values = cbPalette) +
  scale_shape_manual(values = Species_shapes) +
  xlim(-4,4) +
  ylim(-3,3) +
  labs(
    x = paste0("PC1 (", var_exp[1], "% variance explained)"),
    y = paste0("PC2 (", var_exp[2], "% variance explained)")
  ) +
  theme_minimal(base_size = 14) +
  mytheme

print(p)
ggsave("PCA_Species.png", p, width = 8, height = 6, dpi = 300)

# 6.4 载荷条形图（优化版：PC1、PC2、PC3）
plot_load_bar <- function(loadings_df, pc = 1) {
  dd <- loadings_df[, pc, drop = FALSE]
  dd$var <- rownames(loadings_df)
  colnames(dd)[1] <- "loading"
  ggplot(dd, aes(x = reorder(var, loading), y = loading)) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = paste0("Loading on PC", pc),
         title = paste0("Variable Loadings on PC", pc, " (Optimized)")) +
    theme_minimal()
}
print(plot_load_bar(loadings_opt, pc = 1))
print(plot_load_bar(loadings_opt, pc = 2))
if (ncol(loadings_opt) >= 3) print(plot_load_bar(loadings_opt, pc = 3))

# =========================
# 7) 导出结果（CSV）
# =========================
# write.csv(variance_raw, "pca_variance_summary_raw.csv", row.names = FALSE)
# write.csv(loadings_raw, "pca_loadings_raw.csv")
# write.csv(scores_raw,   "pca_scores_raw.csv")
# 
# write.csv(variance_opt, "pca_variance_summary_optimized.csv", row.names = FALSE)
# write.csv(loadings_opt, "pca_loadings_optimized.csv")
# write.csv(scores_opt,   "pca_scores_optimized.csv")
# 
# write.csv(data.frame(Weak_Variables = weak_vars), "pca_weak_variables.csv", row.names = FALSE)
# write.csv(data.frame(Strong_Variables = strong_vars), "pca_strong_variables.csv", row.names = FALSE)

cat("[DONE] CSV files written to current working directory.\n")
