# =========================================================
# PCA for a Target Species (default: ST) — Optimized Version
# =========================================================

# ---- 0) 基础设置与依赖 ----
rm(list = ls())

# 必要包
pkgs <- c("dplyr","ggplot2","tidyr","readr","ggrepel",
          "scales","corrplot","stringr","purrr","tibble")
to_install <- pkgs[!pkgs %in% installed.packages()[,1]]
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 0.1 可配置参数 ----
path_csv      <- "../Data/PlantData/newdat.csv"  # 数据路径
target_species <- "ST"                           # 目标物种
weak_corr_thr  <- 0.25                           # 弱相关阈值(平均绝对相关度) 用于剔除
high_corr_thr  <- 0.90                           # 高共线阈值(两两相关) 用于精修
log1p_regex    <- "(biomass|diameter|conductivity|density)" # 候选对数转换
export_prefix  <- paste0("PCA_", target_species) # 导出前缀
set.seed(1)

# ---- 0.2 变量重命名映射（可选） ----
name_map <- c(
  "Height_mean"   = "Mean plant height",
  "Diameter_mean" = "Mean stem diameter",
  "Biomass"       = "Aboveground biomass",
  "Mature_rate"   = "Seed rate",
  "Elevation"     = "Elevation",
  "PH"            = "Soil pH",
  "Cond"          = "Electrical conductivity",
  "WC"            = "Soil moisture content",
  "Pwc"           = "Leaf relative water content",
  "Den1"          = "Density"
)

# =========================================================
# 1) 工具函数
# =========================================================

# 1.1 迭代去高共线：当 |r|>thr 时，删除两者中“与其他变量平均相关度更高”的那个
remove_high_corr <- function(df, thr = 0.9) {
  df <- as.data.frame(df)
  repeat {
    C <- suppressWarnings(cor(df, use = "pairwise.complete.obs"))
    diag(C) <- 0
    max_corr <- max(abs(C), na.rm = TRUE)
    if (!is.finite(max_corr) || max_corr <= thr) break
    ij <- which(abs(C) == max_corr, arr.ind = TRUE)[1,]
    i <- ij[1]; j <- ij[2]
    mi <- mean(abs(C[i, ]), na.rm = TRUE)
    mj <- mean(abs(C[j, ]), na.rm = TRUE)
    drop_col <- if (mi >= mj) colnames(df)[i] else colnames(df)[j]
    df <- df[, setdiff(names(df), drop_col), drop = FALSE]
  }
  df
}

# 1.2 对数转换：仅对“全正”的候选列做 log1p
log1p_safe <- function(df, regex_pat = "", ignore_case = TRUE) {
  if (regex_pat == "") return(df)
  targ <- grep(regex_pat, names(df), value = TRUE, ignore.case = ignore_case)
  if (!length(targ)) return(df)
  pos  <- targ[sapply(targ, function(v) is.numeric(df[[v]]) && all(df[[v]] > 0, na.rm = TRUE))]
  if (length(pos)) df[pos] <- lapply(df[pos], log1p)
  attr(df, "logged_cols") <- pos
  df
}

# 1.3 载荷条形图
plot_load_bar <- function(loadings_df, pc = 1, title_suffix = "Optimized") {
  dd <- loadings_df[, pc, drop = FALSE]
  dd$var <- rownames(loadings_df)
  colnames(dd)[1] <- "loading"
  ggplot(dd, aes(x = reorder(var, loading), y = loading)) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = paste0("Loading on PC", pc),
         title = paste0("Variable Loadings on PC", pc, " (", title_suffix, ")")) +
    theme_minimal()
}

# =========================================================
# 2) 读取数据与清洗
# =========================================================
dat <- read.csv(path_csv, check.names = FALSE, stringsAsFactors = FALSE)
if (!"Species" %in% names(dat)) dat$Species <- "GroupA"

# 应用重命名（仅对存在的列）
present <- intersect(names(name_map), names(dat))
dat_renamed <- dat
names(dat_renamed)[match(present, names(dat_renamed))] <- name_map[present]

dat_renamed <- dat_renamed %>%
  select(-any_of(c("Density_1", "Density_2")))

# 仅保留目标物种 + 数值列
dat_sp <- dat_renamed %>% filter(Species == target_species)
X_raw  <- dat_sp %>% select(where(is.numeric)) %>% drop_na()

cat(sprintf("[INFO] Species=%s, N obs=%d, N numeric vars=%d\n",
            target_species, nrow(X_raw), ncol(X_raw)))

stopifnot(nrow(X_raw) > 2, ncol(X_raw) > 1)

# =========================================================
# 3) 基准 PCA（全变量）
# =========================================================
pca_raw <- prcomp(X_raw, center = TRUE, scale. = TRUE)
eig_raw <- pca_raw$sdev^2
prop_raw <- eig_raw / sum(eig_raw)
cum_raw  <- cumsum(prop_raw)

variance_raw <- tibble(
  PC = paste0("PC", seq_along(prop_raw)),
  `Standard deviation` = pca_raw$sdev,
  `Proportion of Variance` = prop_raw,
  `Cumulative Proportion` = cum_raw
)

# =========================================================
# 4) 变量筛选 + 变换 + 优化 PCA
# =========================================================
# 4.1 剔除“平均绝对相关度”较低的弱相关变量
abs_cor <- abs(cor(X_raw, use = "pairwise.complete.obs"))
mean_abs_cor <- colMeans(abs_cor, na.rm = TRUE)
weak_vars  <- names(mean_abs_cor[mean_abs_cor < weak_corr_thr])
strong_vars <- setdiff(colnames(X_raw), weak_vars)
X_strong <- X_raw[, strong_vars, drop = FALSE]

cat(sprintf("[INFO] 弱相关阈值=%.2f；剔除弱相关变量: %s\n",
            weak_corr_thr, ifelse(length(weak_vars), paste(weak_vars, collapse=", "), "无")))

# 4.2 迭代去高共线
X_nocol <- remove_high_corr(X_strong, thr = high_corr_thr)

# 4.3 对数转换（安全）
X_trans <- log1p_safe(X_nocol, regex_pat = log1p_regex)
logged_cols <- attr(X_trans, "logged_cols"); if (is.null(logged_cols)) logged_cols <- character(0)

# 4.4 优化 PCA
pca_opt <- prcomp(X_trans, center = TRUE, scale. = TRUE)
eig_opt <- pca_opt$sdev^2
prop_opt <- eig_opt / sum(eig_opt)
cum_opt  <- cumsum(prop_opt)

variance_opt <- tibble(
  PC = paste0("PC", seq_along(prop_opt)),
  `Standard deviation` = pca_opt$sdev,
  `Proportion of Variance` = prop_opt,
  `Cumulative Proportion` = cum_opt
)

cat("[RESULT] 优化后前3个PC的方差贡献：\n")
print(variance_opt %>% mutate(across(where(is.numeric), ~round(.x,4))) %>% slice(1:3))

# =========================================================
# 5) 对比表与导出
# =========================================================
n_pc <- min(nrow(variance_raw), nrow(variance_opt))
compare_df <- tibble(
  PC = paste0("PC", seq_len(n_pc)),
  Raw_Proportion       = round(prop_raw[seq_len(n_pc)], 4),
  Raw_Cumulative       = round(cum_raw[seq_len(n_pc)], 4),
  Optimized_Proportion = round(prop_opt[seq_len(n_pc)], 4),
  Optimized_Cumulative = round(cum_opt[seq_len(n_pc)], 4)
)
print(head(compare_df, 6))

# 导出 CSV
# write.csv(variance_raw, paste0(export_prefix, "_variance_raw.csv"), row.names = FALSE)
# write.csv(variance_opt, paste0(export_prefix, "_variance_opt.csv"), row.names = FALSE)
# write.csv(compare_df,   paste0(export_prefix, "_variance_compare.csv"), row.names = FALSE)
# write.csv(data.frame(Removed_weak = weak_vars),
#           paste0(export_prefix, "_removed_weak.csv"), row.names = FALSE)
# write.csv(data.frame(Kept_after_collinearity = names(X_trans)),
#           paste0(export_prefix, "_kept_after_collinearity.csv"), row.names = FALSE)
# write.csv(data.frame(Logged_cols = logged_cols),
#           paste0(export_prefix, "_log1p_cols.csv"), row.names = FALSE)

# =========================================================
# 6) 可视化（实心点风格）
# =========================================================

# 6.1 相关矩阵（原始）
corrplot(abs_cor, method = "color", type = "upper", tl.col = "black",
         tl.srt = 45, title = "Absolute Correlation Matrix (Raw)",
         mar = c(0,0,2,0))

# 6.2 碎石图：原始 vs 优化
df_scree <- bind_rows(
  tibble(PC = seq_along(prop_raw), Prop = prop_raw, Model = "Raw"),
  tibble(PC = seq_along(prop_opt), Prop = prop_opt, Model = "Optimized")
)

ggplot(df_scree, aes(PC, Prop)) +
  geom_line() +
  geom_point(shape = 16, size = 2) +  # 实心点
  scale_x_continuous(breaks = unique(df_scree$PC)) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~Model, ncol = 1) +
  labs(x = "Principal Component",
       y = "Proportion of Variance Explained",
       title = paste0("Scree Plot (", target_species, "): Raw vs Optimized")) +
  theme_minimal()

# 6.3 Biplot（优化 PC1–PC2，实心点）
scores_opt   <- as.data.frame(pca_opt$x)
loadings_opt <- as.data.frame(pca_opt$rotation)
var_exp_lab  <- round(prop_opt[1:2] * 100, 1)

load2 <- loadings_opt[, 1:2, drop = FALSE]
load2$var <- rownames(load2)
arrow_scale <- max(abs(scores_opt$PC1), abs(scores_opt$PC2), na.rm = TRUE)
load2$xend <- load2$PC1 * arrow_scale
load2$yend <- load2$PC2 * arrow_scale

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

ggplot() +
  geom_point(data = scores_opt, aes(PC1, PC2), color = "#B79F00", shape = 7, size = 2.6, alpha = 0.8) + # 实心点
  geom_segment(data = load2, aes(x = 0, y = 0, xend = xend, yend = yend),
               linewidth = 0.7, arrow = arrow(length = unit(0.02, "npc"))) +
  ggrepel::geom_text_repel(data = load2, aes(x = xend, y = yend, label = var),
                           size = 4, segment.color = "grey60") +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 0, color = "grey60") +
  xlim(-5,5) +
  ylim(-3,3) +
  labs(x = paste0("PC1 (", var_exp_lab[1], "% variance explained)"),
       y = paste0("PC2 (", var_exp_lab[2], "% variance explained)")) +
  annotate("text",
           x = max(scores_opt$PC1) * 0.95,
           y = 2.5,
           label = "Scirpus triqueter",
           hjust = 1, vjust = 1,
           size = 5, fontface = "italic",
           color = "black") +
  theme_minimal(base_size = 14) +
  mytheme

ggsave(paste0(export_prefix, ".png"), width = 6, height = 6, dpi = 300)

# 6.4 载荷条形图（PC1/PC2/PC3）
print(plot_load_bar(loadings_opt, pc = 1, title_suffix = "Optimized"))
print(plot_load_bar(loadings_opt, pc = 2, title_suffix = "Optimized"))
if (ncol(loadings_opt) >= 3) print(plot_load_bar(loadings_opt, pc = 3, title_suffix = "Optimized"))

cat("\n[DONE] Outputs saved with prefix: ", export_prefix, "\n")
