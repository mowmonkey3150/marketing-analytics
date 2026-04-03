# ==============================
# ブランドポジショニングの主成分分析
# 教材：Rによる実践的マーケティングリサーチと分析
# ==============================


prst1 <- read.csv("https://goo.gl/z5P8ce")
summary(prst1)
str(prst1)

sc <- prst1
sc[, 1:9] <- data.frame(scale(prst1[, 1:9]))
summary(sc)

library(corrplot)
corrplot(cor(sc[, 1:9]), order = "hclust")

prst1.mean <- aggregate(. ~ Brand, data = sc, mean)
prst1.mean

prst1.mean.matrix <- as.matrix(prst1.mean[, -1])  # 1列目を除外して格納
rownames(prst1.mean.matrix) <- prst1.mean[, 1] # 残しておいたものを行名に

library(gplots)
library(RColorBrewer)
heatmap.2(prst1.mean.matrix,
          col=brewer.pal(9, "GnBu"),　 #緑から青への9段階のグラデーション
          trace="none",   #トレースラインは非表示
          key=TRUE,    #色と数値の対応を示す凡例を非表示
          dend="none",　　#樹形図非表示
          main="\n\n\n\nBrand attributes")　　#タイトル、\nは改行

prst.pc <- prcomp(sc[, 1:9])
summary(prst.pc)
plot(prst.pc, type="l")
biplot(prst.pc)

prst1.mean
prst.mu.pc <- prcomp(prst1.mean[, -1], scale = TRUE)  #1列除いて分析
summary(prst.mu.pc)
biplot(prst.mu.pc,
       main="Brand potitioning",  #タイトル
       cex=c(1.5,1))  #(ブランド名のサイズ、属性名のサイズ)

rownames(prst1.mean) <- prst1.mean[, 1]   # 1列目を行名に設定
prst.mu.pc <- prcomp(prst1.mean[, -1], scale = TRUE)  # 1列目を除いて再実行

biplot(prst.mu.pc,
       main = "Brand positioning",
       cex  = c(1.5, 1))

biplot(prst.mu.pc,
       main   = "Brand positioning",
       cex    = c(1.5, 0.7),    # 属性ラベルを小さく
       col    = c("black", "blue"),
       xlim   = c(-0.8, 0.8),   # 横軸を広げる
       ylim   = c(-0.8, 0.8),
       expand = 1.3)   # 縦軸を広げる

prst.mu.pc$rotation[, 1:2] # 矢印の先端座標を確認,全行2列目まで(pc2まで)
prst1.mean["Papa", -1] - prst1.mean["Romeo", -1] #一列除いて平均差を算出
colMeans(prst1.mean[c("Papa","Romeo"),-1]) - prst1.mean["Papa",-1]
colMeans(prst1.mean[c("Papa","Sierra"),-1]) - prst1.mean["Papa",-1]

biplot(prst.mu.pc,
       choices = c(2,3),
       main    = "Brand positioning PC2 vs PC3",
       cex     = c(1,1),
       xlim    = c(-1,1),
       ylim    = c(-1,1)
)
prst.mu.pc$rotation[,2:3]       
prst1.mean["Romeo",-1]
