## --------------------------------------------------------------------------------------
prst1 <- read.csv("https://goo.gl/z5P8ce")　#データの読み込み
summary(prst1)　#記述統計
str(prst1)　#データの構造を確認

sc <- prst1
sc[, 1:9] <- data.frame(scale(prst1[, 1:9]))　#1~9列目を標準化
summary(sc)


## --------------------------------------------------------------------------------------
library(corrplot)
corrplot(cor(sc[, 1:9]), 
         order = "hclust")　#似た変数同士をまとめて表示


## --------------------------------------------------------------------------------------
library(gplots)
library(RColorBrewer)

prst1.mean <- aggregate(. ~ Brand, data = sc,
                        mean)　#ブランドごとに平均を算出
prst1.mean.matrix <- as.matrix(prst1.mean[, -1])　#1列目を除いて行列変換

rownames(prst1.mean.matrix) <- prst1.mean[, 1]　#1列目のブランドを行名に
heatmap.2(prst1.mean.matrix,　#ヒートマップは行列を受け取る
          col=brewer.pal(9, "GnBu"),　#緑から青への9段階で表現
          trace="none",　#トレースラインは非表示
          key=TRUE,　#色と数値の対応を表示
          dend="none",　#樹形図の非表示
          main="\n\n\nBrand attributes") #/nは改行


## --------------------------------------------------------------------------------------
prst.pc <- prcomp(sc[, 1:9])　#1~9列目を指定して分析
summary(prst.pc)
plot(prst.pc, type="l")　#折れ線グラフで固有値をプロット


## --------------------------------------------------------------------------------------
rownames(prst1.mean) <- prst1.mean[, 1]　#ブランド名を行名に
prst.mu.pc <- prcomp(prst1.mean[, -1], scale = TRUE)　#１列除いて主成分分析
biplot(prst.mu.pc,
       main = "Brand positioning",　#タイトル
       cex  = c(1.0, 0.7),　#文字の大きさ
       col  = c("black", "blue"),　#色の指定
       xlim = c(-0.8, 0.8),　#軸の調整
       ylim = c(-0.8, 0.8)
       )　#expandで矢印の長さを変えられる


## --------------------------------------------------------------------------------------
prst.mu.pc$rotation[, 1:2]


## --------------------------------------------------------------------------------------
prst1.mean["Papa", -1] - prst1.mean["Romeo", -1]
colMeans(prst1.mean[c("Papa","Romeo"),-1]) - prst1.mean["Papa",-1]
colMeans(prst1.mean[c("Papa","Sierra"),-1]) - prst1.mean["Papa",-1]


## --------------------------------------------------------------------------------------
biplot(prst.mu.pc,
       choices = c(2,3),
       main    = "Brand positioning PC2 vs PC3",
       cex     = c(1,0.7),
       xlim    = c(-0.9,0.9),
       ylim    = c(-0.9,0.9))
prst.mu.pc$rotation[,2:3]


