library(rospca)
library(ggplot2)
library(gridExtra)

wine_dataset <- data.frame(winequality_red)
features <- wine_dataset[colnames(wine_dataset) != 'quality']

# Test sensitivity to different alpha values
# Using ndir=5k as n>500
alphas <- c(0.6, 0.75, 0.9)
for (alpha in alphas){
  robust_pca <- robpca(scale(features), alpha=alpha, ndir=5000)
  print(robust_pca$loadings)
}

# Loadings are not very sensitive to alpha within this range
# Accept default alpha to balance efficiency and robustness
robust_pca <- robpca(scale(features), alpha=0.75, ndir=5000)
print(robust_pca$loadings)

# Obtain PC scores
robust_pc_scores <- robust_pca$scores
robpca_plt_data <- data.frame(cbind(robust_pc_scores, wine_dataset$quality))
colnames(robpca_plt_data) <- c('PC1', 'PC2', 'PC3', 'PC4', 'PC5', 'Quality')

# Compare to classical PCA
classical_pca <- prcomp(features, rank=5, scale=TRUE)
print(classical_pca$rotation)

classical_pc_scores <- classical_pca$x
plt_data <- data.frame(cbind(classical_pc_scores, wine_dataset$quality))
colnames(plt_data) <- c('PC1', 'PC2', 'PC3', 'PC4', 'PC5', 'Quality')

classical_plot <- ggplot(plt_data, aes(x=PC1, y=PC2, color=Quality)) +
  scale_color_gradient2(low = "darkblue", mid='gold', high = "yellow", 
                        midpoint = median(plt_data$Quality)) +
  geom_point(size=1) + ylab("Classical PC2") + xlab("Classical PC1") + 
  ylim(c(-7.5, 4.5)) + xlim(c(-5.5, 9)) +
  ggtitle("Red Wines by Top Two Classical Princiapl Components") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

robust_plot <- ggplot(robpca_plt_data, aes(x=PC1, y=PC2, color=Quality)) +
  scale_color_gradient2(low = "darkblue", mid='gold', high = "yellow", 
                        midpoint = median(robpca_plt_data$Quality)) +
  geom_point(size=1) + ylab("Robust PC2") + xlab("Robust PC1") + 
  ylim(c(-7.5, 4.5)) + xlim(c(-5.5, 9)) +
  ggtitle("Red Wines by Top Two Robust Princiapl Components") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggsave("robpca.png", robust_plot)
ggsave("classical.png", classical_plot)

sugar <- ggplot(wine_dataset, aes(x=residual.sugar)) + geom_histogram(bins=50) +
  xlab("Residual Sugar") + ylab("Count of Wines") +
  ggtitle("Distribution of Residual Sugar Concentration") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

chlorides <- ggplot(wine_dataset, aes(x=chlorides)) + geom_histogram(bins=50) +
  xlab("Chlorides") + ylab("Count of Wines") +
  ggtitle("Distribution of Chloride Concentration") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

density <- ggplot(wine_dataset, aes(x=density)) + geom_histogram(bins=50) +
  xlab("Density") + ylab("Count of Wines") +
  ggtitle("Distribution of Density") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

alcohol <- ggplot(wine_dataset, aes(x=alcohol)) + geom_histogram(bins=50) +
  xlab("Alcohol") + ylab("Count of Wines") +
  ggtitle("Distribution of Alcohol Content") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

g <- grid.arrange(sugar, 
                  chlorides,
                  density,
                  alcohol,
                  nrow = 2, 
                  widths=c(15,15), 
                  heights=c(5,5))

ggsave("sugar_chlorides_density_alcohol_hist.png", g)
