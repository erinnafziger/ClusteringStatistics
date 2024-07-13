library(factoextra)
library(ggbiplot)
library(ggplot2)
library(dplyr)

candybars <- read.table("CandyBars (1).txt",sep=',',header=TRUE)
candybars_num <- candybars %>% select(Oz.pkg, Calories, Total.fat.g, Saturated.fat.g, Cholesterol.g, Sodium.mg, 
                                      Carbohydrate.g, Dietary.fiber.g, Sugars.g, Protein.g, Vitamin.A..RDI, 
                                      Vitamin.C..RDI, Calcium..RDI, Iron..RDI)

#Get variance-covariance matrix
covmat <- cov(candybars_num, use = "na.or.complete")
covmat <- round(covmat, 3)
covmat
write.csv(covmat,"covmat.csv")

#Get correlation matrix
corrmat <- cor(candybars_num, use = "na.or.complete")
corrmat <- round(corrmat,3)
corrmat
write.csv(corrmat,"corrmat.csv")


#Colormap on correlations
#install.packages("corrplot") #run first time only
library(corrplot)
corrplot(corrmat,method="color", order="hclust")


# Start JPEG device
jpeg("all_qq_plots.jpg", width = 800, height = 800) # Adjust size as needed

# Set up layout for plots
num_cols <- ncol(candybars_num)
num_rows <- ceiling(sqrt(num_cols))
par(mfrow = c(num_rows, num_rows))

# Loop through each column and plot
for (i in 1:num_cols) {
  column_data <- candybars_num[, i]
  qqnorm(column_data, main = paste("Q-Q Plot for", names(candybars_num)[i]))
  qqline(column_data, col = "red")
}

# Close the JPEG device
dev.off()


#Start New JPEG Device
jpeg("all_hist.jpg", width = 800, height = 800)

# Set up layout for plots
num_cols_hist <- ncol(candybars_num)
num_rows_hist <- ceiling(sqrt(num_cols_hist))
par(mfrow = c(num_rows_hist , num_rows_hist ))

#Loop through each column and plot using same layout as qq plots
for (i in 1:num_cols){
  hist(candybars_num[,i], main = paste(names(candybars_num)[i], " Histogram"))
}
#close the JPEG device
dev.off()

# Graphical Assessment of Multivariate Normality
x <- as.matrix(candybars_num) # n x p numeric matrix
center <- colMeans(x) # centroid
n <- nrow(x); 
p <- ncol(x); 
cov <- cov(x); 
d <- mahalanobis(x,center,cov) # distances 

#Remove Outliers
d <- sort(d)
d_nooutliers <- d[1:67]
d_nooutliers

#QQ Plot with outliers
qqplot(qchisq(ppoints(n),df=p),d,
       main="QQ Plot Assessing Multivariate Normality",
       ylab="Mahalanobis D2")
abline(a=0,b=1)

#QQ Plot without outliers
qqplot(qchisq(ppoints(n),df=p),d_nooutliers,
       main="QQ Plot Assessing Multivariate Normality (No Outliers)",
       ylab="Mahalanobis D2")
abline(a=0,b=1)

#TRUE forces correlation matrix
pca <- prcomp(candybars_num, scale=TRUE)
eigenvalues <- pca$sdev^2
eigenvalues

#Get variance explained by each component, standard deviation are the square roots of eigenvalues
pca_sum <- summary(pca)


#Scree plot
plot(pca)
fviz_eig(pca)

#Eigenvectors
eigenvectors <- pca$rotation
eigenvectors

#loadings
loadings <- sweep(pca$rotation,2,pca$sdev,`*`)
loadings

#BiPlot
ggbiplot(pca,groups=candybars$Brand)

#FALSE uses covariance matrix
pcacov <- prcomp(candybars_num, scale=FALSE)
eigenvalues <- pcacov$sdev^2
eigenvalues
pcacov_sum <- summary(pcacov)
pcacov_sum
#Scree plot
plot(pcacov)
fviz_eig(pcacov)

#Eigenvectors
eigenvectors <- pcacov$rotation
eigenvectors

#loadings
loadings <- sweep(pcacov$rotation,2,pca$sdev,`*`)
loadings
#BiPlot
ggbiplot(pcacov,groups=candybars$Brand)
