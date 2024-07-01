#Sub Task 1

# Load libraries
library("readxl")
library("dplyr")

# Read data from the file
Whitewine_v6data <- read_excel("Whitewine_v6.xlsx")
cat("Data loaded from Excel file successfully. Dimensions: ", dim(Whitewine_v6data), "\n")



# Calculate the total number of missing values in the dataset
missing_values <- sum(is.na(Whitewine_v6data))

# Check if there are any missing values and if there missing values, remove rows with missing values 
if (missing_values > 0) {
  Whitewine_v6data <- Whitewine_v6data[complete.cases(Whitewine_v6data), ]  # Remove rows with missing values
  cat("Missing values found and rows with missing values removed.","\n")
} else {
  cat("No missing values found. No rows removed.","\n" )
}

# Confirm that all missing values have been removed
missing_values_after <- sum(is.na(Whitewine_v6data))
cat("Total number of missing values after removal: ", missing_values_after, "\n")



par(mar=c(2, 2, 2, 2))
boxplot(Whitewine_v6data)



cat("Starting outlier removel process...","/n")
#define outlier removal function
remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  lower_bound <- qnt[1] - 1.5 * iqr
  upper_bound <- qnt[2] + 1.5 * iqr
  x_clipped <- pmax(pmin(x, upper_bound), lower_bound)
  return(x_clipped)
}

# Perform outlier removal
wine_data_clean <- as.data.frame(lapply(Whitewine_v6data[, 1:11], remove_outliers))
cat("Successfully Outlier removal.","\n")


# Perform scaling on cleaned data
cat("Scaling data...","\n")
scaled_data <- scale(wine_data_clean)
cat("Data scaling completed.","\n")


library("ggplot2")
library("reshape2")
# Melting the data frames into long format
original_long <- melt(wine_data_clean, variable.name = "Variable", value.name = "Value")
original_long$Type <- "Original"

scaled_long <- melt(as.data.frame(scaled_data), variable.name = "Variable", value.name = "Value")
scaled_long$Type <- "Scaled"
# Melting the data frames into long format
original_long <- melt(wine_data_clean, variable.name = "Variable", value.name = "Value")
original_long$Type <- "Original"

scaled_long <- melt(as.data.frame(scaled_data), variable.name = "Variable", value.name = "Value")
scaled_long$Type <- "Scaled"
# Combining the data
combined_data <- rbind(original_long, scaled_long)
# Plotting the data
ggplot(combined_data, aes(x = Value, fill = Type)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  facet_wrap(~ Type, scales = "free_x") +
  labs(title = "Comparison of Original and Scaled Data",
       x = "Value", y = "Count") +
  theme_minimal() +
  theme(legend.position="none")


# Visualization Boxplot for each attribute before outlier removal

par(mfrow=c(3,4))
for (i in 1:11) {
  boxplot(Whitewine_v6data[, i], main = names(Whitewine_v6data)[i])
}

# Visualization Boxplot for each attribute after outlier removal

par(mfrow=c(3,4))
for (i in 1:11) {
  boxplot(scaled_data[, i], main = paste("Cleaned", names(Whitewine_v6data)[i]))
}

# Summary statistics before and after outlier removal
#before
summary_before <- apply(Whitewine_v6data[, 1:11], 2, summary)
cat("Summary statistics before outlier removal:","\n")
print(summary_before)

#after
summary_after <- apply(wine_data_clean, 2, summary)
cat("\nSummary statistics after outlier removal:","\n")
print(summary_after)



library(NbClust)
cat("Performing cluster analysis using NbClust...","\n")

set.seed(200)  # for reproducibility
nbclust_results <- NbClust(scaled_data, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")

# Visualize the NbClust result
barplot(table(nbclust_results$Best.n[1,]), 
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "NbClust Results", col = "lightgreen")

cat("NbClust analysis is successful.","\n")



library(factoextra)
cat("Performing cluster analysis using Elobow Method...","\n")
set.seed(200) # for reproducibility
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 3, color = "red") +
  labs(title = "Elbow Method")
cat("Elbow analysis is successful.","\n")



library(cluster)
cat("Performing cluster analysis using gap statistics Method...","\n")
set.seed(200) # for reproducibility
# Calculating the gap statistic
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = 15, B =500)
# Visualizing the gap statistics
library(factoextra)
fviz_gap_stat(gap_stat)
cat("Gap statistics analysis is successful.","\n")



library(factoextra)
cat("Performing cluster analysis using silhouette Method...","\n")
set.seed(200) # for reproducibility
# Calculating the best number of clusters using silhouette method
silhouette_results <- fviz_nbclust(scaled_data, kmeans, method = "silhouette")
# Plotting the silhouette analysis
print(silhouette_results)
cat("Silhouette analysis is successful.","\n")



# Performing k-means with k=2
set.seed(200)  #for reproducibility
kmeans_2 <- kmeans(scaled_data, centers = 2, nstart = 50)

# Function to display results
display_kmeans_results <- function(km_result) {
  
  total_ss <- sum((scaled_data - colMeans(scaled_data))^2)
  bss <- sum(km_result$betweenss)
  tss <- total_ss
  bss_tss_ratio <- bss / tss
  
  cat("Total SS: ", total_ss, "\n")
  cat("BSS/TSS Ratio: ", bss_tss_ratio, "\n")
  cat("WSS (Within-cluster sum of squares): ", km_result$tot.withinss, "\n\n")
  
}

# Displaying results for k = 2
cat("Results for k=2:","\n")
display_kmeans_results(kmeans_2)

library(factoextra)

# Visualizing clusters
fviz_cluster(list(data = scaled_data, cluster = kmeans_2$cluster), geom = "point", main = "k=2")

library(cluster)
library(factoextra)

# Compute silhouette information
sil_widths <- silhouette(kmeans_2$cluster, dist(scaled_data))

# Visualize the silhouette plot
silhouette_plot <- fviz_silhouette(sil_widths)
print(silhouette_plot)

# Calculate the average silhouette width
average_sil_width <- mean(sil_widths[, "sil_width"])
cat("Average silhouette width:", average_sil_width, "\n")



#Sub Task 2 


# Load libraries
library("factoextra")
library("cluster")

# Apply PCA to the scaled data
pca_result <- prcomp(scaled_data, scale. = TRUE)
pca_summary <- summary(pca_result)
print(pca_summary)

# Extract eigenvalues
eigenvalues <- pca_result$sdev^2
cat("Eigenvalues:","\n")
print(eigenvalues)

#Extract eigenvectors
eigenvectors <- pca_result$rotation
cat("Eigenvectors:","\n")
print(eigenvectors)

#plot of scree plot
plot(pca_result,type="lines",main="Scree Plot")



# Calculate cumulative score per principal component (PC)
cumulative_var <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
cat("Cumulative Variance Explained:\n")
print(cumulative_var)



# Create a new dataset with principal components as attributes
pc_data <- predict(pca_result)

# Choose principal components providing at least cumulative score > 85%
num_components <- which(cumulative_var > 0.85)[1]
cat("Number of components selected (85% variance):", num_components, "\n")

# Select the first 'num_components' principal components
pc_data_selected <- pc_data[, 1:num_components]
cat("Selected Principal Component Data:\n")
print(pc_data_selected)

# Determine the number of clusters for k-means on transformed dataset
fviz_eig(pca_result,addlabels = TRUE,main="Scree Plot")



# NBclust
set.seed(200)
nbclust_results_pca <- NbClust(pc_data_selected, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")
cat("NbClust on PCA data suggests", which.max(table(nbclust_results_pca$Best.n[1,])),"clusters.\n")

barplot(table(nbclust_results_pca$Best.n[1,]), 
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "NbClust Results after PCA", col = "lightgreen")

cat("NbClust analysis completed.\n")



# Elbow method
set.seed(200)
elbow_results_pca <- fviz_nbclust(pc_data_selected, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2, color = "red") +
  labs(title = "Elbow Method")
print(elbow_results_pca)



# Gap statistics
set.seed(200)
gap_stat_pca <- clusGap(pc_data_selected, FUN = kmeans, nstart = 25, K.max = 15, B = 500)
print(gap_stat_pca)
fviz_gap_stat(gap_stat_pca)



# Silhouette method
set.seed(200)
silhouette_results_pca <- fviz_nbclust(pc_data_selected, kmeans, method = "silhouette")
print(silhouette_results_pca)



# Perform k-means clustering
set.seed(200)
cat("K-means Cluster Centers:","\n")
kmeans_pca_2 <- kmeans(pc_data_selected, centers = 2, nstart = 50)

# Display k-means results
print(kmeans_pca_2$centers)
total_ss_pca <- sum((pc_data_selected - colMeans(pc_data_selected))^2)
bss_pca <- sum(kmeans_pca_2$betweenss)
tss_pca <- total_ss_pca
bss_tss_ratio_pca <- bss_pca / tss_pca

cat("Total Sum of Squares (TSS):", total_ss_pca, "\n")
cat("Between-cluster Sum of Squares (BSS)/TSS Ratio:", bss_tss_ratio_pca, "\n")
cat("Within-cluster Sum of Squares (WSS):", kmeans_pca_2$tot.withinss, "\n\n")

# Visualize clusters
fviz_cluster(list(data = pc_data_selected, cluster = kmeans_pca_2$cluster), geom = "point", main = paste("k =", 2))

# Calculate silhouette information
sil_widths_pca_2 <- silhouette(kmeans_pca_2$cluster, dist(pc_data_selected))

# Visualize the silhouette plot
silhouette_plot_pca_2 <- fviz_silhouette(sil_widths_pca_2)
print(silhouette_plot_pca_2)

# Calculate average silhouette width
average_sil_width_pca_2 <- mean(sil_widths_pca_2[, "sil_width"])
cat("Average silhouette width:", average_sil_width_pca_2, "\n")



library(fpc) # for calinhara function

# Function to visualize Caliński-Harabasz Index
fviz_ch <- function(pc_data_selected, max_clusters = 10) {
  ch_scores <- numeric(max_clusters)
  for (i in 2:max_clusters) {
    km <- kmeans(pc_data_selected, centers = i, nstart = 25)
    ch_scores[i] <- calinhara(pc_data_selected, km$cluster, cn = max(km$cluster))
  }
  ch_scores <- ch_scores[2:max_clusters]
  k <- 2:max_clusters
  
  par(mar = c(5, 4, 4, 4) + 0.1) 
  plot(k, ch_scores, xlab = "Cluster number k",
       ylab = "Caliński - Harabasz Score",
       main = "Caliński - Harabasz Plot", cex.main = 1,
       col = "dodgerblue1", cex = 0.9,
       lty = 1, type = "o", lwd = 1, pch = 4,
       bty = "l", las = 1, cex.axis = 0.8, tcl = -0.2)
  abline(v = which.max(ch_scores) + 1, lwd = 1, col = "red", lty = "dashed")
}

# Use this function on the PCA-selected data
fviz_ch(pc_data_selected)


