library(dplyr)
library(readr)   # For reading CSV files
library(spdep)
library(ggplot2)


# Read the CSV files
z_Euclidean <- read.csv("Euclidean.csv", stringsAsFactors = FALSE)
z_Manhattan <- read.csv("Manhattan.csv", stringsAsFactors = FALSE)
z_Minkowski <- read.csv("Minkowski.csv", stringsAsFactors = FALSE)
z_Canberra <- read.csv("Canberra.csv", stringsAsFactors = FALSE)
z_Chebyshev <- read.csv("Chebyshev.csv", stringsAsFactors = FALSE)
z_Optimized <- read.csv("gemeenten_Optimized.csv",stringsAsFactors = FALSE)

z_YPLLEuclidean <- read.csv("YEuclidean.csv", stringsAsFactors = FALSE)
z_YPLLManhattan <- read.csv("YManhattan.csv", stringsAsFactors = FALSE)
z_YPLLMinkowski <- read.csv("YMinkowski.csv", stringsAsFactors = FALSE)
z_YPLLCanberra <- read.csv("YCanberra.csv", stringsAsFactors = FALSE)
z_YPLLChebyshev <- read.csv("YChebyshev.csv", stringsAsFactors = FALSE)
z_YPLLOptimized <- read.csv("OptimizedYPLL.csv", stringsAsFactors = FALSE)

colnames(z_Euclidean) <- c("code", "ZEuclidean")
colnames(z_Manhattan) <- c("code", "ZManhattan")
colnames(z_Minkowski) <- c("code", "ZMinkowski")
colnames(z_Canberra) <- c("code", "ZCanberra")
colnames(z_Chebyshev) <- c("code", "ZChebyshev")
colnames(z_Optimized) <- c("code", "ZOptimized")

colnames(z_YPLLEuclidean) <- c("code", "ZEuclideanYPLL")
colnames(z_YPLLManhattan) <- c("code", "ZManhattanYPLL")
colnames(z_YPLLMinkowski) <- c("code", "ZMinkowskiYPLL")
colnames(z_YPLLCanberra) <- c("code", "ZCanberraYPLL")
colnames(z_YPLLChebyshev) <- c("code", "ZChebyshevYPLL")
colnames(z_YPLLOptimized) <- c("code", "ZOptimizedYPLL")

# Merge data frames
merged_data1 <- z_Euclidean %>%
  full_join(z_Manhattan, by = "code") %>%
  full_join(z_Minkowski, by = "code") %>%
  full_join(z_Canberra, by = "code") %>%
  full_join(z_Chebyshev, by = "code") %>%
  full_join(z_Optimized, by = "code")

# Merge data frames
merged_data2 <- z_YPLLEuclidean %>%
  full_join(z_YPLLManhattan, by = "code") %>%
  full_join(z_YPLLMinkowski, by = "code") %>%
  full_join(z_YPLLCanberra, by = "code") %>%
  full_join(z_YPLLChebyshev, by = "code") %>%
  full_join(z_YPLLOptimized, by = "code")

# Remove rows with NA
merged_data1 <- na.omit(merged_data1)
merged_data2 <- na.omit(merged_data2)


cor_matrix <- cor(merged_data1[, c("ZEuclidean", "ZManhattan", "ZMinkowski", "ZCanberra", "ZChebyshev", "ZOptimized")], method = "pearson")
print("Correlation Matrix:")
print(cor_matrix)

cor_matrix <- cor(merged_data2[, c("ZEuclideanYPLL", "ZManhattanYPLL", "ZMinkowskiYPLL", "ZCanberraYPLL", "ZChebyshevYPLL", "ZOptimizedYPLL")], method = "pearson")
print("Correlation Matrix:")
print(cor_matrix)

ggplot(merged_data1, aes(x = ZCanberra, y = ZOptimized, label = code)) +
  geom_point(color = "blue", size = 2) +
  geom_text(vjust = 1.5, hjust = 1.5, size = 3, check_overlap = TRUE) +  # Add labels
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Z-Score (Canberra)", y = "Z-Score (Optimized)",
       title = "Comparison of Z-Scores: Canberra vs Optimized") +
  theme_minimal()