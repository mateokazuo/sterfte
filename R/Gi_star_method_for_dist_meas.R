library(spdep)
library(dplyr)
library(ggplot2)
library(readr)   # For reading CSV files

# Load the dataset
data <- read_csv("sterfte_meta_amersfoort.csv")
data2 <- read_csv("TestYPLL.csv")
spec(data2)

combined <- inner_join(data, data2, by = "code")  # or whatever the common column is
combined <- na.omit(combined)
combined
coords <- as.matrix(combined[, c("xPos", "yPos")])
values <- combined$YPLL80


dist_matrix <- as.matrix(dist(coords, method = "maximum"))
threshold <- quantile(dist_matrix, 0.2)
binary_weights <- (dist_matrix < threshold) * 1
diag(binary_weights) <- 0
listw <- mat2listw(binary_weights, style = "W", zero.policy = TRUE)

localG(values, listw)
gi_star <- localG(values, listw)
p_values <- 2 * pnorm(-abs(gi_star))

results_df <- data.frame(
  code = combined$code,
  Gi_star = gi_star,
  p_value = p_values
)
write.csv(results_df, "Gi_star_resultsYPLLChebyshev.csv", row.names = FALSE)