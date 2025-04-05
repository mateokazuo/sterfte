# make similarity matrices

library(tidyr)

meta <- read.csv("D:/Trashcan/sterfte_meta.csv")
data <- read.csv("D:/Trashcan/sterfte_SMR.csv",sep=";")
data <- data[,c("RegioS", "Doodsoorzaak", "StandardizedMortalityRatioSMR_2")]
data <- pivot_wider(data,names_from="Doodsoorzaak",values_from="StandardizedMortalityRatioSMR_2")

data <- merge(meta[,c("index","code","name")],data, by.x="code", by.y="RegioS")
data <- data[order(data$index),]

deaths <- as.matrix(data[,4:8])
deaths <- apply(deaths, 2, function(x) as.numeric(trimws(x)))
N <- nrow(deaths) # number municipalities
chiMatrix <- matrix(0, nrow=N, ncol=N)
eucMatrix <- matrix(0, nrow=N, ncol=N)
cosMatrix <- matrix(0, nrow=N, ncol=N)
helMatrix <- matrix(0, nrow=N, ncol=N)
jsdMatrix <- matrix(0, nrow=N, ncol=N)

mahalanobis_dist <- function(x, y, full_cov_matrix) {
  valid_idx <- !is.na(x) & !is.na(y)
  x <- x[valid_idx]
  y <- y[valid_idx]
  
  if (length(x) == 0) return(NA)  # Avoid errors
  
  # Extract covariance submatrix for available causes
  cov_sub <- full_cov_matrix[valid_idx, valid_idx]
  
  # Compute Mahalanobis distance
  diff <- x - y
  return(sqrt(t(diff) %*% solve(cov_sub) %*% diff))
}
entropy <- function(x) {
  x <- x / sum(x)
  x <- x[x > 0]
  return(-sum(x * log(x)))
}
kl_divergence <- function(P, Q) {
  
  P <- P[P > 0]  # Remove zero values to avoid log(0)
  Q <- Q[Q > 0]  # Remove zero values to avoid log(0)
  
  return(sum(P * log(P / Q), na.rm = TRUE))
}

js_divergence <- function(P, Q) {
  P <- P / sum(P)
  Q <- Q / sum(Q)
  M <- (P + Q) / 2

  jsd <- 0.5 * (kl_divergence(P, M) + kl_divergence(Q, M))
  return(jsd)
}
euc <- function(x, y) {
  euclidean_dist <- sqrt(sum((x - y)^2))
  return(-euclidean_dist)
}
jsd <- function(P, Q) {
  jsd_value <- js_divergence(P, Q)
  return(-jsd_value)
}
hellinger_dist <- function(x, y) {
  x <- x / sum(x)  # Normalize to probability distributions
  y <- y / sum(y)
  return(sqrt(sum((sqrt(x) - sqrt(y))^2)) / sqrt(2))
}
hel <- function(x, y) {
  x <- x / sum(x)
  y <- y / sum(y)
  
  h_dist <- sqrt(0.5 * sum((sqrt(x) - sqrt(y))^2))
  return(-h_dist)
}
cosine_sim <- function(x, y) {
  return(sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))))
}


full_cov_matrix <- cov(deaths, use = "pairwise.complete.obs")
deaths[is.na(deaths)] <- 0
for (i in 1:N) {
  for (j in 1:N) {
    pair <- rbind(deaths[i,], deaths[j,])
    
    # Chi-square p-value
    chi <- chisq.test(pair)
    chiMatrix[i,j] <- chi$p.value
    
    #euc <- sqrt(sum((pair[1]-pair[2])^2))
    eucMatrix[i,j] <-  euc(pair[1,], pair[2,])
    
    # Cosine Similarity
    cosMatrix[i,j] <- cosine_sim(pair[1,], pair[2,])
    
    # Hellinger Distance
    helMatrix[i,j] <- hel(pair[1,], pair[2,])
    
    # Entropy Difference
    jsdMatrix[i,j] <- jsd(pair[1,], pair[2,])
  }
}

chiMatrix[is.na(chiMatrix)] <- 0
eucMatrix[is.na(eucMatrix)] <- 0
cosMatrix[is.na(cosMatrix)] <- 0
helMatrix[is.na(helMatrix)] <- 0
jsdMatrix[is.na(jsdMatrix)] <- 0


write.csv(chiMatrix,"D:/Trashcan/sterfte_SMR_chi_raw.csv",row.names = FALSE, col.names = FALSE)
write.csv(eucMatrix,"D:/Trashcan/sterfte_SMR_euc_raw.csv",row.names = FALSE, col.names = FALSE)
write.csv(cosMatrix,"D:/Trashcan/sterfte_SMR_cos_raw.csv",row.names = FALSE, col.names = FALSE)
write.csv(helMatrix,"D:/Trashcan/sterfte_SMR_hel_raw.csv",row.names = FALSE, col.names = FALSE)
write.csv(jsdMatrix,"D:/Trashcan/sterfte_SMR_jsd_raw.csv",row.names = FALSE, col.names = FALSE)