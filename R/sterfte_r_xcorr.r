# histogram metric correlations

chi10k <- as.matrix(read.csv("D:/Trashcan/sterfte_10k_chi_raw.csv", header = TRUE))
euc10k <- as.matrix(read.csv("D:/Trashcan/sterfte_10k_euc_raw.csv", header = TRUE))
cos10k <- as.matrix(read.csv("D:/Trashcan/sterfte_10k_cos_raw.csv", header = TRUE))
hel10k <- as.matrix(read.csv("D:/Trashcan/sterfte_10k_hel_raw.csv", header = TRUE))
jsd10k <- as.matrix(read.csv("D:/Trashcan/sterfte_10k_jsd_raw.csv", header = TRUE))

chiSMR <- as.matrix(read.csv("D:/Trashcan/sterfte_SMR_chi_raw.csv", header = TRUE))
eucSMR <- as.matrix(read.csv("D:/Trashcan/sterfte_SMR_euc_raw.csv", header = TRUE))
cosSMR <- as.matrix(read.csv("D:/Trashcan/sterfte_SMR_cos_raw.csv", header = TRUE))
helSMR <- as.matrix(read.csv("D:/Trashcan/sterfte_SMR_hel_raw.csv", header = TRUE))
jsdSMR <- as.matrix(read.csv("D:/Trashcan/sterfte_SMR_jsd_raw.csv", header = TRUE))

corr <- function(M1, M2) {
  spear <- rep(0,nrow(M1))
  for (i in 1:nrow(M1)) {
    spear[i] <- cor(M1[i,],M2[i,],method="spearman")
  }
  return(spear)
}

chiC <- corr(chi10k, chiSMR)
eucC <- corr(euc10k, eucSMR)
cosC <- corr(cos10k, cosSMR)
helC <- corr(hel10k, helSMR)
jsdC <- corr(jsd10k, jsdSMR)

breaks <- seq(-1, 1, by = 0.05)

bluey <- rgb(0, 255, 255, maxColorValue = 255, alpha = 150)
orangey<-rgb(255, 140, 0, maxColorValue = 255, alpha = 150)

hist(chiC, xlim = c(-1, 1), breaks = breaks, col = "lightblue", main = "Chi-square")
hist(eucC, xlim = c(-1, 1), breaks = breaks, col = "lightblue", main = "Euclidean")
hist(cosC, xlim = c(-1, 1), breaks = breaks, col = "lightblue", main = "Cosine")
hist(helC, xlim = c(-1, 1), breaks = breaks, col = "lightblue", main = "Hellinger")
hist(jsdC, xlim = c(-1, 1), breaks = breaks, col = "lightblue", main = "Jensen-Shannon")


