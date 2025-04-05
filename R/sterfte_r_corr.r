# histogram measure correlations

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

chieuc10k <- corr(chi10k, euc10k)
chicos10k <- corr(chi10k, cos10k)
chihel10k <- corr(chi10k, hel10k)
chijsd10k <- corr(chi10k, jsd10k)

euccos10k <- corr(euc10k, cos10k)
euchel10k <- corr(euc10k, hel10k)
eucjsd10k <- corr(euc10k, jsd10k)

coshel10k <- corr(cos10k, hel10k)
cosjsd10k <- corr(cos10k, jsd10k)

heljsd10k <- corr(hel10k, jsd10k)

chieucSMR <- corr(chiSMR, eucSMR)
chicosSMR <- corr(chiSMR, cosSMR)
chihelSMR <- corr(chiSMR, helSMR)
chijsdSMR <- corr(chiSMR, jsdSMR)

euccosSMR <- corr(eucSMR, cosSMR)
euchelSMR <- corr(eucSMR, helSMR)
eucjsdSMR <- corr(eucSMR, jsdSMR)

coshelSMR <- corr(cosSMR, helSMR)
cosjsdSMR <- corr(cosSMR, jsdSMR)

heljsdSMR <- corr(helSMR, jsdSMR)


breaks <- seq(-1, 1, by = 0.05)

bluey <- rgb(0, 255, 255, maxColorValue = 255, alpha = 150)
orangey<-rgb(255, 140, 0, maxColorValue = 255, alpha = 150)

hist(chieuc10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Chi-square vs Euclidean")
hist(chieucSMR, breaks = breaks, col = orangey, add = TRUE)

hist(chicos10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Chi-square vs Cosine")
hist(chicosSMR, breaks = breaks, col = orangey, add = TRUE)

hist(chihel10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Chi-square vs Hellinger")
hist(chihelSMR, breaks = breaks, col = orangey, add = TRUE)

hist(chijsd10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Chi-square vs Jensen-Shannon")
hist(chijsdSMR, breaks = breaks, col = orangey, add = TRUE)

hist(euccos10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Euclidean vs Cosine")
hist(euccosSMR, breaks = breaks, col = orangey, add = TRUE)

hist(euchel10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Euclidean vs Hellinger")
hist(euchelSMR, breaks = breaks, col = orangey, add = TRUE)

hist(eucjsd10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Euclidean vs Jensen-Shannon")
hist(eucjsdSMR, breaks = breaks, col = orangey, add = TRUE)

hist(coshel10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Cosine vs Hellinger")
hist(coshelSMR, breaks = breaks, col = orangey, add = TRUE)

hist(cosjsd10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Cosine vs Jensen-Shannon")
hist(cosjsdSMR, breaks = breaks, col = orangey, add = TRUE)

hist(heljsd10k, xlim = c(-1, 1), breaks = breaks, col = bluey, main = "Hellinger vs Jensen-Shannon")
hist(heljsdSMR, breaks = breaks, col = orangey, add = TRUE)
