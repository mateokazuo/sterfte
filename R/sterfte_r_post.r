# find optimal scalings

chi <- read.csv("D:/Trashcan/sterfte_SMR_chi_raw.csv", header = TRUE)
chiMatrix <- as.matrix(chi)
euc <- read.csv("D:/Trashcan/sterfte_SMR_euc_raw.csv", header = TRUE)
eucMatrix <- as.matrix(euc)
cos <- read.csv("D:/Trashcan/sterfte_SMR_cos_raw.csv", header = TRUE)
cosMatrix <- as.matrix(cos)
hel <- read.csv("D:/Trashcan/sterfte_SMR_hel_raw.csv", header = TRUE)
helMatrix <- as.matrix(hel)
jsd <- read.csv("D:/Trashcan/sterfte_SMR_jsd_raw.csv", header = TRUE)
jsdMatrix <- as.matrix(jsd)

hist(chiMatrix[109,])
hist(eucMatrix[109,])
hist(cosMatrix[109,])
hist(helMatrix[109,])
hist(jsdMatrix[109,])

linearizer <- function(vec, a=0,b=1) {
  # linear map vector to [a,b]
  M = max(vec)
  m = min(vec)
  if (M==m) {return(rep(b,length(vec)))}
  return(a + (vec-m) * (b-a) / (M-m))
}

mat <- apply(jsdMatrix, 1, linearizer)
sVar <- function(pow) {
  mv <- mean(apply(mat^pow, 1, var))
  return(-mv)
}
optimize(sVar, c(500,1000))
