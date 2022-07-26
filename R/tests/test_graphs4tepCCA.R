rm(list = ls()) ; gc() ; graphics.off()

library(R4SPISE2022) # Our package
library(TExPosition)
library(data4PCCAR)

set.seed(57115)
n <- 57
p1 <- 11
p2 <- 5
X1 <- matrix(rnorm(n * p1), n, p1)
X2 <- matrix(rnorm(n * p2), n, p2)
DATA1 <- ExPosition::expo.scale(X1, scale = "SS1", center = TRUE)
DATA2 <- ExPosition::expo.scale(X2, scale = "SS1", center = TRUE)
RX <- t(DATA1) %*% DATA1
RY <- t(DATA2) %*% DATA2
RXY <- t(DATA1) %*% DATA2

res.tepCCA <- tepCCA(X1, X2, graphs = FALSE)
graph4tepCCA(res.tepCCA)


######### THIS GOES IN THE FUNCTION #########
### 1. Heatmaps

### 2. Latent variables

### 3. Factor scores

### 4. Contributions





