rm(list = ls()) ; graphics.off() ; gc()

library(R4SPISE2022)
library(ExPosition)

set.seed(57115)
I <- 57
J <- 11
X <- matrix(rnorm(I * J), I, J)

res.epPCA <- epPCA(DATA = X, graph = FALSE)
res.f1 <- graph4epPCA(data = X)
res.f2 <- OTAplot(res = res.epPCA, data = X)
