rm(list = ls()) ; graphics.off() ; gc()

library(R4SPISE2022)
library(ExPosition)

set.seed(57115)
I <- 57
J <- 11
X <- matrix(rnorm(I * J), I, J)

res.epPCA <- epPCA(DATA = X, graph = FALSE)
graph4epPCA(data = X)
