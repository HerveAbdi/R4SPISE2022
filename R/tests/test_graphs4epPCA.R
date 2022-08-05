rm(list = ls()) ; graphics.off() ; gc()

library(R4SPISE2022)
library(ExPosition)
library(data4PCCAR)
library(InPosition)
library(plotrix) # for plotting CI

set.seed(57115)
I <- 57
J <- 11
X <- matrix(rnorm(I * J), I, J)

res.epPCA <- epPCA(DATA = X, center = TRUE, scale = "ss1", graph = FALSE)
# res.f1 <- graph4epPCA(data = X, center = TRUE, scale = "ss1")
res.plot <- OTAplot(resPCA = res.epPCA, data = X)
res.plot.inference <- OTAplotInference(resPCA = res.epPCA, data = X)

res.plot$results.graphs$scree
res.plot$results.graphs$factorScoresI12
res.plot$results.graphs$cosineCircleArrowJ12

res.plot.inference$results.graphs$scree
res.plot.inference$results.graphs$BR1
res.plot.inference$results.graphs$BR2
