rm(list = ls())

infres <- epPCA.inference.battery(state.x77, graphs = FALSE)

library(plotrix) # for plotting CI
library(graphics)

# Compute bootstrapped CI for eigenvalues
source("Boot4Eigs.R")
source("PlotScreeWithCI.R")
Boot.eigs <- Boot4Eigs(state.x77,
                       center = TRUE,
                       scale = TRUE,
                       design = state.division,
                       CI.perc = c(0.025, 0.975))

PlotScreeWithCI(ev = infres$Fixed.Data$ExPosition.Data$eigs,
                p.ev = infres$Inference.Data$components$p.vals,
                ci.ev = Boot.eigs,
                polygon.ci = TRUE) # New parameter

