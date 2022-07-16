rm(list = ls()) ; gc() ; graphics.off()

library(R4SPISE2022) # Our package
library(TExPosition)
library(data4PCCAR)
library(ggplot2)
library(MASS)
library(PTCA4CATA)

set.seed(57115)
n <- 57
n1 <- n %/% 2
n2 <- n - n %/% 2
p1 <- 11
p2 <- 5
indnames <- sprintf("Ind%02i", 1:n)
varnamesX <- sprintf("X%02i", 1:p1)
varnamesY <- sprintf("Y%02i", 1:p2)

X <- rbind(
    mvrnorm(n = n1, mu = rep(-1, p1), Sigma = diag(p1)),
    mvrnorm(n = n2, mu = rep(+1, p1), Sigma = diag(p1)))
Y <- rbind(
    mvrnorm(n = n1, mu = rep(-1, p2), Sigma = diag(p2)),
    mvrnorm(n = n2, mu = rep(+1, p2), Sigma = diag(p2)))

dimnames(X) <- list(indnames, varnamesX)
dimnames(Y) <- list(indnames, varnamesY)

DATA1 <- ExPosition::expo.scale(X, scale = "SS1", center = TRUE)
DATA2 <- ExPosition::expo.scale(Y, scale = "SS1", center = TRUE)
RX <- t(DATA1) %*% DATA1
RY <- t(DATA2) %*% DATA2
RXY <- t(DATA1) %*% DATA2
DESIGN <- data.frame(
    groups = rep(c("group1", "group2"),
                 c(n %/% 2, n - n %/% 2)))
col4I <- rep(c(group1 = "hotpink1", group2 = "#436EEE"),
             c(n %/% 2, n - n %/% 2))
# If col4I is null but there is a design: select colors
# from a nice palette.
# If I have col4I and DESIGN but they don't match, then "warning" +
# create the colors automatically.
# If I col4I but no design, then why not.

## To select colors when the users are too lazy to pick their own!!
# col4I <- prettyGraphsColorSelection(nrow(Fi),
#                                     starting.color = 1)


col4J <- rep(c("#1CBE4F", "#C4451C"),
             c(p1 %/% 2, p1 - p1 %/% 2))
col4K <- rep(c("#1CFFCE", "tomato"),
             c(p2 %/% 2, p2 - p2 %/% 2))

color.obs <- list(
    oc = matrix(col4I, ncol = 1, dimnames = list(indnames)),
    gc = rbind(group1 = "hotpink1", group2 = "#436EEE")
)

color.tab <- list(oc = list(col4J, col4K))

res.tepPLS <- tepPLS(X, Y, DESIGN = DESIGN, graphs = FALSE)
res.plot <- coreTTAplot(res = res.tepPLS)
res.plot <- coreTTAplot(
    res = res.tepPLS,
    color.obs = color.obs,
    color.tab = color.tab,
    alpha.points = 0.7)

res.plot$heatmap.rxy
res.plot$scree.eig
res.plot$scree.sv
res.plot$lv.plot
res.plot$ctrX.plot
res.plot$ctrY.plot
######### THIS GOES IN THE FUNCTION #########
### 1. Heatmaps
corrplot::corrplot(RX, tl.col = "black")
corrplot::corrplot(RY, tl.col = 1:5)
corrplot::corrplot(RXY, tl.col = "black")
### 2. Scree plots
PlotScree(ev = res.tepPLS$TExPosition.Data$eigs,
          title = 'Eigenvalue Scree Plot',
          plotKaiser = TRUE,
          color4Kaiser = ggplot2::alpha('darkorchid4', .5),
          lwd4Kaiser  = 2)
PlotScree(ev = res.tepPLS$TExPosition.Data$eigs^(1/2),
          title = 'Singular Value Scree Plot',
          plotKaiser = FALSE,
          color4Kaiser = ggplot2::alpha('darkorchid4', .5),
          lwd4Kaiser  = 2)
### 3. Latent variables
laDim <- 1
lv1.xy <- cbind(
    res.tepPLS$TExPosition.Data$lx[,laDim, drop = FALSE],
    res.tepPLS$TExPosition.Data$ly[,laDim, drop = FALSE])
colnames(lv1.xy) <-
    c(paste0('LX',laDim), paste0('LY',laDim))
lv1 <- createFactorMap(lv1.xy,
                       title = 'PLSC: First Pair of Latent Variables',
                       col.points = col4I,
                       alpha.points = .4,
                       col.labels = col4I,
                       alpha.labels = .4)
## Careful : when there is a design, we need to fade more
(a001.LV1 <- lv1$zeMap +
    xlab(paste0("X Latent Variable ", laDim)) +
    ylab(paste0("Y Latent Variable ", laDim)))


indivMeans <- PTCA4CATA::getMeans(lv1.xy, names(col4I))
col4Means <- c(group1 = "hotpink1", group2 = "#436EEE")
#### the map ----
MapGroup <- PTCA4CATA::createFactorMap(indivMeans,
                                       # use the constraint from the main map
                                       constraints = lv1$constraints,
                                       col.points = col4Means,
                                       cex = 7,  # size of the dot (bigger)
                                       col.labels = col4Means,
                                       text.cex = 6,
                                       pch = 17)
### Warning: means are too big + print it last!
# The map with observations and group means
a003.lv1.withMeans <- a001.LV1 +
    MapGroup$zeMap_dots + MapGroup$zeMap_text
print(a003.lv1.withMeans)
##_________________________________________________
# Confidence intervals
# 3. Boostrap for the groups in LV Space.
# Bootstrap for CI:
BootCube.Gr <- PTCA4CATA::Boot4Mean(lv1.xy,
                                    design = names(col4I),
                                    niter = 100,
                                    suppressProgressBar = TRUE)
# Create the ellipses
#### Bootstrapped CI ----
##_________________________________________________
# Create Confidence Interval Plots
# use function MakeCIEllipses from package PTCA4CATA
dimnames(BootCube.Gr$BootCube)[[2]] <- c("LX1","LY1")
GraphElli <- PTCA4CATA::MakeCIEllipses(
    BootCube.Gr$BootCube[,1:2,],
    names.of.factors = c("LX1","LY1"),
    col = col4Means[rownames(BootCube.Gr$BootCube)],
    p.level = .95
)
##_________________________________________________
# create the I-map with Observations,
#   means and confidence intervals
#
(a004.lv1.withCI <-  a001.LV1 +
        MapGroup$zeMap_text +
        MapGroup$zeMap_dots +
        GraphElli)

### 3. Contributions
##### Ctr I-set ----
Fi   <- res.tepPLS$TExPosition.Data$fi
ctri <- res.tepPLS$TExPosition.Data$ci
signed.ctri <- ctri * sign(Fi)
# LV1
a020.plotCtri.1 <- PrettyBarPlot2(
    bootratio = round(100*signed.ctri[,1]),
    threshold = 100/ nrow(signed.ctri),
    ylim = NULL,
    color4bar = col4J,
    color4ns = "gray75",
    plotnames = TRUE,
    main = 'Important Contributions I-set: LV1',
    ylab = "Signed Contributions")
print(a020.plotCtri.1)
##_________________________________________________
##### Ctr J-set ----
Fj   <- res.tepPLS$TExPosition.Data$fj
ctrj <- res.tepPLS$TExPosition.Data$cj
signed.ctrj <- ctrj * sign(Fj)
# LV1
a021.plotCtrj.1 <- PrettyBarPlot2(
    bootratio = round(100*signed.ctrj[,1]),
    threshold = 100 / nrow(signed.ctrj),
    ylim = NULL,
    color4bar = col4K,
    color4ns = "gray75",
    plotnames = TRUE,
    main = 'Important Contributions J-set: LV1',
    ylab = "Signed Contributions")
print(a021.plotCtrj.1)
