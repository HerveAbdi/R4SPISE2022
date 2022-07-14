rm(list = ls()) ; gc() ; graphics.off()

library(R4SPISE2022) # Our package
library(TExPosition)

library(ade4)
library(CCA)
library(vegan)

set.seed(57115)
n <- 57
p1 <- 11
p2 <- 5
X1 <- matrix(rnorm(n * p1), n, p1)
X2 <- matrix(rnorm(n * p2), n, p2)
DATA1 <- ExPosition::expo.scale(X1, scale = "SS1", center = TRUE)
DATA2 <- ExPosition::expo.scale(X2, scale = "SS1", center = TRUE)


# vegan::cca(X1, X2) # Not the correct one
res.ra <- vegan::rda(X1, X2) # Redundancy Analysis
res.cca <- CCA::cc(X1, X2) ## CCA
res.pls <- ade4::coinertia(
    dudi.pca(X1, scale = TRUE, scan = FALSE, nf = 3),
    dudi.pca(X2, scale = FALSE, scan = FALSE, nf = 2),
    scan = FALSE, nf = 2)

res.tepra  <- tepRA(DATA1 = X1, DATA2 = X2, graphs = FALSE)
res.tepcca <- tepCCA(DATA1 = X1, DATA2 = X2, graphs = FALSE)
res.teppls <- tepPLS(DATA1 = X1, DATA2 = X2, graphs = FALSE)

### RA : check ###
Ura <- res.tepra$TExPosition.Data$u
Pra <- res.tepra$TExPosition.Data$pdq$p
Vra <- res.tepra$TExPosition.Data$v
Qra <- res.tepra$TExPosition.Data$pdq$q
Mra <- res.tepra$TExPosition.Data$W1
Wra <- res.tepra$TExPosition.Data$W2
LXra <- res.tepra$TExPosition.Data$lx
LYra <- res.tepra$TExPosition.Data$ly
delta_ra <- res.tepra$TExPosition.Data$pdq$Dv

t(Pra) %*% Mra %*% Pra
t(Qra) %*% Wra %*% Qra

diag(t(LXra) %*% LYra)

### CCA : check ###
Ucca <- res.tepcca$TExPosition.Data$u
Pcca <- res.tepcca$TExPosition.Data$pdq$p
Vcca <- res.tepcca$TExPosition.Data$v
Qcca <- res.tepcca$TExPosition.Data$pdq$q
Mcca <- res.tepcca$TExPosition.Data$W1
Wcca <- res.tepcca$TExPosition.Data$W2
LXcca <- res.tepcca$TExPosition.Data$lx
LYcca <- res.tepcca$TExPosition.Data$ly
delta_cca <- res.tepcca$TExPosition.Data$pdq$Dv
Ficca <- res.tepcca$TExPosition.Data$fi
Fjcca <- res.tepcca$TExPosition.Data$fj


RX <- t(DATA1) %*% DATA1
RY <- t(DATA2) %*% DATA2
solve(RX) - Mcca
solve(RY) - Wcca

t(Pcca) %*% Mcca %*% Pcca
t(Qcca) %*% Wcca %*% Qcca

t(Ucca) %*% RX %*% Ucca
t(Vcca) %*% RY %*% Vcca

diag(t(LXcca) %*% LYcca) - delta_cca

sum((LXcca - DATA1 %*% Mcca %*% Pcca)**2)
sum((LYcca - DATA2 %*% Wcca %*% Qcca)**2)

sum((Ficca - Ucca %*% diag(delta_cca))**2)
sum((Fjcca - Vcca %*% diag(delta_cca))**2)

diag(t(Pcca) %*% t(DATA1) %*% DATA2 %*% Qcca)

# Comparison to CCA::cc
## canonical correlation
sum((delta_cca - res.cca$cor)**2)
## LX and LY
sum((res.cca$scores$xscores/sqrt(n-1) - LXcca)**2)
sum((res.cca$scores$yscores/sqrt(n-1) - LYcca)**2)
## Loadings
sum((res.cca$xcoef * apply(X1, 2, sd) - Mcca %*% Pcca)**2)
sum((res.cca$ycoef * apply(X2, 2, sd) - Wcca %*% Qcca)**2)

# Other checks for CCA::cc
diag(t(res.cca$scores$xscores) %*% res.cca$scores$yscores) - res.cca$cor * (n-1)
scale(X1, scale = FALSE) %*% res.cca$xcoef - res.cca$scores$xscores





DATA1 <- ExPosition::expo.scale(X1, scale = "SS1", center = TRUE)
DATA2 <- ExPosition::expo.scale(X2, scale = "SS1", center = TRUE)
R <- t(DATA1) %*% DATA2
M <- t(DATA1) %*% DATA1
Mm1 <- data4PCCAR:::matrix.exponent(M, power = -1)
# W <- NULL
# W <- rep(1, ncol(DATA2))
W <- diag(ncol(DATA2))

debugonce(data4PCCAR:::genPDQ2)

res <- data4PCCAR:::genPDQ2(
    datain = R,
    M = Mm1,
    W = W,
    is.mds = FALSE,
    decomp.approach = "svd",
    k = 2)


