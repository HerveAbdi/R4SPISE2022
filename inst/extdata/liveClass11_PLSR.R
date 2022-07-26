#_________________________________________________
# Entête ----
# Live Class 11. ---- 
# Partial Least Square Correlation (PLSR)
# October 26, 2020. PLSR 36 Wines. 
# Predict Sensory from Chemistry
#_________________________________________________
# Preamble ----
# Example for PLSR_SVD 
# Clean start ---
rm(list = ls())
graphics.off()
# parameters for pptx ----
leTitre   <- 'PLSR: Wines of the World'
leDir     <- here::here() # Current dir 
filename  <- 'PLSR4WinesOfTheWorld'
path2save <-  paste0(leDir, '/', filename)
#_________________________________________________
library(corrplot)
library(ExPosition)
library(tidyverse)
library(PTCA4CATA)
library(data4PCCAR) # PLSR 
#_________________________________________________
# **** The data ----
# # from data4PCCAR
# data("fiveWines4Rotation")
data("winesOf3Colors")
rawData <- winesOf3Colors$winesDescriptors
descriptors <-  rawData[,1:4]
Xmat <-  rawData[, 5:8]  # Will be the X-matrix
Ymat <-  rawData[, 9:17] # Will be the Y-matrix
# # Compute the correlation matrix
XY.cor <- cor(Xmat,Ymat)
# Plot it with corrplot
# Plot it with corrplot
corrplot::corrplot(XY.cor, method = "color") 
a0000.corXY <- recordPlot()
#_________________________________________________
# Compute PLSR ----
resPLSR <- PLSR_SVD(Xmat, Ymat, 3, 
                    inference = TRUE,
                    displayJack = FALSE)
#_________________________________________________
# print the results .....
# **** Graphs ----
# pseudo scree -----
# Pseudo scree X
PlotScree(ev    = resPLSR$R2x,
   title = 'Wines of the World: RX2 Scree Plot',
)
# Save the plot
a0002.Scree.R2X <- recordPlot()
# Pseudo scree Y
PlotScree(ev    = resPLSR$R2y,
   title = 'Wines of the World: RY2 Scree Plot',
)
# Save the plot
a0004.Scree.sv.R2Y <- recordPlot()
#_________________________________________________
# wineColors ----
#_________________________________________________
# wineColors ----
wineColors <- winesOf3Colors$winesDescriptors$color
wineColors <- recode(wineColors, 
                   red = 'indianred4', 
                   white = 'gold', 
                   rose = 'lightpink2')
#_________________________________________________
#  Latent variables ----
# T ----
# a graph of the observations
T.Imap <- PTCA4CATA::createFactorMap(
  resPLSR$T,
  col.points = wineColors,
  col.labels = wineColors,
  display.labels = TRUE,
  alpha.points = .5
)
#_________________________________________________
# make labels ----
label4Tx <- labs(x = paste0('T',1,'. R2X = ',
                            round(resPLSR$R2x[1],2) ))
label4Ty <- labs(y = paste0('T',2,'. R2X = ',
                            round(resPLSR$R2x[2],2) ))
#_________________________________________________
a002.Map.I <- T.Imap$zeMap + label4Tx + label4Ty
#_________________________________________________
# W ----
# a graph of the predictors
col4I <- prettyGraphsColorSelection(nrow(resPLSR$W), 
                             starting.color = 1)
W.Imap <- PTCA4CATA::createFactorMap(
  resPLSR$W,
  col.points = col4I,
  col.labels = col4I,
  alpha.points = .5
)
# arrows
zeArrows4I <- addArrows(resPLSR$W, 
                        color = col4I)
# The map
b000.aggMap.W <- W.Imap$zeMap + zeArrows4I +
  label4Tx + label4Ty

# Y set ----
#_________________________________________________
# corrplot ----
col4J <- prettyGraphsColorSelection(ncol(Ymat), 
                            starting.color = 42)
# First a correlation plot (i.e., loadings as r)
cor.TY <- t(cor(Ymat, resPLSR$T))
corrplot(round(cor.TY, 2)  )
c001.corrMap.TY<- recordPlot()
# circle corr plot ----
#_________________________________________________
# Circle with PTCA: Use loading.2: the correlation
# Create the map
jolie.ggplot.J <- PTCA4CATA::createFactorMap(
  t(cor.TY),
  col.points = col4J, 
  col.labels = col4J,                   
  constraints = list(minx = -1, miny = -1,
                     maxx = 1 , maxy = 1) 
)
# make labels ----
label4Yx <- labs(x = paste0('T',1,'. R2Y = ',
                   round(resPLSR$R2y[1],2) ))
label4Yy <- labs(y = paste0('T',2,'. R2Y = ',
                      round(resPLSR$R2y[2],2) ))
#_________________________________________________
# draw the circle
d0001.jolieggMap.J <- jolie.ggplot.J$zeMap + 
  addCircleOfCor()
print(d0001.jolieggMap.J) 
#  Add some arrows
arrows <- addArrows(t(cor.TY), color = col4J)  
d0002.jolieggMap.J <- d0001.jolieggMap.J + 
  arrows + 
  label4Yx + label4Yy
print(d0002.jolieggMap.J )
#_________________________________________________
# 2. Loadings as C ----
# 
jolie.ggplot.J.C <- PTCA4CATA::createFactorMap(
  resPLSR$C, # pseudo loadings
  col.points = col4J, col.labels = col4J,
  constraints = list(minx = -1, miny = -1,
                     maxx = 1 , maxy = 1))
arrows.C <- addArrows(resPLSR$C, color = col4J)  
d0005.jolieggMap.J.C <- 
  jolie.ggplot.J.C$zeMap_background + 
  jolie.ggplot.J.C$zeMap_text +
  arrows.C + 
  label4Yx + label4Yy
print(d0005.jolieggMap.J.C)
#_________________________________________________
#_________________________________________________


#_________________________________________________
# **** save as pptx ----
# Automatic save with saveGraph2pptx
savedList <- PTCA4CATA::saveGraph2pptx(
  file2Save.pptx = path2save, 
  title = leTitre, 
  addGraphNames = TRUE)
# EoF ----
#_________________________________________________



