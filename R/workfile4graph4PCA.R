# Work file for PCA output ----
# Created  07/10/2022
# for package R4SPISE2022
# Current Version 07/10/2022
# Goal: generates standard graphs and tables for PCA
# Author Hervé
# Entêtee -----
# install.packages('sinew')
# sinew::makeOxygen(graph4epPCA)
# Preamble ----
# Pass Results from Exposition
# ExPosition
#
# Documentation -----
#' @title graph4epPCA run a PCA
#' (with the \code{ExPosition} package)
#' and generates the standard graphs and tables.
#' Note: *Still Under Development*.
#' @description \code{graph4epPCA}
#' graph4epPCA run a principal component
#' analysis
#' (with the \code{ExPosition} package)
#' and generates the standard graphs and tables.
#' Note that the parameters
#' \code{data, scale, center, DESIGN,
#'  make_design_nominal, k} are passed
#'  to  the \code{ExPosition} package unchanged
#'  except for \code{scale} which defaults now
#'  to \code{'SS1'}.
#' @param data A data frame or a matrix with
#' numerical data suitable for a PCA. Passed to
#' \code{ExPosition::epPCA}.
#' @param scale  scale (i.e., normalize) the
#' columns of \code{data}.
#'  Default: \code{TRUE}. Values could
#'  be \code{TRUE, FALSE, 'Z', 'SS1'}. \code{TRUE}
#'  defaults to \code{SS1}.
#'  Passed to
#' \code{ExPosition::epPCA}.
#'
#' @param center Default: TRUE.
#' do we center the columns of \code{data}
#' Passed to
#' \code{ExPosition::epPCA}.
#' @param DESIGN  Default: NULL.
#' A design vector (could be factor or character)
#' or (Boolean) matrix used to assigne oobservations
#' to groups.
#' Passed to
#' \code{ExPosition::epPCA}.
#' @param make_design_nominal
#' if TRUE (Default) transform
#' the vector from \code{DESIGN} into
#' a Boolean matrix.
#' Passed to
#' \code{ExPosition::epPCA}.
#' @param k number
#' of factor to keep; when equql to
#' 0  (Default), all factors are kept.
#' Passed to
#' \code{ExPosition::epPCA}.
#' @param graphs  do we want graphs?
#' Current Default is \code{12} which indicates that
#' the graphs are generated for the first
#' 2 components. Note that current version
#' is creating output only for the first two
#' components,
#' @param printGraphs  (Default: FALSE)
#' do we want to print the graphics as \code{.png}?
#' @param col4I  a color vector for
#' plotting the observations (if \code{NULL}
#' Default) use colors from \code{ExPosition::epPCA}.
#' @param col4J
#' a color vector for
#' plotting the variables (if \code{NULL}
#' Default) use colors from \code{ExPosition::epPCA}.
#' @param rotation Do we want to rotate
#' (with \code{varimax})
#' the variables (Default: \code{FALSE})
#' @param nfactor4rotation number of factors to
#' keep for the rotation, could be a number
#' (note if the number is too big
#' \code{nfactor4rotation} will default to 2),
#' or a name (currently only \code{'Kaiser'}
#' which is the default). When \code{'Kaiser'}
#' is chosen, we keep only the components with an eigenvalue
#' larger than average (when \code{scaled == 'SS1'}
#' this is the familiar rule: "keep only
#' the components with eigenvalue larger than 1").
#' @param biplot De we want to create biplots
#'  (Default: \code{FALSE})?
#' @param inferences Run inferences
#'  from \code{InPosition}
#'  Remains to be done,
#'  Default is \code{FALSE}.
#' @param save2pptx  Default: ''
#' Not yet implemented,
#' @param title4pptx PARAM_DESCRIPTION, Default:
#' 'PCA Results'.
#' Not yet implemented,.
#' @return A list made of two lists
#'
#' @details Work in Progress
#' @author Hervé Abdi
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Example from data4PCCAR
#'  data("sixBeers12Descriptors10Judges", package = 'data4PCCAR')
#'  df <- sixBeers12Descriptors10Judges$ratingsIntensity
#'  res4graph <- graph4epPCA(data = df, scale = FALSE)
#'  }
#' }
#' @seealso
#'  \code{\link[ExPosition]{epPCA}}
#'  \code{\link[PTCA4CATA]{PlotScree}}, \code{\link[PTCA4CATA]{createFactorMap}}
#' @rdname graph4epPCA
#' @export
#' @import ggplot2 PTCA4CATA data4PCCAR corrplot
#' @importFrom ExPosition epPCA
##  @importFrom PTCA4CATA PlotScree createFactorMap createxyLabels.gen
#' @importFrom grDevices colorRampPalette  dev.off  jpeg png recordPlot
#' @importFrom stats cor  cov varimax
# function ----
graph4epPCA <- function(
data         ,# the data. No default
scale = TRUE, # 	scale the data. default TRUE = SS1
             #  alternative: Z scores
center = TRUE, # center the data
             #  see expo.scale for details.
DESIGN = NULL, # a design matrix to indicate if rows belong to groups.
make_design_nominal = TRUE,
k = 0,
#
graphs = 12, # possibility a vector with the graphs
            # if NULL no graph. still in thinking
            # could just the list of cpt to keep
printGraphs = FALSE, # when TRUE print the png file
            # currently mostly for debugging
col4I = NULL, # default use result from ExPosition
col4J = NULL, # default use result from ExPosition
biplot   = FALSE, # Get a Biplot
rotation = FALSE, # Do we rotate
nfactor4rotation = 'Kaiser', # little Jiffy nf > mean eig
                   # but at least 2
inferences = FALSE , # remains to be done includes inferences
save2pptx = "", # default is NO. If provided
               # with a name then save in a powerpoint
title4pptx = "PCA Results" # title of pptx

){
printTest = TRUE  # to debug the graphs
printTest = FALSE # to debug the graphs
if (isTRUE(scale)) {scale = 'SS1'}
# 3 Internal functions
# functions rad2deg ----
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}
arcosdeg <- function(lecos){rad2deg(acos(lecos))}
#
# An internal function
# A function ----
plotCtr <- function(signed.ctr, col4, nfac = 1,
                    print = FALSE, stem = "Jset",
                    font.size = 5,
                    horizontal = FALSE){
  leTitre <- paste0("Component ",nfac)
  ctr <- PrettyBarPlot2(signed.ctr[, nfac],
                        threshold = 1 / NROW(signed.ctr),
                        font.size = font.size,
                        signifOnly = FALSE,
                        color4bar = col4,
       ylab = 'Signed Contributions',
       ylim = c(1.2*min(signed.ctr[,nfac]),
         1.2*max(signed.ctr[,nfac])),
       horizontal = horizontal) +
    ggtitle("Contribution Barplots",
            subtitle = leTitre) +
    theme(plot.title = element_text(
      color = "#3E2E8F", size = 20,
      face = "bold"),
      plot.subtitle = element_text(
        color = "#3E2E8F", size = 16,
        face = "italic"),
      plot.caption =  element_text(
        color = "#3E2E8F", size = 14,
        face = "italic"),
      axis.text =  element_text(
        color = "#3E2E8F", size = 12),
      axis.title.x =  element_text(
        color = "#3E2E8F", size = 16,
        face = "italic"))
  if (print){
    png(paste0(stem,nfac,'_Contributions.png'))
    print(ctr)
    dev.off()
    jpeg(paste0(stem,nfac,'_Contributions.jpeg'))
    print(ctr)
    dev.off()
  }
  return(ctr)

} # eof

resPCA <- nameExpositionResults(ExPosition::epPCA(DATA = data,
                            center = center,
                            scale = scale,
                            DESIGN = DESIGN,
            make_design_nominal = make_design_nominal,
            graphs = FALSE,
            k = k) )
# NB use namenameExpositionResults() to have
# Dimensions named
if (is.null(col4I)) {col4I <- resPCA$Plotting.Data$fi.col}
if (is.null(col4J)) {col4J <- resPCA$Plotting.Data$fj.col}
laMat <- resPCA$ExPosition.Data$X
# get nfact for rotation
if (isTRUE(rotation)){
    eigs <-  resPCA$ExPosition.Data$eigs[
              resPCA$ExPosition.Data$eigs >
                    100*.Machine$double.eps]
    nK <- length(eigs)
    if (tolower(nfactor4rotation) == 'kaiser'){
        nfactor4rotation <- sum(eigs > mean(eigs))
         if (nfactor4rotation == 1) nfactor4rotation <- 2
              }
    if (nfactor4rotation > nK) nfactor4rotation <- 2
}
#_________________________________________________
# Get the loadings ----
# But, where are the loadings?
#  And which ones of the loadings?
## 1. Slices of Inertia ----
loadings.1 <- resPCA$ExPosition.Data$fj
## 2. Correlations ----
loadings.2 <- t(cor(data, resPCA$ExPosition.Data$fi))
# Loadings as coefficients
# of the optimal linear combination
cor4print <- loadings.2
row.names(cor4print) <- paste0('f',1:nrow(cor4print))
# round(cor4print, 2)
## 3. mixture coefficients ----
loadings.3 <- resPCA$ExPosition.Data$pdq$q
# Biplots ----
## Shortcuts for Biplot ----
Fi    <- resPCA$ExPosition.Data$fi
# P     <- resPCA$ExPosition.Data$pdq$p
delta <- resPCA$ExPosition.Data$pdq$Dv
Fj    <- resPCA$ExPosition.Data$fj
Q     <- resPCA$ExPosition.Data$pdq$q
Ci    <- resPCA$ExPosition.Data$ci
Cj    <- resPCA$ExPosition.Data$cj
M     <- resPCA$ExPosition.Data$M
W     <- resPCA$ExPosition.Data$W
## Normalization for Biplots ----
# here are some of the popular
# normalization schemes  for Biplots
#(Fi^2) %*% diag(1/delta^2) # ctr
# Fi_scaled_p <- Ci^(1/2) * sign(Fi) # in fact P
# Fj_scaled_q <- Cj^(1/2) * sign(Fj) # in fact Q
# #(Fi^2) * sign(Fi)
#Fi_scaled   <- Fi # in fact P
#Fj_scaled   <- Q # in fact Q  # here shrinks too much
# #(Fi^2) * sign(Fi)
Fi_scaled   <- Fi %*% diag(delta^(-1/2))  # P*D^(1/4)
Fj_scaled   <- Q  %*% diag(delta^( 1/2))  # Q*D^(1/4)
# # F*_scaled re-scaled sqrt(N)
Fi_scaled_N <-  Fi_scaled  / sqrt(NROW(Fi_scaled))
Fj_scaled_N <-  Fj_scaled  / sqrt(NROW(Fj_scaled))
#_________________________________________________
# Here we go for only one biplot normalization
# Today my favorite is:
#  scaled by delta*sqrt(N)
#_________________________________________________
Fi_bi <-  Fi_scaled_N
Fj_bi <-  Fj_scaled_N
# graphs here -----
##  Scree ----
# use PTCA4CATA::PlotScree
scree <- PTCA4CATA::PlotScree(ev =
               resPCA$ExPosition.Data$eigs,
               plotKaiser = TRUE)
a01.leScree <- recordPlot()
#_________________________________________________
## The I-set (plot the rows)----
#_________________________________________________

# look at the help for PTCA4CATA::createFactorMap
jolie.ggplot1 <- PTCA4CATA::createFactorMap(
  resPCA$ExPosition.Data$fi,
  col.points = col4I,
  col.labels = col4I,
  font.face = 'italic'
)
# Create the labels for Inertia per dimension
label4Map <- createxyLabels.gen(1,2,
       lambda = resPCA$ExPosition.Data$eigs,
        tau = resPCA$ExPosition.Data$t)
a3.JolieggMap <- jolie.ggplot1$zeMap +
  label4Map +
  labs(title = 'The Observation Map.')
# to look at the map
# if( printTest) print(a3.JolieggMap)
# To make some changes at the labels:
#  use and modify this code
label4Map2 <- list(label4Map,
    # The standard label from createxyLabels.gen
   theme(axis.title = element_text(# the new theme
   color = "darkorchid4",
   size = rel(1.1),    # relative to default
   family = 'Times',   # "Times", "sans", "Courier"
   face   = "italic" , # 'plain','italic', 'bold',
   # NB: face does not work with current ggplot2
   ), # end of element_text
    plot.title = element_text(color = '#5826A3')
                 ) )

a4.JolieggMap <- jolie.ggplot1$zeMap + label4Map2
if(printTest){print(a4.JolieggMap)}
# print(a4.JolieggMap)
a4.JolieggMap.2 <- a3.JolieggMap + label4Map2
# to look at the map
if (printGraphs){
   png('ObservationImap.png')
   print(a4.JolieggMap.2)
   dev.off() }


### Contributions ----
signed.ctrI <- resPCA$ExPosition.Data$ci *
              sign( resPCA$ExPosition.Data$fi)
### ctr4I -----
leStem <-  'I-Set'
nfac  <- 1
ctrI1 <- plotCtr(signed.ctrI, col4I,
                 nfac, stem = leStem)
nfac  <- 2
ctrI2 <- plotCtr(signed.ctrI, col4I,
                 nfac, stem = leStem)
### I-Cosines -----
cos4I <- sqrt(resPCA$ExPosition.Data$ri) *
         sign(resPCA$ExPosition.Data$fi)
### 1.Iset - Circle of corr ----
jolie.ggplot.I <- PTCA4CATA::createFactorMap(
  cos4I,
  col.points = col4I,
  col.labels = col4I,
  constraints = list(minx = -1, miny = -1,
                     maxx = 1 , maxy = 1),
  font.face = "italic")
# draw the circle
a01.jolieggMap.I <- jolie.ggplot.I$zeMap +
  addCircleOfCor()
if (printGraphs){
  print(a01.jolieggMap.I)
}
#  Dot for the I-set, no arrows
a02.jolieggMap.I <- jolie.ggplot.I$zeMap_background +
  jolie.ggplot.I$zeMap_text +
  addCircleOfCor() +
  jolie.ggplot.I$zeMap_dots  + label4Map2
if(printTest){print(a02.jolieggMap.I)}
if (printGraphs){
  png('CircleOfCorrISet.png')
  print(a02.jolieggMap.I)
  dev.off() }
#________________________________________________
### A heat map ----
####  V1a data raw ----
a001.heatMap <- suppressWarnings(makeggHeatMap4CT(
  data,
  colorProducts = col4I,
  colorAttributes = col4J,
  fontSize.x = 15,
  face.x = 'bold',
  face.y = 'italic',
) )  +
  ggtitle("Mean Intensity") +
  theme(plot.title = element_text(
                 family = "Helvetica",
                 face   = "bold", size = (20)))
if (printGraphs){
png('JIntensityHeatMap.png')
print(a001.heatMap)
dev.off() }
 if(printTest){a001.heatMap}
####  V1b data centered ----
df.centered  <- apply(data, 2, scale0, scale = FALSE)
a002.heatMap <- suppressWarnings(makeggHeatMap4CT(
  df.centered,
  colorProducts = col4I,
  colorAttributes = col4J,
  fontSize.x = 15,
  face.x = 'bold',
  face.y = 'italic',
) )  +
  ggtitle("Centered Data") +
  theme(plot.title = element_text(
              family = "Helvetica",
              face = "bold", size = (20)))
if(printTest){df.centered}
if (printGraphs){
  png('JIntensityHeatMapCentered.png')
  print(df.centered)
  dev.off() }
####  V1c df normed ----
df.normed <- apply(data, 2, scale0, scale = 'SS1')
a002n.heatMap <- suppressWarnings(makeggHeatMap4CT(
  df.normed,
  colorProducts = col4I,
  colorAttributes = col4J,
  fontSize.x = 15,
  face.x = 'bold',
  face.y = 'italic',
) )  +
  ggtitle("Centered and Scaled Data") +
  theme(plot.title = element_text(
    family = "Helvetica",
    face = "bold", size = (20)))
if(printTest){a002n.heatMap}
if (printGraphs){
  png('JIntensityHeatMapNormed.png')
  print(a002n.heatMap)
  dev.off() }

# Covariance ----
nI <- nrow(data)
Smat <- cov(data) #  *(  (nI - 1) / N)
# covariance with N - 1
# Covariance mat ----
### 0. Cov Mat
col <- colorRampPalette(
  c("#BB4444", "#EE9988",
    "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(Smat,
         is.corr = FALSE,
         method = "color", # "number"
         col = col(200),
         tl.col = col4J,
         pch.col = col(200),
         type = "upper",
         order = "hclust",
         addCoef.col =  "grey", #"black",
         addCoefasPercent	= TRUE,
         # Add coefficient of correlation
         tl.srt = 45,
         #Text label color and rotation
         # Combine with significance
         #p.mat = p.mat,
         # sig.level = 0.01,
         insig = "blank",
         # variance in the principal diagonal
         diag = TRUE
)
a5.02.covMap <- recordPlot()
if(printTest){a5.02.covMap}
if (printGraphs){
  png('JbyJ-Covariance.png')
  print(a5.02.covMap)
  dev.off() }
# Correlation mat ----
Rmat <- cor(data) #
corrplot(Rmat,
         is.corr = TRUE,
         method = "color", # "number"
         col = col(200),
         tl.col = col4J,
         pch.col = col(200),
         type = "upper",
         order = "hclust",
         addCoef.col =  "grey", #"black",
         addCoefasPercent	= TRUE,
         # Add coefficient of correlation
         tl.srt = 45,
         #Text label color and rotation
         # Combine with significance
         #p.mat = p.mat,
         # sig.level = 0.01,
         insig = "blank",
         # variance in the principal diagonal
         diag = TRUE
)
a5.02.correlationMap <- recordPlot()
if (printGraphs){
  png('JbyJ-Correlation.png')
  print(a5.02.correlationMap)
  dev.off() }
if(printTest){a5.02.correlationMap}
#_________________________________________________
### 1. Circle of corr ----
# Circle with PTCA: Use loading.2: the correlation
# Create the map
jolie.ggplot.J <- PTCA4CATA::createFactorMap(
  t(loadings.2),
  col.points = col4J,
  col.labels = col4J,
  constraints = list(minx = -1, miny = -1,
                     maxx = 1 , maxy = 1))
# draw the circle
b1.jolieggMap.J <- jolie.ggplot.J$zeMap +
                        addCircleOfCor()
if (printGraphs){
   png('J-CircleOfCorr_noArrow.png')
   print(b1.jolieggMap.J)
}
#  Add some arrows
arrows <- addArrows(t(loadings.2), color = col4J)
b2.jolieggMap.J <- jolie.ggplot.J$zeMap_background +
  jolie.ggplot.J$zeMap_text +
  addCircleOfCor() +
  arrows + label4Map2
# print(b2.jolieggMap.J)
if (printGraphs){
    png('J-CircleOfCorr.png')
    print(b2.jolieggMap.J)
    dev.off() }
## I & J Circle -----
b2.jolieggMap.IJ <- b2.jolieggMap.J +
   jolie.ggplot.I$zeMap_text +
   jolie.ggplot.I$zeMap_dots
#print(b2.jolieggMap.IJ)
if (printGraphs){
  png('IJ-CircleOfCorr.png')
  print(b2.jolieggMap.IJ)
  dev.off() }
## Get ctr as corr
corJ_PC <- round(t(loadings.2^2),2)
colnames(corJ_PC) <- paste0('f',1:ncol(corJ_PC))
### 2. Loadings as Q ----
#
jolie.ggplot.J.Q <- PTCA4CATA::createFactorMap(
  loadings.3,
  col.points = col4J,
  col.labels = col4J,
  constraints = list(minx = -1, miny = -1,
                     maxx =  1, maxy =  1) )
arrows.Q <- addArrows(loadings.3, color = col4J)
b3.jolieggMap.J.Q <- jolie.ggplot.J.Q$zeMap +
  #arrows.Q +
  label4Map2
b3.jolieggMap.J.Q.arrow <- jolie.ggplot.J.Q$zeMap +
  arrows.Q +
  label4Map2
# print(b3.jolieggMap.J.Q)
if (printGraphs){
    png('J-LoadingAsQ.png')
    print(b3.jolieggMap.J.Q)
 dev.off() }
if (printGraphs){
  png('J-LoadingAsQArrows.png')
  print(b3.jolieggMap.J.Q.arrow)
  dev.off() }
# Contributions ----
signed.ctrJ <- resPCA$ExPosition.Data$cj *
                 sign( resPCA$ExPosition.Data$fj)
## plot ctr ----
# plot contributions for component 1
col4J.bar <- col4J
#col4J.bar[3] <- '#2F7D1F'
nfac  <- 1
ctrJ1 <- plotCtr(signed.ctrJ, col4J.bar, nfac)
nfac  <- 2
ctrJ2 <- plotCtr(signed.ctrJ, col4J.bar, nfac)

# ***Rotation Here ----
if(isTRUE(rotation)){# rotation
    resVar <-  varimax(Fj[, 1 : nfactor4rotation],
                normalize = FALSE)
    Fj_rot <-  Fj[, 1 : nfactor4rotation] %*% resVar$rotmat
    Fi_rot <-  Fi[, 1 : nfactor4rotation] %*% resVar$rotmat
## Map for rotation here ----
## End of rotation map
} # end Rotation
#_________________________________________________
#__________________-----
# **** If Biplot ****
if (isTRUE(biplot)){ # start Biplot
# Biplot ----
## Scaled by sqrt(Delta/sqrt(N)) ----
### theme graph ----
col4labels <- "darkorchid4"
label4Map4Bi <- list(label4Map,
    # The standard label from createxyLabels.gen
    theme(axis.title = element_text(# the new theme
    color = col4labels,
    size = rel(1.2),    # relative to default
    family = 'sans',   # "Times", "sans", "Courier"
    face   = "italic" , # 'plain','italic', 'bold',
    # NB: face does not work with current ggplot2
    ), # end of element_text
    axis.text = element_text(size  = 12,
                             color = col4labels),
   plot.title = element_text(color = '#5826A3')
   ) )
# get constraints
# Fi_bi & Fj_bi
lesContraintes <- minmaxHelper(Fi_bi,Fj_bi)
# look at the help for PTCA4CATA::createFactorMap
sizeFontI  <- 5
jolie.biplot.I <- PTCA4CATA::createFactorMap(
  Fi_bi,
  col.points = col4I,
  col.labels = col4I,
  font.face = 'italic',
  constraints = lesContraintes,
  text.ce = sizeFontI)
jolie.biplot.J <- PTCA4CATA::createFactorMap(
  Fj_bi,
  col.points = col4J,
  col.labels = col4J,
  constraints = lesContraintes)
# add arrows
arrows.Bi <- addArrows( (Fj_bi), color = col4J)
e.JolieBiplot <- jolie.biplot.I$zeMap +
  jolie.biplot.J$zeMap_text + arrows.Bi +
  label4Map2 +
  labs(title = 'The Biplot Map.')
# to look at the map
print(e.JolieBiplot )
if (printGraphs){
   png('Biplot_FiFj.png')
   print(e.JolieBiplot )
dev.off() }

# *** End if Biplot ****
} # end of Biplots
#__________________----
# return lists ----
## stat ----
results.stats <- list(
            ExPosition.Data = resPCA$ExPosition.Data,
            Plotting.Data = resPCA$Plotting.Data,
            loadings.as.inertia = loadings.1,
            loadings.as.correlation = loadings.2,
            loadings.as.weights = loadings.3 #,
            # Fi_biplot = Fi_bi,
            # Fj_biplot = Fj_bi
            )
### stat rotation ----
if (isTRUE(rotation)){
    results.stats <-  append(results.stats,
             list(varimax.rotation.res = resVar,
                  rotated.Fj = Fj_rot,
                  rotated.Fi = Fi_rot)
                 )
}
### stat Biplot ----
if (isTRUE(biplot)){
    results.stats <-  append(results.stats,
             list(
                  Fi_biplot = Fi_bi,
                  Fj_biplot = Fj_bi)
                 )

}
# Question for graphs: separate ggplot2 from Recordplot
## graphs ----
results.graphs <- list(
                # cor and cov mat here
                rawData = a001.heatMap,
                centeredData = df.centered,
                centeredScaledData = a002n.heatMap,
                covariance = a5.02.covMap,
                correlation = a5.02.correlationMap,
                scree = a01.leScree,
                ctrI.1 = ctrI1,
                ctrI.2 = ctrI2,
            factorScoresI12 = a4.JolieggMap.2,
            cosineCircle4I12 = a02.jolieggMap.I,
            cosineCircleJ12  =  b1.jolieggMap.J,
            ctrJ.1 = ctrJ1,
            ctrJ.2 = ctrJ2,
            cosineCircleArrowJ12  =  b2.jolieggMap.J,
            cosineCircleArrowIJ12 = b2.jolieggMap.IJ,
            loadings12 = b3.jolieggMap.J.Q,
            loadings12.arrow = b3.jolieggMap.J.Q.arrow #,
            # biplot12 = e.JolieBiplot
                 )
description.graphs <- list(
    rawData = "The Row Data (Heat Map)",
    centeredData = "The Centered Data",
    centeredScaledData = "The Centered and Normalized Data",
    covariance = "The Covariance Matrix Heat Map",
    correlation = "The Correlation Matrix Heat Map",
    scree = "The Eigenvalues Scree Plot",
    ctrI.1 = "Observations: Contributions Dimension 1",
    ctrI.2 = "Observations: Contributions Dimension 2",
    factorScoresI12 =  "Observations: Factor Scores 1*2",
    cosineCircle4I12 = "Observations: Cosine Circle 1*2",
    cosineCircleJ12  = "Variables: Correlation Circle 1*2",
    ctrJ.1 = "Variables: Contributions Dimension 1",
    ctrJ.2 = "Variables: Contributions Dimension 2",
    cosineCircleArrowJ12  =  "Variables: Correlation Circle 1*2 (with arrows)",
    cosineCircleArrowIJ12 = "Variables & Observations: Correlation Circle 1*2",
    loadings12 = "Variables: Loadings as Inertia  1*2",
    loadings12.arrow = "Variables: Loadings as Weights  1*2" #,
    # biplot12 = e.JolieBiplot
)

### graph biplots ----
if (isTRUE(biplot)){
    results.graphs <-  append(results.graph,
                  list(biplots12 = e.JolieBiplot))
    description.graphs <- append(description.graphs,
                    list(biplots12 = "Biplot 1*2"))

}
## list stat & graphs ----
results <- list(results.stats = results.stats,
                results.graphs = results.graphs,
                description.graphs = description.graphs
                )
return(results)
# EOF ----
}

