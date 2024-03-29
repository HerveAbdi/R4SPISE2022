# Work file for PCA output ----
# Created  07/10/2022
# for package R4SPISE2022
# Current Version 07/10/2022
# Goal: generates standard graphs and tables for PCA
# Author Hervé
# Entête -----
# install.packages('sinew')
# sinew::makeOxygen(graph4epPCA)
# Preamble ----
# Pass Results from Exposition
# ExPosition
#
# Documentation -----
#' @title
#' create the graphs for a PCA analysis
#' from \code{ExPosition}
#' and generate the standard graphs and tables.
#' Note: *Still Under Development*.
#' @description \code{OTAPlot}
#' creates the graphs for a PCA analysis
#' from \code{ExPosition}.
#' Note: *Still Under Development*.
#' @param resPCA Output from \code{ExPosition::epPCA()}
#' @param data the data frame or a matrix with
#' numerical data suitable for a PCA
#' that was used by
#' \code{ExPosition::epPCA}.
#' @param DESIGN  Default: NULL.
#' A design vector (could be factor or character)
#' or (Boolean) matrix used to assign observations
#' to groups.
#' @param make_design_nominal
#' if TRUE (Default) transform
#' the vector from \code{DESIGN} into
#' a Boolean matrix.
#' Passed to
#' \code{ExPosition::epPCA}.
#' @param k number
#' of factor to keep; when equal to
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
#' @param show.TI whether to plot the tolerance intervals or not. Default: FALSE
#' @param show.CI whether to plot the confidence intervals or not. Default: TRUE
#' @param mean.cex the size of the dots of the means. Default: 3
#' @param mean.textcex the size of the texts of the means. Default: 3
#' @param display.labels.ind If TRUE, the labels of observations will be printed. Default: FALSE.
#' @param display.labels.var If TRUE, the labels of variables will be printed. Default: TRUE.
#' @param display.points.mean If TRUE, the mean factor scores will be plotted. Default: TRUE.
#' @param mean.constraints A list of the constraints (that include \code{minx}, \code{miny}, \code{maxx}, and \code{maxy})
#' The constraints of the figure that only includes the means. This constraints
#' will be used if \code{only.mean = TRUE}. Default: NULL
#' @param scale.mean.constraints A value used to scale the constraints (by multiplication).
#' This function is used to adjust the constraints when the confidence or the tolerance intervals are outside of the figure.
#' Default: 1.5
#' @param max.n4bar When the number of bars exceed this value, the labels will be hidden. Default: 40.
#' @param max.n4heat When the number of row/columns of a heatmap exceed this value, the labels will be hidden. Default: 50.
#' @param title.size.heatmap the size of the title of the heatmaps. Default: 20.
#' @param save2pptx  Default: FALSE
#' @param title4pptx Title of the PPTX, Default:
#' 'PCA Results'.
#' @return A list made of two lists
#'
#' @details Work in Progress
#' @author Herve Abdi
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
#'  \code{\link[ExPosition]{epPCA}},
#'  \code{\link[PTCA4CATA]{PlotScree}},
#'  \code{\link[PTCA4CATA]{createFactorMap}}
#' @rdname graph4epPCA
#' @export
#' @import prettyGraphs
#' @import ggplot2 PTCA4CATA data4PCCAR corrplot
#' @importFrom ExPosition epPCA
##  @importFrom
##  @importFrom PTCA4CATA PlotScree createFactorMap createxyLabels.gen
#'
#' @importFrom grDevices colorRampPalette  dev.off  jpeg png recordPlot
#' @importFrom stats cor cov varimax
#' @importFrom ExPosition makeNominalData
OTAplot <- function(
        resPCA,
        data,
        DESIGN = NULL,
        make_design_nominal = TRUE,
        k = 0,
        graphs = 12,
        printGraphs = FALSE,
        col4I = NULL,
        col4J = NULL,
        biplot   = FALSE,
        rotation = FALSE,
        nfactor4rotation = 'Kaiser',
        show.TI = FALSE,
        show.CI = TRUE,
        mean.cex = 3,
        mean.textcex = 3,
        display.labels.ind = FALSE,
        display.labels.var = TRUE,
        display.points.mean = TRUE,
        mean.constraints = NULL,
        scale.mean.constraints = 1.5,
        max.n4bar = 40,
        max.n4heat = 50,
        title.size.heatmap = 20,
        save2pptx = FALSE,
        title4pptx = "PCA Results"
    ) { # title of pptx
    printTest <- TRUE  # to debug the graphs
    printTest <- FALSE # to debug the graphs
    # print option for ggrepel
    options(ggrepel.max.overlaps = Inf)
    if (isTRUE(scale)) {
        scale = 'SS1'
    }
    # NB use namenameExpositionResults() to have
    # Dimensions named
    if (is.null(col4I)) {
        if (is.null(DESIGN)){
            col4I <- resPCA$Plotting.Data$fi.col
        }else{
            col4I <- createColorVectorsByDesign(
                ExPosition::makeNominalData(as.matrix(DESIGN)))$oc
        }
    }
    if (is.null(col4J)) {
        col4J <- resPCA$Plotting.Data$fj.col
    }
    laMat <- resPCA$ExPosition.Data$X
    # get nfact for rotation
    if (isTRUE(rotation)) {
        eigs <-  resPCA$ExPosition.Data$eigs[resPCA$ExPosition.Data$eigs >
                                                 100 * .Machine$double.eps]
        nK <- length(eigs)
        if (tolower(nfactor4rotation) == 'kaiser') {
            nfactor4rotation <- sum(eigs > mean(eigs))
            if (nfactor4rotation == 1)
                nfactor4rotation <- 2
        }
        if (nfactor4rotation > nK)
            nfactor4rotation <- 2
    }
    if ((is.data.frame(DESIGN)|is.matrix(DESIGN)) && ncol(DESIGN) > 1){
        DESIGN <- DESIGN %*% diag(1:ncol(DESIGN)) |> rowSums()
    }else if((is.data.frame(DESIGN)|is.matrix(DESIGN)) && ncol(DESIGN) == 1){
        DESIGN <- as.vector(as.matrix(DESIGN))
    }
    #_________________________________________________
    # Get the loadings ----
    # But, where are the loadings?
    #  And which ones of the loadings?
    ## 1. Slices of Inertia ----
    loadings.1 <- resPCA$ExPosition.Data$fj
    ## 2. Correlations ----
    loadings.2 <- cor(data, resPCA$ExPosition.Data$fi)
    # Loadings as coefficients
    # of the optimal linear combination
    cor4print <- loadings.2
    row.names(cor4print) <- paste0('f', 1:nrow(cor4print))
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
    Fi_scaled   <-
        Fi %*% diag(delta ^ (-1 / 2))  # P*D^(1/4)
    Fj_scaled   <-
        Q  %*% diag(delta ^ (1 / 2))  # Q*D^(1/4)
    # # F*_scaled re-scaled sqrt(N)
    Fi_scaled_N <-
        Fi_scaled  / sqrt(NROW(Fi_scaled))
    Fj_scaled_N <-
        Fj_scaled  / sqrt(NROW(Fj_scaled))
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
    jolie.ggplot1.transparent <- PTCA4CATA::createFactorMap(
        resPCA$ExPosition.Data$fi,
        col.points = col4I,
        col.labels = col4I,
        display.labels = display.labels.ind,
        alpha.points = 0.2,
        font.face = 'italic'
    )
    # Create the labels for Inertia per dimension
    label4Map <- PTCA4CATA::createxyLabels.gen(1,
                                    2,
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
                       theme(
                           axis.title = element_text(
                               # the new theme
                               color = "darkorchid4",
                               size = rel(1.1),
                               # relative to default
                               # family = 'Times',
                               # "Times", "sans", "Courier"
                               face   = "italic" ,
                               # 'plain','italic', 'bold',
                               # NB: face does not work with current ggplot2
                           ),
                           # end of element_text
                           plot.title = element_text(color = '#5826A3')
                       ))

    a4.JolieggMap <-
        jolie.ggplot1$zeMap + label4Map2
    if (printTest) {
        print(a4.JolieggMap)
    }
    # print(a4.JolieggMap)
    a4.JolieggMap.2 <-
        a3.JolieggMap + label4Map2

    if (printGraphs) {
        png('ObservationImap.png')
        print(a4.JolieggMap.2)
        dev.off()
    }

    # to look at the map with or without the means

    if (!is.null(DESIGN)) {
        fi.mean <- getMeans(resPCA$ExPosition.Data$fi, DESIGN)
        colnames(fi.mean) <- paste0("Dimension ", 1:ncol(fi.mean))
        if (is.null(mean.constraints)) {
            mean.constraints <- lapply(minmaxHelper(fi.mean), '*', scale.mean.constraints)
        }

        grpidx.tmp <- tapply(seq_along(col4I), col4I, identity)[unique(col4I)]
        grpidx <- sapply(grpidx.tmp, "[[", 1)
        col4CI <- names(grpidx)
        names(col4CI) <- DESIGN[grpidx]
        # reorder according to fi.mean
        col4CI <- col4CI[rownames(fi.mean)]

        plot.fi.mean <- createFactorMap(fi.mean,
                                        col.background = NULL,
                                        col.axes = "orchid4",
                                        alpha.axes = 0.5,
                                        title = "The Observation Map.",
                                        col.points = col4CI[rownames(fi.mean)],
                                        col.labels =  col4CI[rownames(fi.mean)],
                                        constraints = mean.constraints,
                                        cex = mean.cex,
                                        text.cex = mean.textcex,
                                        pch = 17,
                                        alpha.points = 0.8)

        bootCI.res <- Boot4Mean(resPCA$ExPosition.Data$fi, DESIGN)
        colnames(bootCI.res$BootCube) <- paste0("Dimension ", 1:ncol(resPCA$ExPosition.Data$fi))
        fi.CI <- MakeCIEllipses(bootCI.res$BootCube,
                                col = col4CI,
                                alpha.ellipse = 0.1,
                                line.size = 0.5, alpha.line = 0.2)

        data4TI <- resPCA$ExPosition.Data$fi
        colnames(data4TI) <- paste0("Dimension ", 1:ncol(data4TI))
        fi.TI <- MakeToleranceIntervals(data4TI,
                                        design = DESIGN,
                                        axis1 = 1, axis2 = 2,
                                        col = col4CI,
                                        line.size = 1,
                                        alpha.ellipse = 0.05,
                                        alpha.line = 0.3,
                                        p.level = .80)
        a5.JolieggMap <- jolie.ggplot1.transparent$zeMap + label4Map2 + plot.fi.mean$zeMap_dots + plot.fi.mean$zeMap_text

        if (show.TI) {
            a5.JolieggMap.Fi.TI <- jolie.ggplot1.transparent$zeMap + label4Map2 + fi.TI + plot.fi.mean$zeMap_dots + plot.fi.mean$zeMap_text
            a5.JolieggMap.Fimean.TI <- jolie.ggplot1.transparent$zeMap_background + label4Map2 + fi.TI + plot.fi.mean$zeMap_dots + plot.fi.mean$zeMap_text
            if (printGraphs) {
                png('ObservationImapWithTI.png')
                print(a5.JolieggMap.Fi.TI)
                dev.off()

                png('ObservationImapWithTI(MeansOnly).png')
                print(a5.JolieggMap.Fimean.TI)
                dev.off()
            }
        }
        if (show.CI) {
            a5.JolieggMap.Fi.CI <- jolie.ggplot1.transparent$zeMap + label4Map2 + fi.CI + plot.fi.mean$zeMap_dots + plot.fi.mean$zeMap_text
            a5.JolieggMap.Fimean.CI <- jolie.ggplot1.transparent$zeMap_background + label4Map2 + fi.CI + plot.fi.mean$zeMap_dots + plot.fi.mean$zeMap_text
            if (printGraphs) {
                png('ObservationImapWithCI.png')
                print(a5.JolieggMap.Fi.CI)
                dev.off()

                png('ObservationImapWithCI(MeansOnly).png')
                print(a5.JolieggMap.Fimean.CI)
                dev.off()
            }
        }
        if (show.TI == TRUE & show.CI == TRUE) {
            a5.JolieggMap.Fi.CITI <- jolie.ggplot1.transparent$zeMap + label4Map2 + fi.TI + fi.CI + plot.fi.mean$zeMap_dots + plot.fi.mean$zeMap_text
            a5.JolieggMap.Fimean.CITI <- jolie.ggplot1.transparent$zeMap_background + label4Map2 + fi.TI + fi.CI + plot.fi.mean$zeMap_dots + plot.fi.mean$zeMap_text
            if (printGraphs) {
                png('ObservationImapWithCITI.png')
                print(a5.JolieggMap.Fi.CITI)
                dev.off()

                png('ObservationImapWithCITI(MeansOnly).png')
                print(a5.JolieggMap.Fimean.CITI)
                dev.off()
            }
        }




    }


    ### Contributions ----
    signed.ctrI <- resPCA$ExPosition.Data$ci *
        sign(resPCA$ExPosition.Data$fi)
    if (!is.null(DESIGN)){
        signed.ctrI <- signed.ctrI[order(DESIGN),]
    }
    ### ctr4I -----
    leStem <-  'I-Set'
    nfac  <- 1
    nind <- nrow(signed.ctrI)
    if (nind < 10){
        font.size.ctr <- 5
    }else {
        font.size.ctr = 5-(round((nind-10)/10)-1)
    }
    ctrI1 <- plotCtr(signed.ctrI, col4I,
                     nfac, stem = leStem,
                     font.size = font.size.ctr)

    nfac  <- 2
    ctrI2 <- plotCtr(signed.ctrI, col4I,
                     nfac, stem = leStem,
                     font.size = font.size.ctr)

    if (nind > max.n4bar){
        ctrI1$layers[[3]] <- NULL
        ctrI2$layers[[3]] <- NULL
    }
    ### I-Cosines -----
    cos4I <- sqrt(resPCA$ExPosition.Data$ri) *
        sign(resPCA$ExPosition.Data$fi)
    ### 1.Iset - Circle of corr ----
    jolie.ggplot.I <-
        PTCA4CATA::createFactorMap(
            cos4I,
            col.points = col4I,
            col.labels = col4I,
            display.labels = display.labels.ind,
            constraints = list(
                minx = -1,
                miny = -1,
                maxx = 1 ,
                maxy = 1
            ),
            font.face = "italic"
        )
    # draw the circle
    a01.jolieggMap.I <- jolie.ggplot.I$zeMap +
        addCircleOfCor(color = "darkorchid4")
    if (printGraphs) {
        print(a01.jolieggMap.I)
    }
    #  Dot for the I-set, no arrows
    a02.jolieggMap.I <-
        jolie.ggplot.I$zeMap_background +
        jolie.ggplot.I$zeMap_text +
        addCircleOfCor(color = "darkorchid4") +
        jolie.ggplot.I$zeMap_dots  + label4Map2
    if (printTest) {
        print(a02.jolieggMap.I)
    }
    if (printGraphs) {
        png('CircleOfCorrISet.png')
        print(a02.jolieggMap.I)
        dev.off()
    }
    #________________________________________________
    ### A heat map ----
    ####  V1a data raw ----
    font.size.heatmap.x <- 14*(2/(ncol(data)))
    font.size.heatmap.y <- 8*(20/(nrow(data)))

    a001.heatMap <- suppressWarnings(
        makeggHeatMap4CT(
            data,
            colorProducts = c(col4I, col4I),
            colorAttributes = c(col4J, col4J),
            fontSize.x = font.size.heatmap.x,
            fontSize.y = font.size.heatmap.y,
            face.x = 'bold',
            face.y = 'italic',
        )
    )  +
        ggtitle("Mean Intensity") +
        theme(plot.title = element_text(
            family = "Helvetica",
            face   = "bold",
            size = title.size.heatmap
        ))
    if (ncol(data) > max.n4heat){
        a001.heatMap <- a001.heatMap + theme(axis.text.x = element_blank())
    }
    if (nrow(data) > max.n4heat) {
        a001.heatMap <- a001.heatMap + theme(axis.text.y = element_blank())
    }

    if (printGraphs) {
        png('JIntensityHeatMap.png')
        print(a001.heatMap)
        dev.off()
    }
    if (printTest) {
        a001.heatMap
    }
    ####  V1b data centered ----
    df.centered  <-
        apply(data, 2, scale0, scale = FALSE)
    a002.heatMap <- suppressWarnings(
        makeggHeatMap4CT(
            df.centered,
            colorProducts = col4I,
            colorAttributes = col4J,
            fontSize.x = font.size.heatmap.x,
            fontSize.y = font.size.heatmap.y,
            face.x = 'bold',
            face.y = 'italic',
        )
    )  +
        ggtitle("Centered Data") +
        theme(plot.title = element_text(
            family = "Helvetica",
            face = "bold",
            size = title.size.heatmap
        ))
    if (ncol(data) > max.n4heat){
        a002.heatMap <- a002.heatMap + theme(axis.text.x = element_blank())
    }
    if (nrow(data) > max.n4heat) {
        a002.heatMap <- a002.heatMap + theme(axis.text.y = element_blank())
    }
    if (printTest) {
        df.centered
    }
    if (printGraphs) {
        png('JIntensityHeatMapCentered.png')
        print(a002.heatMap)
        dev.off()
    }
    ####  V1c df normed ----
    df.normed <-
        apply(data, 2, scale0, scale = 'SS1')
    a002n.heatMap <- suppressWarnings(
        makeggHeatMap4CT(
            df.normed,
            colorProducts = col4I,
            colorAttributes = col4J,
            fontSize.x = font.size.heatmap.x,
            fontSize.y = font.size.heatmap.y,
            face.x = 'bold',
            face.y = 'italic',
        )
    )  +
        ggtitle("Centered and Scaled Data") +
        theme(plot.title = element_text(
            family = "Helvetica",
            face = "bold",
            size = title.size.heatmap
        ))
    if (ncol(data) > max.n4heat){
        a002n.heatMap <- a002n.heatMap + theme(axis.text.x = element_blank())
    }
    if (nrow(data) > max.n4heat) {
        a002n.heatMap <- a002n.heatMap + theme(axis.text.y = element_blank())
    }
    if (printTest) {
        a002n.heatMap
    }
    if (printGraphs) {
        png('JIntensityHeatMapNormed.png')
        print(a002n.heatMap)
        dev.off()
    }

    # Covariance ----
    nI <- nrow(data)
    Smat <- cov(data) #  *(  (nI - 1) / N)
    # covariance with N - 1
    # Covariance mat ----
    ### 0. Cov Mat
    col <-
        colorRampPalette(c("#BB4444", "#EE9988",
                           "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(
        Smat,
        is.corr = FALSE,
        method = "color",
        # "number"
        col = col(200),
        tl.col = col4J,
        pch.col = col(200),
        type = "upper",
        order = "hclust",
        addCoef.col =  "grey",
        #"black",
        addCoefasPercent	= TRUE,
        # Add coefficient of correlation
        tl.srt = 45,
        tl.cex = (10/ncol(Smat)),
        number.font = (10/ncol(Smat)),
        #Text label color and rotation
        # Combine with significance
        #p.mat = p.mat,
        # sig.level = 0.01,
        insig = "blank",
        # variance in the principal diagonal
        diag = TRUE
    )
    a5.02.covMap <- recordPlot()
    if (printTest) {
        a5.02.covMap
    }
    if (printGraphs) {
        png('JbyJ-Covariance.png')
        print(a5.02.covMap)
        dev.off()
    }
    # Correlation mat ----
    Rmat <- cor(data) #
    corrplot(
        Rmat,
        is.corr = TRUE,
        method = "color",
        # "number"
        col = col(200),
        tl.col = col4J,
        pch.col = col(200),
        type = "upper",
        order = "hclust",
        addCoef.col =  "grey",
        #"black",
        addCoefasPercent	= TRUE,
        # Add coefficient of correlation
        tl.srt = 45,
        tl.cex = (10/ncol(Rmat)),
        number.font = (10/ncol(Rmat)),
        #Text label color and rotation
        # Combine with significance
        #p.mat = p.mat,
        # sig.level = 0.01,
        insig = "blank",
        # variance in the principal diagonal
        diag = TRUE
    )
    a5.02.correlationMap <- recordPlot()
    if (printGraphs) {
        png('JbyJ-Correlation.png')
        print(a5.02.correlationMap)
        dev.off()
    }
    if (printTest) {
        a5.02.correlationMap
    }
    #_________________________________________________
    ### 1. Circle of corr ----
    # Circle with PTCA: Use loading.2: the correlation
    # Create the map
    jolie.ggplot.J <-
        PTCA4CATA::createFactorMap(
            loadings.2,
            col.points = col4J,
            col.labels = col4J,
            display.labels = display.labels.var,
            constraints = list(
                minx = -1,
                miny = -1,
                maxx = 1 ,
                maxy = 1
            )
        )
    # draw the circle
    b1.jolieggMap.J <- jolie.ggplot.J$zeMap +
        addCircleOfCor(color = "darkorchid4") + label4Map2
    if (printGraphs) {
        png('J-CircleOfCorr_noArrow.png')
        print(b1.jolieggMap.J)
        dev.off()
    }
    #  Add some arrows
    arrows <-
        addArrows(loadings.2, color = col4J)
    b2.jolieggMap.J <-
        jolie.ggplot.J$zeMap_background +
        jolie.ggplot.J$zeMap_text +
        addCircleOfCor(color = "darkorchid4") +
        arrows + label4Map2
    # print(b2.jolieggMap.J)
    if (printGraphs) {
        png('J-CircleOfCorr.png')
        print(b2.jolieggMap.J)
        dev.off()
    }
    ## I & J Circle -----
    b2.jolieggMap.IJ <- b2.jolieggMap.J +
        jolie.ggplot.I$zeMap_text +
        jolie.ggplot.I$zeMap_dots + label4Map2
    #print(b2.jolieggMap.IJ)
    if (printGraphs) {
        png('IJ-CircleOfCorr.png')
        print(b2.jolieggMap.IJ)
        dev.off()
    }
    ## Get ctr as corr
    corJ_PC <- round(loadings.2 ^ 2, 2)
    colnames(corJ_PC) <-
        paste0('f', 1:ncol(corJ_PC))

    ### 2. Loadings as Q ----
    jolie.ggplot.J.Q <-
        PTCA4CATA::createFactorMap(
            loadings.3,
            col.points = col4J,
            col.labels = col4J
        )
    arrows.Q <-
        addArrows(loadings.3, color = col4J)
    b3.jolieggMap.J.Q <-
        jolie.ggplot.J.Q$zeMap +
        #arrows.Q +
        label4Map2
    b3.jolieggMap.J.Q.arrow <-
        jolie.ggplot.J.Q$zeMap +
        arrows.Q +
        label4Map2
    # print(b3.jolieggMap.J.Q)
    if (printGraphs) {
        png('J-LoadingAsQ.png')
        print(b3.jolieggMap.J.Q)
        dev.off()
    }
    if (printGraphs) {
        png('J-LoadingAsQArrows.png')
        print(b3.jolieggMap.J.Q.arrow)
        dev.off()
    }
    ### 3. Loadings as Fj ----
    jolie.ggplot.J.fj <-
        PTCA4CATA::createFactorMap(
            loadings.1,
            display.labels = display.labels.var,
            col.points = col4J,
            col.labels = col4J
        )
    arrows.fj <-
        addArrows(loadings.1, color = col4J)
    b3.jolieggMap.J.fj <-
        jolie.ggplot.J.fj$zeMap +
        #arrows.Q +
        label4Map2
    b3.jolieggMap.J.fj.arrow <-
        jolie.ggplot.J.fj$zeMap +
        arrows.fj +
        label4Map2
    # print(b3.jolieggMap.J.Q)
    if (printGraphs) {
        png('J-LoadingAsFj.png')
        print(b3.jolieggMap.J.fj)
        dev.off()
    }
    if (printGraphs) {
        png('J-LoadingAsFjArrows.png')
        print(b3.jolieggMap.J.fj.arrow)
        dev.off()
    }
    # Contributions ----
    signed.ctrJ <- resPCA$ExPosition.Data$cj *
        sign(resPCA$ExPosition.Data$fj)
    ## plot ctr ----
    # plot contributions for component 1
    col4J.bar <- col4J
    #col4J.bar[3] <- '#2F7D1F'
    nfac  <- 1
    nvar <- nrow(signed.ctrJ)
    if (nvar < 10){
        font.size.ctr <- 5
    }else {
        font.size.ctr = 5-(round((nvar-10)/10)-1)
    }

    ctrJ1 <-
        plotCtr(signed.ctrJ, col4J.bar, nfac,
                font.size = font.size.ctr)
    nfac  <- 2
    ctrJ2 <-
        plotCtr(signed.ctrJ, col4J.bar, nfac,
                font.size = font.size.ctr)
    if (nvar > max.n4bar){
        ctrJ1$layers[[3]] <- NULL
        ctrJ2$layers[[3]] <- NULL
    }


    # ***Rotation Here ----
    if (isTRUE(rotation)) {
        # rotation
        resVar <-
            varimax(Fj[, 1:nfactor4rotation],
                    normalize = FALSE)
        Fj_rot <-
            Fj[, 1:nfactor4rotation] %*% resVar$rotmat
        Fi_rot <-
            Fi[, 1:nfactor4rotation] %*% resVar$rotmat
        ## Map for rotation here ----
        ## End of rotation map
    } # end Rotation
    #_________________________________________________
    #__________________-----
    # **** If Biplot ****
    if (isTRUE(biplot)) {
        # start Biplot
        # Biplot ----
        ## Scaled by sqrt(Delta/sqrt(N)) ----
        ### theme graph ----
        col4labels <- "darkorchid4"
        label4Map4Bi <- list(
            label4Map,
            # The standard label from createxyLabels.gen
            theme(
                axis.title = element_text(
                    # the new theme
                    color = col4labels,
                    size = rel(1.2),
                    # relative to default
                    # family = 'sans',
                    # "Times", "sans", "Courier"
                    face   = "italic" ,
                    # 'plain','italic', 'bold',
                    # NB: face does not work with current ggplot2
                ),
                # end of element_text
                axis.text = element_text(size  = 12,
                                         color = col4labels),
                plot.title = element_text(color = '#5826A3')
            )
        )
        # get constraints
        # Fi_bi & Fj_bi
        lesContraintes <-
            minmaxHelper(Fi_bi, Fj_bi)
        # look at the help for PTCA4CATA::createFactorMap
        sizeFontI  <- 5
        jolie.biplot.I <-
            PTCA4CATA::createFactorMap(
                Fi_bi,
                col.points = col4I,
                col.labels = col4I,
                font.face = 'italic',
                constraints = lesContraintes,
                text.ce = sizeFontI
            )
        jolie.biplot.J <-
            PTCA4CATA::createFactorMap(
                Fj_bi,
                col.points = col4J,
                col.labels = col4J,
                constraints = lesContraintes
            )
        # add arrows
        arrows.Bi <-
            addArrows((Fj_bi), color = col4J)
        e.JolieBiplot <- jolie.biplot.I$zeMap +
            jolie.biplot.J$zeMap_text + arrows.Bi +
            label4Map2 +
            labs(title = 'The Biplot Map.')
        # to look at the map
        print(e.JolieBiplot)
        if (printGraphs) {
            png('Biplot_FiFj.png')
            print(e.JolieBiplot)
            dev.off()
        }

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
    if (isTRUE(rotation)) {
        results.stats <-  append(
            results.stats,
            list(
                varimax.rotation.res = resVar,
                rotated.Fj = Fj_rot,
                rotated.Fi = Fi_rot
            )
        )
    }
    ### stat Biplot ----
    if (isTRUE(biplot)) {
        results.stats <-  append(results.stats,
                                 list(Fi_biplot = Fi_bi,
                                      Fj_biplot = Fj_bi))

    }
    # Question for graphs: separate ggplot2 from Recordplot
    ## graphs ----
    results.graphs <- list(
        # cor and cov mat here
        covariance = a5.02.covMap,
        correlation = a5.02.correlationMap,
        scree = a01.leScree,
        ctrI.1 = ctrI1,
        ctrI.2 = ctrI2,
        factorScoresI12 = a4.JolieggMap.2,
        factorScoresJ12 = b3.jolieggMap.J.fj,
        factorScoresJ12.arrow = b3.jolieggMap.J.fj.arrow,
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
        covariance = "The Covariance Matrix Heat Map",
        correlation = "The Correlation Matrix Heat Map",
        scree = "The Eigenvalues Scree Plot",
        ctrI.1 = "Observations: Contributions Dimension 1",
        ctrI.2 = "Observations: Contributions Dimension 2",
        factorScoresI12 =  "Observations: Factor Scores 1*2",
        factorScoresJ12 = "Variables: Loadings as Inertia 1*2",
        factorScoresJ12.arrow = "Variables: Loadings as Inertia 1*2 (with arrows)",
        cosineCircle4I12 = "Observations: Cosine Circle 1*2",
        cosineCircleJ12  = "Variables: Correlation Circle 1*2",
        ctrJ.1 = "Variables: Contributions Dimension 1",
        ctrJ.2 = "Variables: Contributions Dimension 2",
        cosineCircleArrowJ12  =  "Variables: Correlation Circle 1*2 (with arrows)",
        cosineCircleArrowIJ12 = "Variables & Observations: Correlation Circle 1*2",
        loadings12 = "Variables: Loadings as Weights  1*2",
        loadings12.arrow = "Variables: Loadings as Weights  1*2 (with arrows)" #,
        # biplot12 = e.JolieBiplot
    )

    if (max(ncol(data), nrow(data)) < max.n4heat) {
        results.graphs$rawData = a001.heatMap
        results.graphs$centeredData = a002.heatMap
        results.graphs$centeredScaledData = a002n.heatMap
        description.graphs$rawData = "The Row Data (Heat Map)"
        description.graphs$centeredData = "The Centered Data"
        description.graphs$centeredScaledData = "The Centered and Normalized Data"
    }

    if (!is.null(DESIGN)){
        results.graphs$factorScoresI12design = a5.JolieggMap
        description.graphs$factorScoresI12design = "Observations with design: Factor Scores 1*2"
        if (show.TI) {
            results.graphs$factorScoresI12design.TI = a5.JolieggMap.Fi.TI
            results.graphs$meanfactorScoresI12design.TI = a5.JolieggMap.Fimean.TI
            description.graphs$factorScoresI12design.TI = "Observations with tolerance intervals: Factor Scores 1*2"
            description.graphs$meanfactorScoresI12design.TI = "Observations with tolerance intervals: Mean Factor Scores 1*2"
        }
        if (show.CI) {
            results.graphs$factorScoresI12design.CI = a5.JolieggMap.Fi.CI
            results.graphs$meanfactorScoresI12design.CI = a5.JolieggMap.Fimean.CI
            description.graphs$factorScoresI12design.CI = "Observations with confidence intervals: Factor Scores 1*2"
            description.graphs$meanfactorScoresI12design.CI = "Observations with confidence intervals: Mean Factor Scores 1*2"
        }
        if (show.TI == TRUE & show.CI == TRUE) {
            results.graphs$factorScoresI12design.CITI = a5.JolieggMap.Fi.CITI
            results.graphs$meanfactorScoresI12design.CITI = a5.JolieggMap.Fimean.CITI
            description.graphs$factorScoresI12design.CITI = "Observations with CI and TI: Factor Scores 1*2"
            description.graphs$meanfactorScoresI12design.CITI = "Observations with CI and TI: Mean Factor Scores 1*2"
        }
    }

    ### graph biplots ----
    if (isTRUE(biplot)) {
        results.graphs <-  append(results.graphs,
                                  list(biplots12 = e.JolieBiplot))
        description.graphs <-
            append(description.graphs,
                   list(biplots12 = "Biplot 1*2"))

    }
    ## list stat & graphs ----
    results <- list(
        results.stats = results.stats,
        results.graphs = results.graphs,
        description.graphs = description.graphs
    )

    if (save2pptx) {
        saveAllGraphsInList2pptx(
            list2Save = results.graphs,
            titles4list2Save = description.graphs,
            file2Save.pptx = paste0(title4pptx, ".pptx"),
            title = title4pptx
        )
    }

    return(results)
    # EOF ----
}
