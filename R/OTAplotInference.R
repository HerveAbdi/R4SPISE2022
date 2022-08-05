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
#' @param resPCA Output from epPCA
#' @param data A data frame or a matrix with
#' numerical data suitable for a PCA. Passed to
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
#' @param save2pptx  Default: FALSE
#' @param title4pptx Title of the PPTX, Default:
#' 'PCA Results'.
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
#'  \code{\link[ExPosition]{epPCA}}, \code{\link[InPosition]{epPCA.inference.battery}}
#'  \code{\link[PTCA4CATA]{PlotScree}}, \code{\link[PTCA4CATA]{createFactorMap}}
#' @rdname graph4epPCA
#' @export
#' @import prettyGraphs
#' @import ggplot2 PTCA4CATA data4PCCAR corrplot
#' @importFrom ExPosition epPCA
#' @importFrom InPosition epPCA.inference.battery
##  @importFrom PTCA4CATA PlotScree createFactorMap createxyLabels.gen
#'
#' @importFrom grDevices colorRampPalette  dev.off  jpeg png recordPlot
#' @importFrom stats cor  cov varimax
OTAplotInference <- function(
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
        niter.boot = 100,
        niter.perm = 100,
        save2pptx = FALSE,
        title4pptx = "PCA Inference Results"
    ) { # title of pptx
    printTest <- TRUE  # to debug the graphs
    printTest <- FALSE # to debug the graphs
    data <- ExPosition::expo.scale(
        data,
        center = resPCA$ExPosition.Data$center,
        scale = resPCA$ExPosition.Data$scale)
    # NB use namenameExpositionResults() to have
    # Dimensions named
    if (is.null(col4I)) {
        col4I <- resPCA$Plotting.Data$fi.col
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
    # graphs here -----
    ##  Scree ----
    # use PTCA4CATA::PlotScree
    ##### Inference and bootstrap
    infres <- epPCA.inference.battery(
        DATA = data, # data is preprocessed already
        center = FALSE,
        scale = FALSE,
        DESIGN = DESIGN,
        test.iters = niter.perm,
        graphs = FALSE)
    bootstapped.Eigenvalues <- Boot4Eigs(
        data = data, # data is preprocessed already
        center = FALSE,
        scale = FALSE,
        design = DESIGN,
        niter = niter.boot,
        CI.perc = c(0.025, 0.975),
        suppressProgressBar = TRUE)

    ## Scree with confidence intervals
    PlotScreeWithCI(
        ev = bootstapped.Eigenvalues$FixedEigs,
        ci.ev = bootstapped.Eigenvalues$BootMatEigsCI,
        ci.tau = bootstapped.Eigenvalues$BootMatTausCI,
        p.ev = infres$Inference.Data$components$p.vals)
    a01.leScree <- recordPlot()
    ## Boostrap ratios
    BR <- infres$Inference.Data$fj.boots$tests$boot.ratios
    rownames(BR) <- colnames(data)
    laDim = 1
    ba001.BR1 <- PTCA4CATA::PrettyBarPlot2(BR[,laDim],
                                threshold = 2,
                                font.size = 5,
                                color4bar = col4J, # we need hex code
                                main = paste0('PCA: Bootstrap ratio ',
                                    laDim),
                                ylab = 'Bootstrap ratios'
    )
    #
    laDim <- 2
    ba002.BR2 <- PTCA4CATA::PrettyBarPlot2(BR[,laDim],
                                threshold = 2,
                                font.size = 5,
                                color4bar = col4J, # we need hex code
                                main = paste0(
                                    'PCA: Bootstrap ratio ', laDim),
                                ylab = 'Bootstrap ratios'
    )

    ## graphs ----
    results.graphs <- list(
        scree = a01.leScree,
        BR1 = ba001.BR1,
        BR2 = ba002.BR2
    )
    description.graphs <- list(
        scree = "The Eigenvalues Scree Plot",
        BR1 = "Bootstrap Ratios for Dimension 1",
        BR2 = "Bootstrap Ratios for Dimension 2"
    )

    ## list stat & graphs ----
    results <- list(
        results.stats = NULL,
        results.graphs = results.graphs,
        description.graphs = description.graphs
    )

    if (save2pptx) {
        saveAllGraphsInList2pptx(
            list2Save = results.graphs,
            titles4list2Save = description.graphs,
            file2Save.pptx = "OTA.pptx",
            title = title4pptx
            )
    }

    return(results)
    # EOF ----
}
