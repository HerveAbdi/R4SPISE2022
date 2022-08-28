# Work file for PCA output ----
# Created  07/10/2022
# for package R4SPISE2022
# Current Version 07/10/2022
# Goal: generates standard graphs and tables for PCA
# Author Hervé
# Entête -----
# install.packages('sinew')
# sinew::makeOxygen(OTA)
# Preamble ----
# Pass Results from Exposition
# ExPosition
#
# Documentation -----
#' @title Create the inferential graphs from
#' results of a PCA run with
#' the package \code{InPosition}
#' Note: *Still Under Development*.
#' @description \code{OTAplotInference}
#' graph4epPCA  Create the inferential graphs from
#' results of a PCA run with
#' the package \code{InPosition}.
#' @param resPCA Output from
#' \code{InPosition::epCA.inference.battery}
#' @param data The data frame used for the PCA.
#' Note will be defunct in the next version.
#' @param DESIGN  Default: \code{NULL}.
#' A design vector (could be factor or character)
#' or (Boolean) matrix used to assigne oobservations
#' to groups.
#' @param make_design_nominal
#' if TRUE (Default) transform
#' the vector from \code{DESIGN} into
#' a Boolean matrix.
#' @param k number
#' of factor to keep; when equql to
#' 0  (Default), all factors are kept.
#' @param graphs  What graphs do we want?
#' Current Default is \code{12} which indicates that
#' the graphs are generated for the first
#' 2 components.
#' Note that current version
#' is creating output only for the first two
#' components,
#' @param printGraphs  (Default: \code{FALSE})
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
#'  @param col4I a color-name or a vector color-names
#'  (or correct dimensions) for the rows of the data
#'  (i.e., the I-set).
#'  @param col4J a color-name or a vector color-names
#'  (or correct dimensions) for the columns of the data
#'  (i.e., the J-set).
#' @param biplot (default \code{FALSE}) do we want
#' to compute and plot a biplot.
#' @param rotation
#' (default \code{FALSE}) do we want
#' to compute and plot a rotation.
#' @param niter.boot (default = 100)
#' How many iteration for the
#' bootstrap for the mean of the \code{DESIGN}
#' variable.
#' @param niter.perm (default = 100)
#' How many iteration for the
#' permutation test for the eigenvalues.
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
#' @importFrom grDevices colorRampPalette dev.off jpeg png recordPlot
#' @importFrom stats cor cov varimax
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
            file2Save.pptx = paste0(title4pptx, ".pptx"),
            title = title4pptx
            )
    }

    return(results)
    # EOF ----
}
