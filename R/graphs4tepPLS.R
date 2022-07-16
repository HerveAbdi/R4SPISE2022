# Work file for PLS output ----
# Created  07/15/2022
# for package R4SPISE2022
# Current Version 07/15/2022
# Goal: generates standard graphs and tables for PLS
# Author Hervé
# Entête -----
# install.packages('sinew')
# sinew::makeOxygen(graph4tepPLS)
# Preamble ----
# Pass Results from Exposition
# ExPosition
#
# Documentation -----
#' @title graph4tepPLS run a PLS
#' (with the \code{tepPLS} function)
#' and generates the standard graphs and tables.
#' Note: *Still Under Development*.
#' @description \code{graph4tepPLS}
#' graph4tepPLS run a principal component
#' analysis
#' (with the \code{tepPLS} function)
#' and generates the standard graphs and tables.
#' Note that the parameters
#' \code{data, scale, center, DESIGN,
#'  make_design_nominal, k} are passed
#'  to  the \code{ExPosition} package unchanged
#'  except for \code{scale} which defaults now
#'  to \code{'SS1'}.
#' @param data A data frame or a matrix with
#' numerical data suitable for a PCA. Passed to
#' \code{TExPosition::tepPLS}.
#' @param scale  scale (i.e., normalize) the
#' columns of \code{data}.
#'  Default: \code{TRUE}. Values could
#'  be \code{TRUE, FALSE, 'Z', 'SS1'}. \code{TRUE}
#'  defaults to \code{SS1}.
#'  Passed to
#' \code{TExPosition::tepPLS}.
#'
#' @param center Default: TRUE.
#' do we center the columns of \code{data}
#' Passed to
#' \code{TExPosition::tepPLS}.
#' @param DESIGN  Default: NULL.
#' A design vector (could be factor or character)
#' or (Boolean) matrix used to assigne oobservations
#' to groups.
#' Passed to
#' \code{TExPosition::tepPLS}.
#' @param make_design_nominal
#' if TRUE (Default) transform
#' the vector from \code{DESIGN} into
#' a Boolean matrix.
#' Passed to
#' \code{TExPosition::tepPLS}.
#' @param k number
#' of factor to keep; when equql to
#' 0  (Default), all factors are kept.
#' Passed to
#' \code{TExPosition::tepPLS}.
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
#' Default) use colors from \code{TExPosition::tepPLS}.
#' @param col4J
#' a color vector for
#' plotting the variables (if \code{NULL}
#' Default) use colors from \code{TExPosition::tepPLS}.
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
#'  res4graph <- graph4tepPLS(data = df, scale = FALSE)
#'  }
#' }
#' @seealso
#'  \code{\link[TExPosition]{tepPLS}}
#'  \code{\link[PTCA4CATA]{PlotScree}}, \code{\link[PTCA4CATA]{createFactorMap}}
#' @rdname graph4tepPLS
#' @export
#' @import prettyGraphs
#' @import ggplot2 PTCA4CATA data4PCCAR corrplot
#' @importFrom TExPosition tepPLS
##  @importFrom
##  @importFrom PTCA4CATA PlotScree createFactorMap createxyLabels.gen
#' @importFrom grDevices colorRampPalette  dev.off  jpeg png recordPlot
#' @importFrom stats cor  cov varimax
# function ----
graph4tepPLS <- function(
        resPLS, # res from tepPLS
        data,# the data. No default
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
        title4pptx = "PLS Results" # title of pptx
){

    res.core <- coreTTAplot(title.plot = "PLS-C")

    return(results)
    # EOF ----
}

