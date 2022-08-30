#' Core function to generate two-table analysis plots.
#'
#' @param res the result of a two-table analysis;
#' @param method the method used; 'PLSC' or 'CCA' (default to 'PLSC');
#' @param tab1 the first data table;
#' @param tab2 the second data table;
#' @param center1 how the variables in tab1 is centered (default to TRUE);
#' @param scale1 how the variables in tab1 is centered (default to "ss1");
#' @param center2 how the variables in tab1 is centered (default to TRUE);
#' @param scale2 how the variables in tab1 is centered (default to "ss1");
#' @param leDim vector containning the dimensions to be plotted;
#' @param leDim4CirCor vector containning the dimensions to be correlated with the latent variables for the circle of correlation plot;
#' @param color.obs list containing the colors for the observations;
#' @param color.tab list containing the colors for the two-tables;
#' @param title.plot plot of the title;
#' @param alpha.points opacity for points;
#' @param DESIGN design for the observations;
#' @param sigbar.p whether to plot only significant bars or not for the contribution plot of the first block (default to FALSE);
#' @param sigbar.q whether to plot only significant bars or not for the contribution plot of the second block (default to FALSE);
#' @param tab1.name the name of the first table (default to "1");
#' @param tab2.name the name of the second table (default to "2");
#' @param TI whether to plot toerance intervals (default to FALSE);
#' @param mean.cex size expansion for the mean symbols;
#' @param mean.textcex size expansion for the mean symbols;
#' @param only.mean whether to plot only means;
#' @param only.ind whether to plot only observations on the latent variable plot (default to FALSE);
#' @param score.constraints constraints on the plot (use with care);
#' @param mean.constraints constraints on the plot (use with care);
#' @param scale.mean.constraints constraints on the plot (use with care).
#' @param save2pptx  Default: FALSE
#' @param title4pptx Title of the PPTX, Default: 'TTA Results'.
#'
#' @return a list of plots
#' @export
#' @import ggplot2 PTCA4CATA data4PCCAR corrplot
#'
#' @examples
#' \dontrun{
#'   # An example will be added very soon
#' }
#'
TTAplotInference <- function(
        res,
        method = 'PLSC',
        tab1,
        tab2,
        center1 = TRUE,
        scale1 = "ss1",
        center2 = TRUE,
        scale2 = "ss1",
        leDim = c(1, 1),
        leDim4CirCor = c(1, 2),
        color.obs = NULL,
        color.tab = NULL,
        title.plot = "Results",
        alpha.points = 0.25,
        DESIGN = NULL,
        sigbar.p = FALSE,
        sigbar.q = FALSE,
        tab1.name = "J-set",
        tab2.name = "K-set",
        TI = FALSE,
        mean.cex = 3,
        mean.textcex = 3,
        only.mean = FALSE,
        only.ind = FALSE,
        score.constraints = NULL,
        mean.constraints = NULL,
        scale.mean.constraints = 3,
        save2pptx = FALSE,
        title4pptx = "TTA Inference Results") {

  # res: two lists (lx and ly) each with two obs x var. matrices of factor scores
  # leDim: a vector the component to plot for each table. e.g., c(component of table1, component of table2)
  # color.obs: a list that contains oc (colors for individual observations) and gc (colors for individuals' groups)
  # color.tab: a list that contains oc (colors for individual variables) and gc (colors for tables)
  # title.plot: title of the plot
  # DESIGN: a vector with group variables
  # tab1.name: name for table 1
  # tab2.name: name for table 2
  # bar.font.size: a vector with font sizes for labels in barplots for respectively x and y
  rxy <- res$TExPosition.Data$X
  I <- nrow(rxy)
  J <- ncol(rxy)

  if ((is.data.frame(DESIGN)|is.matrix(DESIGN)) && ncol(DESIGN) > 1){
      DESIGN <- DESIGN %*% diag(1:ncol(DESIGN)) |> rowSums()
  }else if((is.data.frame(DESIGN)|is.matrix(DESIGN)) && ncol(DESIGN) == 1){
      DESIGN <- as.vector(as.matrix(DESIGN))
  }

  ## PLSC inference
  if (method == "PLSC"){
      # 1. Permutation ----
      resPerm <- data4PCCAR::perm4PLSC(
          tab1, # First Data matrix
          tab2, # Second Data matrix
          center1 = center1,
          scale1 = scale1,
          center2 = center2,
          scale2 = scale2,
          nIter = 1000,# How many iterations
          permType = 'byColumns'
      )
      # 2. Bootstrap -----
      resBoot <- data4PCCAR::Boot4PLSC(
          tab1, # First Data matrix
          tab2, # Second Data matrix
          center1 = center1,
          scale1 = scale1,
          center2 = center2,
          scale2 = scale2,
          nIter = 1000,# How many iterations
          Fi = res$TExPosition.Data$fi,
          Fj = res$TExPosition.Data$fj,
          nf2keep = 3,
          critical.value = 2,
          eig = TRUE,
          alphaLevel = .05
      )
  }else if(method == "CCA"){
      stop("Inference analysis for CCA is still under development.")
      # # 1. Permutation ----
      # resPerm <- data4PCCAR::perm4CCA(
      #     tab1, # First Data matrix
      #     tab2, # Second Data matrix
      #     nIter = 1000,# How many iterations
      #     permType = 'byColumns'
      # )
      # # 2. Bootstrap -----
      # resBoot <- data4PCCAR::Boot4CCA(
      #     tab1, # First Data matrix
      #     tab2, # Second Data matrix
      #     nIter = 1000,# How many iterations
      #     Fi = res$TExPosition.Data$fi,
      #     Fj = res$TExPosition.Data$fj,
      #     nf2keep = 3,
      #     critical.value = 2,
      #     eig = TRUE,
      #     alphaLevel = .05
      # )
  }

  if (is.null(color.obs)) color.obs <- list(oc = "darkorchid4")
  if (is.null(color.tab)) {
      color.tab <- list(
          oc = list(
              rep("darkorchid4", I),
              rep("darkorchid4", J)))
  }

  if (is.null(tab1.name)) tab1.name <- 1
  if (is.null(tab2.name)) tab2.name <- 2

  ## Scree with confidence intervals
  PlotScreeWithCI(
      ev = res$TExPosition.Data$eigs,
      ci.ev = t(resBoot$eigenCI),
      polygon.ci = 'ev',
      p.ev = resPerm$pEigenvalues
      )
  scree <- recordPlot()

  # BR K-set ----
  BR.X <- PrettyBarPlot2(
      bootratio = resBoot$bootRatios.i[, leDim[1]],
      threshold = 2,
      ylim = NULL,
      color4bar = color.tab$oc[[1]],
      color4ns = "gray75",
      plotnames = TRUE,
      main = sprintf('Bootstrap Ratios. J-set: LV%i', leDim[1]),
      ylab = "Bootstrap Ratios")
  #_____________________________________________________________________
  # BR K-set ----
  BR.Y <- PrettyBarPlot2(
      bootratio = resBoot$bootRatios.j[, leDim[2]],
      threshold = 2,
      ylim = NULL,
      color4bar = color.tab$oc[[2]],
      color4ns = "gray75",
      plotnames = TRUE,
      main = sprintf('Bootstrap Ratios. K-set: LV%i', leDim[2]),
      ylab = "Bootstrap Ratios")

  ##_________________________________________________
  ### Prepare to save as PPTX ###
  results.stats <- list(
      TExPosition.Data = res$TExPosition.Data,
      Plotting.Data = res$Plotting.Data#,
      # loadings.as.correlation = loadingsAsCorr
  )
  results.graphs <- list(
      scree = scree,
      BR.X = BR.X,
      BR.Y = BR.Y
  )
  description.graphs <- list(
      scree.eig = "Scree Plot with Confindence Intervals",
      lv.plot = "Latent Variable Map",
      BR.X = "Bootstrap Ratios for X",
      BR.Y = "Bootstrap Ratios for Y"
  )

  ## list stat & graphs ----
  results <- list(results.stats = results.stats,
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

