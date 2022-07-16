#' Core function to generate two-table analysis plots.
#'
#' @param res the result of a two-table analysis;
#' @param leDim vector containning the dimensions to be plotted;
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
#'
#' @return a list of plots
#' @export
#'
#' @examples
#' \dontrun{
#'   # An example will be added very soon
#' }
#'
coreTTAplot <- function(
        res,
        leDim = c(1, 1),
        color.obs = NULL,
        color.tab = NULL,
        title.plot = "Results",
        alpha.points = 0.05,
        DESIGN = NULL,
        sigbar.p = FALSE,
        sigbar.q = FALSE,
        tab1.name = NULL,
        tab2.name = NULL,
        TI = FALSE,
        mean.cex = 3,
        mean.textcex = 3,
        only.mean = TRUE,
        only.ind = FALSE,
        score.constraints = NULL,
        mean.constraints = NULL,
        scale.mean.constraints = 3) {

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

  lv2plot <- cbind(
      res$TExPosition.Data$lx[, leDim[1]],
      res$TExPosition.Data$ly[, leDim[2]])

  if (is.null(color.obs)) color.obs <- list(oc = "darkorchid4")
  if (is.null(color.tab)) {
      color.tab <- list(
          oc = list(
              rep("darkorchid4", I),
              rep("darkorchid4", J)))
  }

  if (is.null(tab1.name)) tab1.name <- 1
  if (is.null(tab2.name)) tab2.name <- 2

  colnames(lv2plot) <- paste0(c("Lx ","Ly "), leDim,": ", c(tab1.name,tab2.name))

  if (is.null(score.constraints)){
    get.constraints <- minmaxHelper(lv2plot)
    min.scale <- min(get.constraints$minx, get.constraints$miny)
    max.scale <- max(get.constraints$maxx, get.constraints$maxy)
    score.constraints <- list(minx = min.scale, miny = min.scale, maxx = max.scale, maxy = max.scale)
  }

  ### 1. Heatmaps
  rxy_long <- tibble::tibble(
      yids = rownames(rxy),
      tibble::as_tibble(rxy))%>%
      tidyr::pivot_longer(
          -yids,
          values_to = "correlation",
          names_to = "xids")
  heatmap.rxy <- rxy_long  %>%
      ggplot2::ggplot(ggplot2::aes(xids, yids, fill = correlation)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(limits = c(-1, 1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
          axis.ticks.y = ggplot2::element_line(color = color.tab$oc[[1]]),
          axis.ticks.x = ggplot2::element_line(color = color.tab$oc[[2]]),
          axis.text.y = ggplot2::element_text(color = color.tab$oc[[1]]),
          axis.text.x = ggplot2::element_text(color = color.tab$oc[[2]])
      ) +
    ggplot2::coord_equal() +
      labs(x = "", y = "")

  ### 2. Scree plots
  PlotScree(ev = res$TExPosition.Data$eigs,
            title = 'Eigenvalue Scree Plot',
            plotKaiser = TRUE,
            color4Kaiser = ggplot2::alpha('darkorchid4', .5),
            lwd4Kaiser  = 2)
  scree.eig <- recordPlot()

  PlotScree(ev = res$TExPosition.Data$eigs^(1/2),
            title = 'Singular Value Scree Plot',
            plotKaiser = FALSE,
            color4Kaiser = ggplot2::alpha('darkorchid4', .5),
            lwd4Kaiser  = 2)
  scree.sv <- recordPlot()

  ## 3. Latent variables
  plot.lv <- createFactorMap(lv2plot,
                             col.background = NULL,
                             col.axes = "orchid4",
                             alpha.axes = 0.5,
                             col.points = color.obs$oc,
                             col.labels = color.obs$oc,
                             constraints = score.constraints,
                             alpha.points = alpha.points
  )


  lv.plot <- plot.lv$zeMap_background + plot.lv$zeMap_dots

  if (!is.null(DESIGN)) {
    lv2plot.mean <- getMeans(lv2plot, DESIGN)
    if (is.null(mean.constraints)) {
      mean.constraints <- lapply(minmaxHelper(lv2plot.mean), '*', scale.mean.constraints)
    }

    plot.lv.mean <- createFactorMap(lv2plot.mean,
                                    col.background = NULL,
                                    col.axes = "orchid4",
                                    alpha.axes = 0.5,
                                    col.point = color.obs$gc[rownames(lv2plot.mean),],
                                    col.labels =  color.obs$gc[rownames(lv2plot.mean),],
                                    constraints = mean.constraints,
                                    cex = mean.cex,
                                    text.cex = mean.textcex,
                                    pch = 17,
                                    alpha.points = 0.8)

    bootCI.res <- Boot4Mean(lv2plot, DESIGN)
    colnames(bootCI.res$BootCube) <- colnames(lv2plot)
    col4CI <- as.matrix(color.obs$gc[rownames(lv2plot.mean),])
    names(col4CI) <- rownames(lv2plot.mean) ## THIS IS STRANGE
    lv.CI <- MakeCIEllipses(bootCI.res$BootCube,
                            names.of.factors = colnames(lv2plot),
                            col = col4CI,
                            alpha.ellipse = 0.1,
                            line.size = 0.5, alpha.line = 0.2)

    lv.TI <- MakeToleranceIntervals(lv2plot,
                                    design = DESIGN,
                                    axis1 = 1, axis2 = 2,
                                    col = col4CI,
                                    line.size = 1,
                                    alpha.ellipse = 0.05, alpha.line = 0.3,
                                    p.level = .80)
    if (!only.ind) {
      if (TI) {
          # lv.plot <- lv.plot$background + lv.TI + lv.CI + plot.lv.mean$zeMap_dots + plot.lv.mean$zeMap_text + theme(plot.margin = unit(c(1,1,1,1), "lines"))
          lv.plot <- plot.lv$zeMap_background + lv.TI + lv.CI + plot.lv.mean$zeMap_dots + plot.lv.mean$zeMap_text + theme(plot.margin = unit(c(1,1,1,1), "lines"))
      } else {
          lv.plot <- plot.lv$zeMap_background + plot.lv$zeMap_dots + lv.CI + plot.lv.mean$zeMap_dots + plot.lv.mean$zeMap_text + theme(plot.margin = unit(c(1,1,1,1), "lines"))
      }
      if (only.mean) {
          lv.plot <- plot.lv.mean$zeMap_background + lv.CI + plot.lv.mean$zeMap_dots + plot.lv.mean$zeMap_text + theme(plot.margin = unit(c(1,1,1,1), "lines"))
      }
    }

  }

  ##### Ctr I-set ----
  Fi   <- res$TExPosition.Data$fi
  ctri <- res$TExPosition.Data$ci
  signed.ctri <- ctri * sign(Fi)
  # LV1
  ctrX.plot <- PrettyBarPlot2(
      bootratio = round(100*signed.ctri[,1]),
      threshold = 100/ nrow(signed.ctri),
      ylim = NULL,
      color4bar = color.tab$oc[[1]],
      color4ns = "gray75",
      plotnames = TRUE,
      main = 'Important Contributions I-set: LV1',
      ylab = "Signed Contributions")

  ##_________________________________________________
  ##### Ctr J-set ----
  Fj   <- res$TExPosition.Data$fj
  ctrj <- res$TExPosition.Data$cj
  signed.ctrj <- ctrj * sign(Fj)
  # LV1
  ctrY.plot <- PrettyBarPlot2(
      bootratio = round(100*signed.ctrj[,1]),
      threshold = 100 / nrow(signed.ctrj),
      ylim = NULL,
      color4bar = color.tab$oc[[2]],
      color4ns = "gray75",
      plotnames = TRUE,
      main = 'Important Contributions J-set: LV1',
      ylab = "Signed Contributions")

  ### Prepare to save as PPTX ###
  results.stats <- list(
      TExPosition.Data = res$TExPosition.Data,
      Plotting.Data = res$Plotting.Data,
      loadings.as.correlation = loadingsAsCorr
  )
  results.graphs <- list(
      heatmap.rxy = heatmap.rxy,
      scree.eig = scree.eig,
      scree.sv = scree.sv,
      lv.plot = lv.plot,
      ctrX.plot = ctrX.plot,
      ctrY.plot = ctrY.plot
  )
  description.graphs <- list(
      heatmap.rxy = "The XY Correlation Matrix (Heat Map)",
      scree.eig = "The Eigenvalues Scree Plot",
      scree.sv = "The Singular Values Scree Plot",
      lv.plot = "Latent Variable Map",
      ctrX.plot = "Contributions for X",
      ctrY.plot = "Contributions for Y",
  )
  ## list stat & graphs ----
  results <- list(results.stats = results.stats,
                  results.graphs = results.graphs,
                  description.graphs = description.graphs
  )
  return(results)
  # EOF ----
}

