#' Core function to generate two-table analysis plots.
#'
#' @param res the result of a two-table analysis;
#' @param tab1 the result of a two-table analysis;
#' @param tab2 the result of a two-table analysis;
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
PLSRplot <- function(
        res,
        tab1,
        tab2,
        leDim = c(1, 2),
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
        title4pptx = "PLSR Results") {

  # res: two lists (lx and ly) each with two obs x var. matrices of factor scores
  # leDim: a vector the component to plot for each table. e.g., c(component of table1, component of table2)
  # color.obs: a list that contains oc (colors for individual observations) and gc (colors for individuals' groups)
  # color.tab: a list that contains oc (colors for individual variables) and gc (colors for tables)
  # title.plot: title of the plot
  # DESIGN: a vector with group variables
  # tab1.name: name for table 1
  # tab2.name: name for table 2
  # bar.font.size: a vector with font sizes for labels in barplots for respectively x and y
  rxy <- cor(tab1,tab2)
  I <- nrow(rxy)
  J <- ncol(rxy)

  if ((is.data.frame(DESIGN)|is.matrix(DESIGN)) && ncol(DESIGN) > 1){
      DESIGN <- DESIGN %*% diag(1:ncol(DESIGN)) |> rowSums()
  }else if((is.data.frame(DESIGN)|is.matrix(DESIGN)) && ncol(DESIGN) == 1){
      DESIGN <- as.vector(as.matrix(DESIGN))
  }

  ## color for the two sets of variables
  if (is.null(color.obs)) color.obs <- list(oc = "darkorchid4")
  if (is.null(color.tab)) {
      color.tab <- list(
          oc = list(
              rep("darkorchid4", I),
              rep("darkorchid4", J)))
  }

  if (is.null(tab1.name)) tab1.name <- 1
  if (is.null(tab2.name)) tab2.name <- 2

  if (is.null(score.constraints)){
    get.constraints <- minmaxHelper(res$T, axis1 = leDim[1], axis2 = leDim[2])
    min.scale <- min(get.constraints$minx, get.constraints$miny)
    max.scale <- max(get.constraints$maxx, get.constraints$maxy)
    score.constraints <- list(minx = min.scale, miny = min.scale, maxx = max.scale, maxy = max.scale)
  }

  ### 1. Heatmaps
  rxy_long <- tibble::tibble(
      yids = rownames(rxy),
      tibble::as_tibble(rxy)) |>
      tidyr::pivot_longer(
          -yids,
          values_to = "correlation",
          names_to = "xids")
  heatmap.rxy <- rxy_long  |>
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
  PlotScree(ev = res$R2x,
            title = 'Eigenvalue Scree Plot for RX2',
            plotKaiser = TRUE,
            color4Kaiser = ggplot2::alpha('darkorchid4', .5),
            lwd4Kaiser  = 2)
  scree.eig.R2X <- recordPlot()

  PlotScree(ev = res$R2x^(1/2),
            title = 'Singular Value Scree Plot for RX2',
            plotKaiser = TRUE,
            color4Kaiser = ggplot2::alpha('darkorchid4', .5),
            lwd4Kaiser  = 2)
  scree.sv.R2X <- recordPlot()

  PlotScree(ev = res$R2y,
            title = 'Eigenvalue Scree Plot for RY2',
            plotKaiser = TRUE,
            color4Kaiser = ggplot2::alpha('darkorchid4', .5),
            lwd4Kaiser  = 2)
  scree.eig.R2Y <- recordPlot()

  PlotScree(ev = res$R2y^(1/2),
            title = 'Singular Value Scree Plot for RY2',
            plotKaiser = TRUE,
            color4Kaiser = ggplot2::alpha('darkorchid4', .5),
            lwd4Kaiser  = 2)
  scree.sv.R2Y <- recordPlot()

  ## 3. Latent variables
  plot.lv <- createFactorMap(res$T,
                             axis1 = leDim[1],
                             axis2 = leDim[2],
                             col.background = NULL,
                             col.axes = "orchid4",
                             alpha.axes = 0.5,
                             title = paste0("Latent variables: ", tab2.name, " predicted by ", tab1.name),
                             col.points = color.obs$oc,
                             col.labels = color.obs$oc,
                             constraints = score.constraints,
                             alpha.points = alpha.points
  )


  lv.plot <- plot.lv$zeMap_background + plot.lv$zeMap_dots

  if (!is.null(DESIGN)) {
    lv2plot.mean <- getMeans(res$T, DESIGN)
    if (is.null(mean.constraints)) {
      mean.constraints <- lapply(minmaxHelper(lv2plot.mean), '*', scale.mean.constraints)
    }

    plot.lv.mean <- createFactorMap(lv2plot.mean,
                                    axis1 = leDim[1],
                                    axis2 = leDim[2],
                                    col.background = NULL,
                                    col.axes = "orchid4",
                                    alpha.axes = 0.5,
                                    title = paste0("Latent variables: ", tab1.name, " predicted by ", tab2.name),
                                    col.points = color.obs$gc[rownames(lv2plot.mean),],
                                    col.labels =  color.obs$gc[rownames(lv2plot.mean),],
                                    constraints = mean.constraints,
                                    cex = mean.cex,
                                    text.cex = mean.textcex,
                                    pch = 17,
                                    alpha.points = 0.8)

    bootCI.res <- Boot4Mean(res$T, DESIGN)
    colnames(bootCI.res$BootCube) <- colnames(res$T)
    col4CI <- as.matrix(color.obs$gc[rownames(lv2plot.mean),])
    names(col4CI) <- rownames(lv2plot.mean) ## THIS IS STRANGE
    lv.CI <- MakeCIEllipses(bootCI.res$BootCube[,leDim,],
                            names.of.factors = colnames(res$T)[leDim],
                            col = col4CI,
                            alpha.ellipse = 0.1,
                            line.size = 0.5, alpha.line = 0.2)

    lv.TI <- MakeToleranceIntervals(res$T[,leDim],
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


  ##_________________________________________________
  ##### Ctr and loadings J-set ----
  Fi   <- res$W
  ctri <- res$W^2
  signed.ctri <- ctri * sign(Fi)
  # LV1
  ctrW1.plot <- PrettyBarPlot2(
      bootratio = round(100*signed.ctri[,leDim[1]]),
      threshold = 100/ nrow(signed.ctri),
      ylim = NULL,
      color4bar = color.tab$oc[[1]],
      color4ns = "gray75",
      plotnames = TRUE,
      main = paste0("Important Contributions – ", tab1.name, ": LV", leDim[1]),
      ylab = "Signed Contributions")

  # LV2
  ctrW2.plot <- PrettyBarPlot2(
      bootratio = round(100*signed.ctri[,leDim[2]]),
      threshold = 100/ nrow(signed.ctri),
      ylim = NULL,
      color4bar = color.tab$oc[[1]],
      color4ns = "gray75",
      plotnames = TRUE,
      main = paste0("Important Contributions – ", tab1.name, ": LV", leDim[1]),
      ylab = "Signed Contributions")

  ## map
  W.map <- PTCA4CATA::createFactorMap(
      Fi,
      axis1 = leDim[1],
      axis2 = leDim[2],
      col.points = color.tab$oc[[1]],
      col.labels = color.tab$oc[[1]],
      alpha.points = .5
  )
  # arrows
  zeArrows4J <- addArrows(res$W,
                          axis1 = leDim[1],
                          axis2 = leDim[2],
                          color = color.tab$oc[[1]])
  # make labels
  label4Tx <- labs(x = paste0('T',leDim[1],'. R2X = ',
                              round(res$R2x[leDim[1]],2) ))
  label4Ty <- labs(y = paste0('T',leDim[2],'. R2X = ',
                              round(res$R2x[leDim[2]],2) ))

  # The map
  LoadingsMap.X <- W.map$zeMap + zeArrows4J +
      label4Tx + label4Ty

  ##### Ctr and loadings K-set ----
  Fj   <- as.matrix(res$C)
  ctrj <- as.matrix(res$C^2)
  signed.ctrj <- ctrj * sign(Fj)
  # LV1
  ctrC1.plot <- PrettyBarPlot2(
      bootratio = round(100*signed.ctrj[,leDim[1]]),
      threshold = 100/ nrow(signed.ctrj),
      ylim = NULL,
      color4bar = color.tab$oc[[2]],
      color4ns = "gray75",
      plotnames = TRUE,
      main = paste0("Important Contributions – ", tab2.name, ": LV", leDim[1]),
      ylab = "Signed Contributions")

  # LV2
  ctrC2.plot <- PrettyBarPlot2(
      bootratio = round(100*signed.ctrj[,leDim[2]]),
      threshold = 100/ nrow(signed.ctrj),
      ylim = NULL,
      color4bar = color.tab$oc[[2]],
      color4ns = "gray75",
      plotnames = TRUE,
      main = paste0("Important Contributions – ", tab2.name, ": LV", leDim[1]),
      ylab = "Signed Contributions")

  ## map
  C.map <- PTCA4CATA::createFactorMap(
      Fj,
      axis1 = leDim[1],
      axis2 = leDim[2],
      col.points = color.tab$oc[[2]],
      col.labels = color.tab$oc[[2]],
      alpha.points = .5
  )
  # arrows
  zeArrows4K <- addArrows(res$C,
                          axis1 = leDim[1],
                          axis2 = leDim[2],
                          color = color.tab$oc[[2]])
  # make labels
  label4Yx <- labs(x = paste0('T',leDim[1],'. R2Y = ',
                              round(res$R2y[leDim[1]],2) ))
  label4Yy <- labs(y = paste0('T',leDim[2],'. R2Y = ',
                              round(res$R2y[leDim[2]],2) ))

  # The map
  LoadingsMap.Y <- C.map$zeMap + zeArrows4K +
      label4Yx + label4Yy


  ##_________________________________________________
  ### Circle of corr for Y-set:
  # Create labels
  label4Map <- labs(x = sprintf("Dimension %i", leDim[1]),
                    y = sprintf("Dimension %i", leDim[2]))
  label4Map2 <- list(label4Map,
                     # The standard label from createxyLabels.gen
                     theme(axis.title = element_text(# the new theme
                         color = "darkorchid4",
                         size = rel(1.1),    # relative to default
                         # family = 'Times',   # "Times", "sans", "Courier"
                         face   = "italic" , # 'plain','italic', 'bold',
                         # NB: face does not work with current ggplot2
                     ), # end of element_text
                     plot.title = element_text(color = '#5826A3')
                     ) )

  ### correlation matrix
  cor.TY <- t(cor(tab2, res$T))
  rty_long <- tibble::tibble(
      yids = rownames(cor.TY),
      tibble::as_tibble(cor.TY)) |>
      tidyr::pivot_longer(
          -yids,
          values_to = "correlation",
          names_to = "xids")
  heatmap.rty <- rty_long  |>
      ggplot2::ggplot(ggplot2::aes(xids, yids, fill = correlation)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(limits = c(-1, 1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
          axis.ticks.x = ggplot2::element_line(color = color.tab$oc[[2]]),
          axis.text.x = ggplot2::element_text(color = color.tab$oc[[2]])
      ) +
      ggplot2::coord_equal() +
      labs(x = "", y = "")

  ### Circle of corr for K-set ----
  # Create the map
  loadingsY.cor <- cor.TY
  map4Cir.Y <- PTCA4CATA::createFactorMap(
      t(loadingsY.cor),
      col.points = color.tab$oc[[2]],
      col.labels = color.tab$oc[[2]],
      col.background = NULL,
      col.axes = "darkorchid4",
      alpha.axes = 0.5,
      constraints = list(minx = -1, miny = -1,
                         maxx = 1 , maxy = 1),
      title = paste0("Circle of Correlation for the ", tab2.name))
  #  Add some arrows
  arrows.Y <- addArrows(t(loadingsY.cor), color = color.tab$oc[[2]])

  # draw the circle
  cirCorY.plot <- map4Cir.Y$zeMap_background +
      map4Cir.Y$zeMap_text +
      addCircleOfCor(color = "darkorchid4") +
      arrows.Y + label4Yx + label4Yy

  ##_________________________________________________
  ### Prepare to save as PPTX ###
  results.stats <- list(
      TExPosition.Data = res
      # loadings.as.correlation = loadingsAsCorr
  )
  results.graphs <- list(
      heatmap.rxy = heatmap.rxy,
      scree.eig.R2X = scree.eig.R2X,
      scree.eig.R2Y = scree.eig.R2Y,
      scree.sv.R2X = scree.sv.R2X,
      scree.sv.R2Y = scree.sv.R2Y,
      lv.plot = lv.plot,
      ctrW1.plot = ctrW1.plot,
      ctrW2.plot = ctrW2.plot,
      LoadingsMap.X = LoadingsMap.X,
      ctrC1.plot = ctrC1.plot,
      ctrC2.plot = ctrC2.plot,
      LoadingsMap.Y = LoadingsMap.Y,
      heatmap.rty = heatmap.rty,
      cirCorY.plot = cirCorY.plot
  )
  description.graphs <- list(
      heatmap.rxy = "The XY Correlation Matrix (Heat Map)",
      scree.eig.R2X = "The Eigenvalues Scree Plot for X",
      scree.eig.R2Y = "The Eigenvalues Scree Plot for Y",
      scree.sv.R2X = "The Singular Values Scree Plot for X",
      scree.sv.R2Y = "The Singular Values Scree Plot for Y",
      lv.plot = "Latent Variable Map",
      ctrW1.plot = "Contributions for W1 (X weights)",
      ctrW2.plot = "Contributions for W2 (X weights)",
      LoadingsMap.X = "Loadings map for X",
      ctrC1.plot = "Contribution of C1 (Y loadings)",
      ctrC2.plot = "Contribution of C2 (Y loadings)",
      LoadingsMap.Y = "Loadings map for Y",
      heatmap.rty = "The TY Correlation Matrix (Heat Map)",
      cirCorY.plot = "Circle of Correlation for Y"
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
          file2Save.pptx = "TTA.pptx",
          title = title4pptx
      )
  }

  return(results)
  # EOF ----
}

