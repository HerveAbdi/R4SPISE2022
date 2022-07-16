pls.pair.plot <- function(res, 
                          leDim = c(1,1), 
                          color.obs, 
                          color.tab, 
                          title.plot = "Results",
                          alpha.points = 0.05,
                          DESIGN = NULL, 
                          DESIGN.p = NULL,
                          DESIGN.q = NULL,
                          sigbar.p = FALSE,
                          sigbar.q = FALSE,
                          line.size.p = 1.5,
                          line.size.q = 1.5,
                          tab1.name = NULL, 
                          tab2.name = NULL, 
                          bar.font.size = c(3,3),
                          legend.tab1 = FALSE, 
                          legend.tab2 = FALSE,
                          TI = FALSE,
                          mean.cex = 3,
                          mean.textcex = 3,
                          only.mean = TRUE,
                          only.ind = FALSE,
                          score.constraints = NULL,
                          mean.constraints = NULL,
                          scale.mean.constraints = 3){
  require(gridExtra)
  require(grid)
  # res: two lists (lx and ly) each with two obs x var. matrices of factor scores
  # leDim: a vector the component to plot for each table. e.g., c(component of table1, component of table2)
  # color.obs: a list that contains oc (colors for individual observations) and gc (colors for individuals' groups)
  # color.tab: a list that contains oc (colors for individual variables) and gc (colors for tables)
  # title.plot: title of the plot
  # DESIGN: a vector with group variables
  # tab1.name: name for table 1
  # tab2.name: name for table 2
  # bar.font.size: a vector with font sizes for labels in barplots for respectively x and y
  
  lv2plot <- cbind(res$lx[,leDim[1]],res$ly[,leDim[2]])
  
  if (is.null(tab1.name)){tab1.name = 1}
  if (is.null(tab2.name)){tab2.name = 2}
  colnames(lv2plot) <- paste0(c("Lx ","Ly "), leDim,": ", c(tab1.name,tab2.name))
  
  if (is.null(score.constraints)){
    get.constraints <- minmaxHelper(lv2plot)
    min.scale <- min(get.constraints$minx, get.constraints$miny)
    max.scale <- max(get.constraints$maxx, get.constraints$maxy)
    score.constraints <- list(minx = min.scale, miny = min.scale, maxx = max.scale, maxy = max.scale)
  }
  
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
  
  if (is.null(DESIGN) == FALSE){
    lv2plot.mean <- getMeans(lv2plot, DESIGN)
    if (is.null(mean.constraints)){
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
    names(col4CI) <- rownames(lv2plot.mean)
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
    if(only.ind == TRUE){
      lv.plot <- lv.plot
    }else{
      
      if (TI == TRUE){
        lv.plot <- lv.plot$background + lv.TI + lv.CI + plot.lv.mean$zeMap_dots + plot.lv.mean$zeMap_text + theme(plot.margin = unit(c(1,1,1,1), "lines"))  
      }else{
        lv.plot <- plot.lv$zeMap_background + plot.lv$zeMap_dots + lv.CI + plot.lv.mean$zeMap_dots + plot.lv.mean$zeMap_text + theme(plot.margin = unit(c(1,1,1,1), "lines"))
      }
      
      if (only.mean == TRUE){
        lv.plot <- plot.lv.mean$zeMap_background + lv.CI + plot.lv.mean$zeMap_dots + plot.lv.mean$zeMap_text + theme(plot.margin = unit(c(1,1,1,1), "lines"))
      }
    }
    
  }
  
  
  p.tab1 <- PrettyBarPlot2(res$p[,leDim[1]],
                           threshold = 0,
                           signifOnly = sigbar.p, 
                           ylim = c(min(min(res$p[,leDim[1]]), plot.lv$constraints$minx),
                                    max(max(res$p[,leDim[1]]), plot.lv$constraints$maxx)),
                           font.size = bar.font.size[1],
                           color4bar = gplots::col2hex(color.tab$oc[[1]]), # we need hex code
                           ylab = paste0('Loadings: ', tab1.name),
                           horizontal = FALSE)
  if (!is.null(DESIGN.p)){
    p.tab1 <- p.tab1 + geom_line(aes(x = IDnum, y = bootratio, group = DESIGN.p), size = line.size.p)
  }
  
  q.tab2 <- PrettyBarPlot2(res$q[,leDim[2]],
                           threshold = 0,
                           signifOnly = sigbar.q, 
                           ylim = c(min(min(res$q[,leDim[2]]), plot.lv$constraints$miny),
                                    max(max(res$q[,leDim[2]]), plot.lv$constraints$maxy)),
                           font.size = bar.font.size[2],
                           color4bar = gplots::col2hex(color.tab$oc[[2]]), # we need hex code
                           ylab = paste0('Loadings: ', tab2.name),
                           horizontal = TRUE
  )
  
  if (!is.null(DESIGN.q)){
    q.tab2 <- q.tab2 + geom_line(aes(x = IDnum, y = bootratio, group = DESIGN.q), size = line.size.q)
  }
  
  p3.1 <- ggplot() + theme_void()+ xlim(1,6) 
  if (legend.tab1 == TRUE){
    annot <- data.frame(list(x = c(rep(c(5,4),each = 5),4),
                             y = c(1:6, 1:5),
                             label = as.vector(rownames(color.tab$gc[[1]])))
    )
    p3.1 <- p3.1 + geom_text(data=annot, aes(x=x, y=y, label=label), 
                             color=color.tab$gc[[1]], 
                             size=2 , fontface="bold")
    
  }
  master <- grid.arrange(
    q.tab2,lv.plot,p3.1,p.tab1,
    ncol = 2,nrow = 2,
    widths = 1:2, heights = 2:1,
    top = textGrob(title.plot, gp = gpar(fontsize = 18, font = 3))
  )  
  
  return(list(master = master, lv.plot = lv.plot, p.bar = p.tab1, q.bar = q.tab2, annot = p3.1))
  
  
}

