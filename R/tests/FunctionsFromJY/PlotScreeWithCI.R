# I have changed the name
PlotScreeWithCI <- function(ev,
                            p.ev = NULL,
                            max.ev = NULL,
                            ci.ev = NULL, # ADDED - JY ---- # results from Boot4Eigs
                            polygon.ci = FALSE, # ADDED - JY ---- # TRUE/FALSE statement
                            alpha = .05,
                            col.ns = '#006D2C', col.sig = '#54278F',
                            title = "Explained Variance per Dimension",
                            plotKaiser = FALSE,
                            color4Kaiser = 'darkorchid4',
                            lwd4Kaiser = 2.5
){
  # percentage of inertia
  val.tau = (100*ev / sum(ev))
  # ADDED - JY ---------------------------
  # percentage of inertia for the CIs
  val.tau.ci = (100*ci.ev$BootMatEigsCI/sum(ev))
  #---------------------------------------
  Top.y = ceiling(max(val.tau) * .1) * 10
  # if ev is already a percentage convert it back
  if (!is.null(max.ev)){ev = ev * (max.ev / ev[1])}
  #
  par(mar = c(5,6,4,4))
  # plot.window(xlim = c(0, length(val.tau)+5),
  #         ylim = c(0,Top.y),asp = .6)
  plot(x = seq(1, length(val.tau)), y = val.tau, xlab = 'Dimensions',
       ylab = 'Percentage of Explained Variance',
       main = title,
       type = 'l', col = col.ns, lwd = 1,
       xlim = c(1, length(val.tau)),
       ylim = c(0,Top.y) # This was the orinigal line
  )
  # ADDED - JY ------------------------------------------------------
  if(!is.null(ci.ev)){ # plot the confidence intervals if ci.ev exist
    arrows(seq(1, length(val.tau)), val.tau.ci[,1], seq(1, length(val.tau)), val.tau.ci[,2], length=0.05, angle=90, code=3)
  }
  if(!is.null(polygon.ci)){
    polygon(c(seq(1, length(val.tau)),seq(length(val.tau),1)),
            c(val.tau.ci[,1],rev(val.tau.ci[,2])),
            col = adjustcolor(col.ns,alpha.f = 0.2),
            border = FALSE
            )
  }
  #------------------------------------------------------------------
  points(x = seq(1,length(val.tau)),y = val.tau,
         pch = 16,  cex = 1, col = col.ns, lwd = 2.5
  )
  if (!is.null(p.ev)){# plot the significant vp if exist
    # Plot the significant factors
    signi.vp = which(p.ev < alpha)
    # These are the lines Ju-Chi changed ####
    lines(x = seq(1, max(signi.vp)), y = val.tau[1:max(signi.vp)],
          type = "l", col = col.sig, lwd = 1.5)
    points(x = signi.vp, y = val.tau[signi.vp],
           pch = 16, cex = 1.5, col = col.sig, lwd = 3.5)
    #______________________________________________
  } # end of plot significant vp
  par(new = TRUE)
  par(mar = c(5,6,4,4) + .5)
  le.max.vp = Top.y*(ev[1]/val.tau[1])
  plot(ev, ann = FALSE,axes = FALSE,type = "n",#line=3,
       ylim = c(0,le.max.vp))
  if (plotKaiser){
    abline(h = sum(ev)/length(ev),  col = color4Kaiser, lwd = lwd4Kaiser)
  }
  mtext("Inertia Extracted by the Components", side = 4, line = 3)
  axis(4)
} # end of function
