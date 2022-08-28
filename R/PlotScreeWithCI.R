#' PlotScreeWithCI ----
#' plot the scree for the eigenvalues
#' of an SVD based multivariate analysis,
#' and add bootstrap confidence intervals.
#'
#' \code{PlotScreeWithCI}: Plot the scree for the eigenvalues
#' of an SVD-based multivariate analysis.
#' Note that the function can recompute the
#' eigen-values when a percentage is given.
#' For example  \code{ExPosition} does not return all ev
#'        but only the requested one. but return all percentage
#'        so if max.ev is specified, it is used to recompute
#'        all eigenvalues.
#'  By default \code{PlotScree}
#'  will not plot the line corresponding to
#'  the average inertia (i.e., Kaiser criterion).
#'  If provided with probabilities,
#'  \code{PlotScree} will
#'  color differently the "significant"
#'  eigenvalues.
#' @author Hervé Abdi with help
#' from Derek Beaton and Ju-Chi Yu.
#' @param ev the eigenvalues to plot.
#' No default.
#' @param p.ev the probabilities
#' associated to the
#' eigen-values, (default = \code{NULL}).
#' @param max.ev
#' the max eigenvalue
#'        needed because \code{ExPosition}
#'        does not always return all
#'        eigenvalues
#'        but sometimes only the requested ones;
#'        however \code{ExPosition} always returns
#'        all percentages i.e., \code{tau}),
#'        so if \code{max.ev} is specified,
#'        it is used to recompute
#'        all eigenvalues.
#' @param ci.ev, confidence intervals for the eigenvalues,
#'        computed with Boot4Eigs. Default to \code{NULL}.
#' @param ci.tau, confidence intervals for the taus,
#'        computed with Boot4Eigs. Default to \code{NULL}.
#' @param polygon.ci, the confidence intervals to plot.
#' \code{'tau'} will plot the bootstrap confidence intervals of taus;
#' \code{'ev'} will plot the bootstrap confidence intervals of eigenvalues;
#' \code{NULL} will not plot the polygon. Default to \code{'tau'}.
#' @param alpha
#' threshold for significance
#'   \code{Default = .05}).
#' @param col.ns color for the non significant
#' eigenvalues. Default is \code{'Green'}.
#' @param col.sig  color for significant
#' eigen-values.
#' Default is \code{'Violet'}.
#' @param title a title for the graph
#' default is
#' \code{"Explained Variance per Dimension"}.
#' @param xlab The names of the dimensions
#' (default \code{'Dimensions'}).
#' @param plotKaiser  when \code{TRUE}
#' plot a line corresponding to the average inertia
#' (Kaiser criterion); do not plot when
#' \code{FALSE} (default).
#' @param color4Kaiser
#' color for Kaiser's
#' line
#' (default is \code{'darkorchid4'})
#' @param lwd4Kaiser lwd value (i.e., width)
#' for Kaiser's criterion line.
#' (default is \code{'2.5'})
#' @import graphics
#' @examples  # PlotScree(ev)
#' @export
#'
PlotScreeWithCI <- function(ev,
                            p.ev = NULL,
                            max.ev = NULL,
                            ci.ev = NULL, # ADDED - JY ---- # results from Boot4Eigs
                            ci.tau = NULL,
                            polygon.ci = 'tau', # ADDED - JY ---- # TRUE/FALSE statement
                            alpha = .05,
                            col.ns = '#006D2C', col.sig = '#54278F',
                            title = "Explained Variance per Dimension",
                            xlab = "Dimensions",
                            plotKaiser = FALSE,
                            color4Kaiser = 'darkorchid4',
                            lwd4Kaiser = 2.5
){
  # percentage of inertia
  val.tau = (100*ev / sum(ev))
  # ADDED - JY ---------------------------
  if (polygon.ci == 'ev'){
      val.tau.ci = (100*ci.ev / sum(ev))
  }else if (polygon.ci == 'tau'){
      val.tau.ci = 100*ci.tau
  }

  # percentage of inertia for the CIs
  #---------------------------------------
  Top.y = ceiling(max(val.tau.ci) * .1) * 10
  # if ev is already a percentage convert it back
  if (!is.null(max.ev)){ev = ev * (max.ev / ev[1])}
  #
  par(mar = c(5,6,4,4))
  # plot.window(xlim = c(0, length(val.tau)+5),
  #         ylim = c(0,Top.y),asp = .6)
  plot(x = seq(1, length(val.tau)), y = val.tau, xlab = xlab,
       ylab = 'Percentage of Explained Variance',
       main = title,
       type = 'l', col = col.ns, lwd = 1,
       xlim = c(1, length(val.tau)),
       ylim = c(0,Top.y) # This was the original line
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
  if (!is.null(p.ev) & sum(p.ev < alpha) > 0){# plot the significant vp if exist
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
