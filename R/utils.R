##### Internal functions #####

#' Transform angle in rad to deg
#' @param rad numeric
#' @noRd
rad2deg <- function(rad) {(rad * 180) / (pi)}

#' Transform angle in deg to rad
#' @param deg numeric
#' @noRd
deg2rad <- function(deg) {(deg * pi) / (180)}

#' Computes the arc-cosine in degrees
#' @param lecos numeric
#' @noRd
arcosdeg <- function(lecos){rad2deg(acos(lecos))}
#
#' Plot contributions
#'
#' @param signed.ctr signed contribtions
#' @param col4 colors
#' @param nfac number of dimensions
#' @param print plot the result
#' @param stem title
#' @param font.size font size
#' @param horizontal whether the barplot is horizontal
#'
#' @noRd
plotCtr <- function(signed.ctr, col4, nfac = 1,
                    print = FALSE, stem = "Jset",
                    font.size = 5,
                    horizontal = FALSE){
    leTitre <- paste0("Component ",nfac)
    ctr <- PrettyBarPlot2(signed.ctr[, nfac],
                          threshold = 1 / NROW(signed.ctr),
                          font.size = font.size,
                          signifOnly = FALSE,
                          color4bar = col4,
                          ylab = 'Signed Contributions',
                          ylim = c(1.2*min(signed.ctr[,nfac]),
                                   1.2*max(signed.ctr[,nfac])),
                          horizontal = horizontal) +
        ggtitle("Contribution Barplots",
                subtitle = leTitre) +
        theme(plot.title = element_text(
            color = "#3E2E8F", size = 20,
            face = "bold"),
            plot.subtitle = element_text(
                color = "#3E2E8F", size = 16,
                face = "italic"),
            plot.caption =  element_text(
                color = "#3E2E8F", size = 14,
                face = "italic"),
            axis.text =  element_text(
                color = "#3E2E8F", size = 12),
            axis.title.x =  element_text(
                color = "#3E2E8F", size = 16,
                face = "italic"))
    if (print){
        png(paste0(stem,nfac,'_Contributions.png'))
        print(ctr)
        dev.off()
        jpeg(paste0(stem,nfac,'_Contributions.jpeg'))
        print(ctr)
        dev.off()
    }
    return(ctr)

}


