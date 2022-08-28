
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R4SPISE2022

<!-- badges: start -->
<!-- badges: end -->

R4SPISE2022 provide data, programs, and documentation for the SPISE-2022
*Advanced Workshop on Sensory Evaluation* 
(That took place from July 27, 2022 to July 28,
2022, in Hanoi Vietnam).

## Installation

You can install the development version of R4SPISE2022 with the
following lines of code:

``` r
# if remotes is not installed, decomment the following line
# install.packages('remotes')
remotes::install_github('HerveAbdi/R4SPISE2022')
```

To install the vignettes while installing the package, 
then run the
following commands:

``` r
remotes::install_github('HerveAbdi/R4SPISE2022', 
  build_vignettes = TRUE,
  force = TRUE)
```


## List of functions

### Boot4Eigs    
Boostrap for eigenvalues.
### graph4epPCA    
create the graphs for a PCA analysis from `ExPosition` 
and generate the standard graphs and tables. Note: _Still Under Development_.
### OTAplot    
create the graphs for a PCA analysis from 'ExPosition' and generate the standard graphs and tables. Note: _Still Under Development_.
### OTAplotInference    
create the graphs for a PCA analysis from 'ExPosition' and generate the standard graphs and tables. Note: _Still Under Development_.
### PlotScreeWithCI    
plot the scree for the eigenvalues of an SVD based multivariate analysis, and add bootstrap confidence intervals.
### PLSRplot    
core function to generate PLSR plots.
### R4SPISE2022    
A set of Programs, Vignettes, and Data for the SPISE2022 Advanced Workshop
### saveAllGraphsInList2pptx    
saves all the graphics in a list into a PowerPoint file.
### TTAplot    
Core function to generate two-table analysis plots.
### TTAplotInference    
Core function to generate two-table analysis plots.

## List of data sets

To be 

## Some Related Papers 

 * Principal Component Analysis [paper](inst/extdata/abdi-awPCA2010.pdf).
 * Partial Least Methods: A Review [paper](inst/extdata/abdi-PLSC_and_PLSR2012.pdf).
 * Canonical Correation Analysis [paper](inst/extdata/abdi2017-CanonicalCorrelationAnalysis.pdf).

=======
>>>>>>> 174eece5b0e310c163cd116420a9939c477f85ef
