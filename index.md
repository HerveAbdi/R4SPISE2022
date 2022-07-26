
# R4SPISE2022

<!-- badges: start -->
<!-- badges: end -->
R4SPISE2022 provide data, programs, 
and documentation for the SPISE-2022 
*Advanced Workshop on Sensory Evaluation*
(July 27, 2022 to July 28, 2022;
to take place in Hanoi Vietnam).

## Installation

You can install the development version of
R4SPISE2022 with the following lines of code:

``` r
# if remotes is not installed, decomment the following line
# install.packages('remotes')
remotes::install_github('HerveAbdi/R4SPISE2022')
```

To install the vignettes while installing the package, then run the following commands:

``` r
remotes::install_github('HerveAbdi/R4SPISE2022', 
  build_vignettes = TRUE,
  force = TRUE)
```
## Example

This is a basic example which shows 
how to solve a common problem:

``` r
library(R4SPISE2022)

## load data

data("sixBeers12Descriptors10Judges", package = "data4PCCAR")
beers <- sixBeers12Descriptors10Judges$ratingsIntensity

## run PCA
res.pca <- epPCA(beers, 
                 center = TRUE, # Center the data
                 scale = FALSE, # **Do not** scale the variables
                 graphs = FALSE) 

## generate plots for descriptive results       
res.plot.pca <- OTAplot(resPCA = res.pca,
                        data = beers)

## generate plots for inference results
res.plot.pca.inference <- OTAplotInference(resPCA = res.pca,
                                           data = beers)
```

## List of main functions

* `OTAplot()`: Core function to generate figures for descriptive results from one table analysis (i.e., principal component analysis; PCA)

* `OTAplotInference()`: Core function to generate figures for inference results from one table analysis (i.e., PCA)

* `TTAplot()`: Core function to generate figures for descriptive results from two table analyses (i.e., PLSC, CCA, and RA)

* `TTAplotInferenct()`: Core function to generate figures for inference results from two table analyses (only PLSC for now)

* `PLSRplot()`: Core function to generate figures for descriptive and inference results from partial least square regression (PLSR)


## List of data sets

* [`sixBeers12Descriptors10Judges`](articles/A1_DataBeers.html)

* [`sixteenGums4Descriptors`](articles/A2_DataChewingGums.html)

* [`winesOf3Colors`](articles/A3_DataWines.html)


## Other material

 * Principal Component Analysis [paper](articles/other/abdi-awPCA2010.pdf).
 * Partial Least Square Methods: A review [paper](articles/other/abdi-PLSC_and_PLSR2012.pdf).
 * Canonical Correlation Analysis
[paper](articles/other/abdi-CCA2018.pdf)
