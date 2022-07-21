
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
## basic example code
empty()
```

More to come soon

## List of functions

## List of data sets

## Other material

 * Principal Component Analysis [paper](../vignettes/other/abdi-awPCA2010.pdf).
 * Partial Least Methods: A review [paper](../vignettes/other/abdi-PLSC_and_PLSR2012.pdf).
