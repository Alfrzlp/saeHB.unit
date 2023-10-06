
# saeHB.unit

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/saeHB.unit)](https://CRAN.R-project.org/package=saeHB.unit)
[![check-standard](https://github.com/Alfrzlp/saeHB.unit/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Alfrzlp/saeHB.unit/actions/workflows/check-standard.yaml)

<!-- badges: end -->

# Author

Azka Ubaidillah, Ridson Al Farizal P

# Maintainer

Ridson Al Farizal P \<alfrzlp@gmail.com\>

# Description

We designed this package to provide several functions for unit level of
small area estimation (Battese, Harter and Fuller model) using
hierarchical Bayesian (HB) method. This package also provides a dataset
produced by a data generation. The `rjags` package is employed to obtain
parameter estimates

# Installation

You can install the development version of saeHB.unit from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Alfrzlp/saeHB.unit")
```

Or you can install cran version with

``` r
install.packages(saeHB.unit)
```

## Example 1

This is a basic example which shows you how to solve a common problem:

``` r
library(dplyr)
library(saeHB.unit)
library(ggplot2)
library(tidyr)

windowsFonts(
  poppins = windowsFont('poppins'),
  tnr = windowsFont('Times New Roman')
)
```

### Data

``` r
glimpse(cornsoybean)
#> Rows: 37
#> Columns: 5
#> $ County      <int> 1, 2, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9…
#> $ CornHec     <dbl> 165.76, 96.32, 76.08, 185.35, 116.43, 162.08, 152.04, 161.…
#> $ SoyBeansHec <dbl> 8.09, 106.03, 103.60, 6.47, 63.82, 43.50, 71.43, 42.49, 10…
#> $ CornPix     <int> 374, 209, 253, 432, 367, 361, 288, 369, 206, 316, 145, 355…
#> $ SoyBeansPix <int> 55, 218, 250, 96, 178, 137, 206, 165, 218, 221, 338, 128, …
glimpse(cornsoybeanmeans)
#> Rows: 12
#> Columns: 6
#> $ CountyIndex           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
#> $ CountyName            <fct> CerroGordo, Hamilton, Worth, Humboldt, Franklin,…
#> $ SampSegments          <int> 1, 1, 1, 2, 3, 3, 3, 3, 4, 5, 5, 6
#> $ PopnSegments          <dbl> 545, 566, 394, 424, 564, 570, 402, 567, 687, 569…
#> $ MeanCornPixPerSeg     <dbl> 295.29, 300.40, 289.60, 290.74, 318.21, 257.17, …
#> $ MeanSoyBeansPixPerSeg <dbl> 189.70, 196.65, 205.28, 220.22, 188.06, 247.13, …
```

``` r
Xarea <- cornsoybeanmeans %>%
   dplyr::select(
      County = CountyIndex,
      CornPix = MeanCornPixPerSeg,
      SoyBeansPix = MeanSoyBeansPixPerSeg
   )
head(Xarea)
#>   County CornPix SoyBeansPix
#> 1      1  295.29      189.70
#> 2      2  300.40      196.65
#> 3      3  289.60      205.28
#> 4      4  290.74      220.22
#> 5      5  318.21      188.06
#> 6      6  257.17      247.13
```

### Model

``` r
corn_model <- hb_BHF(
   CornHec ~ SoyBeansPix + CornPix,
   data_unit = cornsoybean,
   data_area = Xarea,
   domain = "County",
   iter.update = 20
)
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->![](man/figures/README-unnamed-chunk-7-2.png)<!-- -->

    #>                   Mean         SD       2.5%        25%        50%        75%
    #> intercept    0.1552265  0.7415844 -1.3078113 -0.3262636  0.1458748  0.6601395
    #> SoyBeansPix  0.0045318  0.0044236 -0.0041645  0.0016640  0.0045969  0.0074832
    #> CornPix      0.4002724  0.0025485  0.3951512  0.3985503  0.4002998  0.4019972
    #>              97.5%
    #> intercept   1.6317
    #> SoyBeansPix 0.0132
    #> CornPix     0.4053

## Example 2

### Data

``` r
head(dummy_unit)
#>      domain     y_di       x1       x2
#> 181      d1 72.67585 13.88978 14.35716
#> 279      d1 63.10395 10.38460 13.58800
#> 343      d1 80.10076 16.36110 14.59864
#> 539      d1 68.83004 13.33046 14.17516
#> 670      d1 77.48515 18.62796 12.55454
#> 1424     d1 78.00826 13.44454 16.34891
```

``` r
head(dummy_area)
#>   domain       x1       x2 parameter
#> 1     d1 15.05076 14.96766  77.03467
#> 2     d2 15.07153 14.98934  74.67858
#> 3     d3 14.96426 14.94145  73.35885
#> 4     d4 15.03803 15.02529  77.99655
#> 5     d5 14.98165 14.99815  76.76959
#> 6     d6 15.04244 15.00129  77.30116
```

### Model

``` r
hb_model <- hb_BHF(
  formula = y_di ~ x1 + x2,
  data_unit = dummy_unit,
  data_area = dummy_area,
  domain = "domain",
  iter.update = 30,
  plot = FALSE
)
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 2/30 | ■■■ 7% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 3/30 | ■■■■ 10% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 4/30 | ■■■■■ 13% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 5/30 | ■■■■■■ 17% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 6/30 | ■■■■■■■ 20% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 7/30 | ■■■■■■■■ 23% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 8/30 | ■■■■■■■■■ 27% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 9/30 | ■■■■■■■■■■ 30% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 10/30 | ■■■■■■■■■■■ 33% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 11/30 | ■■■■■■■■■■■■ 37% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 12/30 | ■■■■■■■■■■■■■ 40% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 13/30 | ■■■■■■■■■■■■■■ 43% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 14/30 | ■■■■■■■■■■■■■■■ 47% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 15/30 | ■■■■■■■■■■■■■■■■ 50% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 16/30 | ■■■■■■■■■■■■■■■■■ 53% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 17/30 | ■■■■■■■■■■■■■■■■■■ 57% | ETA: 1m
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 18/30 | ■■■■■■■■■■■■■■■■■■■ 60% | ETA: 50s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 19/30 | ■■■■■■■■■■■■■■■■■■■■ 63% | ETA: 46s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 20/30 | ■■■■■■■■■■■■■■■■■■■■■ 67% | ETA: 42s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 21/30 | ■■■■■■■■■■■■■■■■■■■■■■ 70% | ETA: 38s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 22/30 | ■■■■■■■■■■■■■■■■■■■■■■■ 73% | ETA: 34s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 23/30 | ■■■■■■■■■■■■■■■■■■■■■■■■ 77% | ETA: 30s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 24/30 | ■■■■■■■■■■■■■■■■■■■■■■■■■ 80% | ETA: 25s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 25/30 | ■■■■■■■■■■■■■■■■■■■■■■■■■■ 83% | ETA:
#> 21s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 26/30 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■ 87% | ETA:
#> 17s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 27/30 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 90% |
#> ETA: 13s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 28/30 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 93% |
#> ETA: 8s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> Update 29/30 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 97%
#> | ETA: 4s
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> 
#> Warning in FUN(X[[i]], ...): Failed to set trace monitor for muT
#> Variable muT not found
#> 
#> ── Coefficient ─────────────────────────────────────────────────────────────────
#>                Mean        SD      2.5%       25%       50%       75%  97.5%
#> intercept 0.3888520 0.0603124 0.2736811 0.3463387 0.3884280 0.4297975 0.5090
#> x1        2.0298767 0.0023656 2.0253100 2.0283008 2.0298457 2.0314584 2.0346
#> x2        3.0232782 0.0021441 3.0189985 3.0218045 3.0232850 3.0248043 3.0273
```

## Autocorelation, Trace and Density plot

``` r
saeHB.unit::autoplot(hb_model)
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->![](man/figures/README-unnamed-chunk-11-2.png)<!-- -->
\## Coefficients

``` r
summary(hb_model)
#>                Mean        SD      2.5%       25%       50%       75%  97.5%
#> intercept 0.3888520 0.0603124 0.2736811 0.3463387 0.3884280 0.4297975 0.5090
#> x1        2.0298767 0.0023656 2.0253100 2.0283008 2.0298457 2.0314584 2.0346
#> x2        3.0232782 0.0021441 3.0189985 3.0218045 3.0232850 3.0248043 3.0273
```

``` r
data.frame(
  id = 1:30,
  hb = hb_model$Est$MEAN,
  parameter = dummy_area$parameter
) %>%
  pivot_longer(-1, names_to = "metode", values_to = "rse") %>%
  ggplot(aes(x = id, y = rse, col = metode)) +
  geom_line() +
  scale_color_discrete(
    labels = c('HB with NA', 'Parameter')
  ) +
  labs(col = NULL, y = 'Estimate', x = 'County', title = 'Comparison of estimates') +
  theme(
    text = element_text(family = 'poppins'),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = 2, vjust = 0),
    plot.subtitle = element_text(colour = 'gray30', vjust = 0)
  )
```

![](man/figures/README-unnamed-chunk-13-1.png)<!-- -->

# References

- Battese, G. E., Harter, R. M., & Fuller, W. A. (1988). An
  error-components model for prediction of county crop areas using
  survey and satellite data. Journal of the American Statistical
  Association, 83(401), 28-36.

- Rao, J. N., & Molina, I. (2015). Small area estimation. John Wiley &
  Sons.
