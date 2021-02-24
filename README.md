
# Track Covid-19 severity measures

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Installation

Install the unstable development version of the package with:

``` r
remotes::install_github("epiforecasts/covid19.track.severity")
```

## Proof of concept

  - Packages

<!-- end list -->

``` r
# Packages ----------------------------------------------------------------
library(covid19.track.severity)
library(idbrms)
library(brms)
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.14.4). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> The following object is masked from 'package:stats':
#> 
#>     ar
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(purrr)
```

  - Load data

<!-- end list -->

``` r
obs <- get_uk_obs_grid(date_from = Sys.Date() - 4 * 7 * 12, 
                       date_to = Sys.Date()) %>% 
  filter(level %in% "DA") 
# set number of parallel cores
options(mc.cores = 4)
```

  - Prepare for modelling

<!-- end list -->

``` r
obs <- obs %>% 
  mutate(data = map(data, prepare, model = "convolution", max_convolution = 15))
```

  - Model

<!-- end list -->

``` r
fit <- idbrm(data = obs$data[[1]],
             family = negbinomial(link = "identity"),
             formula = id_formula(obs$data[[1]],
                                  scale = ~ time*location,
                                  cmean =  ~ (1 | location),
                                  lcsd =  ~ (1 | location)),
             priors = id_priors(obs$data[[1]],
                                scale = c(log(0.05), 1),
                                cmean = c(log(5), 0.25),
                                lcsd = c(log(0.5), 0.25)))
#> Warning: The global prior 'normal(0, 1)' of class 'b' will not be used in the
#> model as all related coefficients have individual priors already. If you did not
#> set those priors yourself, then maybe brms has assigned default priors. See ?
#> set_prior and ?get_prior for more details.
#> Warning: The global prior 'normal(0, 0.5)' of class 'b' will not be used in the
#> model as all related coefficients have individual priors already. If you did not
#> set those priors yourself, then maybe brms has assigned default priors. See ?
#> set_prior and ?get_prior for more details.
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 795 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 990 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#> http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.92, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#tail-ess
```

  - Summarise model

<!-- end list -->

``` r
summary(fit)
#> Warning: Parts of the model have not converged (some Rhats are > 1.05). Be
#> careful when analysing the results! We recommend running more iterations and/or
#> setting stronger priors.
#> Warning: There were 795 divergent transitions after warmup. Increasing
#> adapt_delta above 0.8 may help. See http://mc-stan.org/misc/
#> warnings.html#divergent-transitions-after-warmup
#>  Family: negbinomial 
#>   Links: mu = identity; shape = identity 
#> Formula: secondary ~ idbrms_convolve(primary, scale, cmean, lcsd, cmax, index, cstart, init_obs) 
#>          scale ~ time * location
#>          cmean ~ (1 | location)
#>          lcsd ~ (1 | location)
#>    Data: data (Number of observations: 452) 
#> Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup samples = 4000
#> 
#> Group-Level Effects: 
#> ~location (Number of levels: 4) 
#>                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(cmean_Intercept)    36.58     28.99     6.39   117.01 1.07      160      186
#> sd(lcsd_Intercept)      6.11      3.52     1.68    13.07 1.22       14       64
#> 
#> Population-Level Effects: 
#>                                    Estimate Est.Error l-95% CI u-95% CI Rhat
#> scale_Intercept                       -3.86      0.06    -3.99    -3.75 1.20
#> scale_time                             0.00      0.00     0.00     0.01 1.18
#> scale_locationNorthernIreland         -0.35      0.14    -0.61    -0.06 1.53
#> scale_locationScotland                 0.47      0.10     0.29     0.68 1.12
#> scale_locationWales                    0.11      0.10    -0.09     0.31 1.21
#> scale_time:locationNorthernIreland     0.00      0.00    -0.00     0.00 1.34
#> scale_time:locationScotland           -0.00      0.00    -0.01    -0.00 1.11
#> scale_time:locationWales              -0.00      0.00    -0.01    -0.00 1.19
#> cmean_Intercept                        1.63      0.23     1.18     2.04 1.03
#> lcsd_Intercept                        -0.64      0.25    -1.16    -0.19 1.10
#>                                    Bulk_ESS Tail_ESS
#> scale_Intercept                          14      188
#> scale_time                               15      204
#> scale_locationNorthernIreland             7       30
#> scale_locationScotland                   24      233
#> scale_locationWales                      14      127
#> scale_time:locationNorthernIreland        9       36
#> scale_time:locationScotland              25      330
#> scale_time:locationWales                 15      129
#> cmean_Intercept                         138      907
#> lcsd_Intercept                           26      195
#> 
#> Family Specific Parameters: 
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> shape    12.23      1.00    10.38    14.56 1.07       79      746
#> 
#> Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

  - Random effects

<!-- end list -->

``` r
ranef(fit)
#> $location
#> , , cmean_Intercept
#> 
#>                   Estimate Est.Error        Q2.5     Q97.5
#> England           30.93680  28.72227    3.066667 106.09142
#> Northern Ireland -16.23846  39.73027 -120.081999  47.46465
#> Scotland          34.89743  40.37004    2.189062 142.41272
#> Wales             28.71092  26.47147    1.742296  96.00177
#> 
#> , , lcsd_Intercept
#> 
#>                  Estimate  Est.Error       Q2.5     Q97.5
#> England          2.665139  0.4738968  1.6803864  3.534606
#> Northern Ireland 4.387238 11.1901060 -8.7126936 37.037140
#> Scotland         2.329757  0.5518073  1.2217768  3.408673
#> Wales            1.317785  0.6385782 -0.1467899  2.329896
```
