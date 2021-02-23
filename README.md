
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
             family = poisson(link = "identity"),
             formula = id_formula(obs$data[[1]],
                                  scale = ~ (time | location),
                                  cmean =  ~ 1,
                                  lcsd =  ~ 1),
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

#> Warning: The global prior 'normal(0, 0.5)' of class 'b' will not be used in the
#> model as all related coefficients have individual priors already. If you did not
#> set those priors yourself, then maybe brms has assigned default priors. See ?
#> set_prior and ?get_prior for more details.
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 103 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 1522 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#> http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#> Warning: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> http://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.07, indicating chains have not mixed.
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
#> Warning: There were 103 divergent transitions after warmup. Increasing
#> adapt_delta above 0.8 may help. See http://mc-stan.org/misc/
#> warnings.html#divergent-transitions-after-warmup
#>  Family: poisson 
#>   Links: mu = identity 
#> Formula: secondary ~ idbrms_convolve(primary, scale, cmean, lcsd, cmax, index, cstart, init_obs) 
#>          scale ~ (time | location)
#>          cmean ~ 1
#>          lcsd ~ 1
#>    Data: data (Number of observations: 452) 
#> Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup samples = 4000
#> 
#> Group-Level Effects: 
#> ~location (Number of levels: 4) 
#>                                 Estimate Est.Error l-95% CI u-95% CI Rhat
#> sd(scale_Intercept)                 1.49      0.81     0.60     3.73 1.02
#> sd(scale_time)                      0.01      0.01     0.01     0.03 1.02
#> cor(scale_Intercept,scale_time)    -0.87      0.24    -1.00    -0.07 1.03
#>                                 Bulk_ESS Tail_ESS
#> sd(scale_Intercept)                  193      363
#> sd(scale_time)                       242      441
#> cor(scale_Intercept,scale_time)      133       95
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> scale_Intercept    -3.51      0.33    -4.23    -2.80 1.01      158      359
#> cmean_Intercept     4.21      0.34     3.84     4.57 1.07       56       31
#> lcsd_Intercept      0.80      0.04     0.74     0.85 1.06       71       31
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
#> , , scale_Intercept
#> 
#>                     Estimate Est.Error      Q2.5      Q97.5
#> England          -0.47984315 0.3329862 -1.191670  0.2459475
#> Northern Ireland -1.92984717 0.3356053 -2.643787 -1.1931039
#> Scotland          0.04918841 0.3335770 -0.649727  0.7935367
#> Wales            -0.56955616 0.3376644 -1.281036  0.1730682
#> 
#> , , scale_time
#> 
#>                      Estimate    Est.Error          Q2.5       Q97.5
#> England          0.0045259014 0.0003163115  0.0041734523 0.004888939
#> Northern Ireland 0.0179898614 0.0008986803  0.0163570630 0.019587284
#> Scotland         0.0004113214 0.0005680096 -0.0006636893 0.001533194
#> Wales            0.0049510168 0.0007366337  0.0035914081 0.006406404
```
