
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
library(dplyr)
library(purrr)
```

  - Load data

<!-- end list -->

``` r
obs <- get_uk_obs_grid(
  date_from = Sys.Date() - 7 * 16,
  date_to = Sys.Date()
) %>%
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
fit <- idbrm(
  data = obs$data[[1]][location %in% "England"],
  family = poisson(link = "identity"),
  formula = id_formula(obs$data[[1]],
    scale = ~ s(time),
    cmean = ~1,
    lcsd = ~1
  ),
  priors = id_priors(obs$data[[1]],
    scale = c(log(0.05), 1),
    cmean = c(log(5), 0.25),
    lcsd = c(log(0.5), 0.25)
  )
)
#> Warning: The global prior 'normal(0, 1)' of class 'b' will not be used in the model as all related coefficients
#> have individual priors already. If you did not set those priors yourself, then maybe brms has assigned default
#> priors. See ?set_prior and ?get_prior for more details.
#> Warning: The global prior 'normal(0, 0.5)' of class 'b' will not be used in the model as all related coefficients
#> have individual priors already. If you did not set those priors yourself, then maybe brms has assigned default
#> priors. See ?set_prior and ?get_prior for more details.
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 44 divergent transitions after warmup. See
#> http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#> http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#bulk-ess
```

  - Summarise model

<!-- end list -->

``` r
summary(fit)
#> Warning: There were 44 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://
#> mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#>  Family: poisson 
#>   Links: mu = identity 
#> Formula: secondary ~ idbrms_convolve(primary, scale, cmean, lcsd, cmax, index, cstart, init_obs) 
#>          scale ~ s(time)
#>          cmean ~ 1
#>          lcsd ~ 1
#>    Data: data (Number of observations: 108) 
#> Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup samples = 4000
#> 
#> Smooth Terms: 
#>                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(scale_stime_1)     4.66      1.37     2.73     8.20 1.01      395      603
#> 
#> Population-Level Effects: 
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> scale_Intercept    -3.58      0.01    -3.59    -3.57 1.00     3508     2707
#> cmean_Intercept     2.18      0.19     1.82     2.56 1.00     1756     2008
#> lcsd_Intercept      0.77      0.05     0.67     0.86 1.00     1890     2082
#> scale_stime_1      -3.60      0.45    -4.49    -2.71 1.00     2492     2336
#> 
#> Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```
