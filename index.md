EMMA Prototype
================
true
10-13-2021

    ## Loading required package: foreach

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    ## Loading required package: iterators

    ## Loading required package: parallel

    ## Loading required package: sp

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

The details are given in
\[@slingsby_near-real_2020;@wilson_climatic_2015\], but in short what we
do is estimate the age of a site by calculating the years since the last
fire. We then fit a curve to model the recovery of vegetation (measured
using NDVI) as a function of it’s age. For this we use a negative
exponential curve with the following form:

$$\\mu\_{i,t}=\\alpha_i+\\gamma_i\\Big(1-e^{-\\frac{age\_{i,t}}{\\lambda_i}}\\Big)$$

where *μ*<sub>*i*, *t*</sub> is the expected NDVI for site *i* at time
*t*

The observed greenness *N**D**V**I*<sub>*i*, *t*</sub> is assumed to
follow a normal distribution with mean *μ*<sub>*i*, *t*</sub>
*N**D**V**I*<sub>*i*, *t*</sub> ∼ 𝒩(*μ*<sub>*i*, *t*</sub>, *σ*<sub>)</sub>

An additional level models the parameters of the negative exponential
curve as a function of environmental variables. This means that sites
with similar environmental conditions should have similar recovery
curves. The full model also includes a sinusoidal term to capture
seasonal variation, but lets keep it simple here.

## ADVI

We have `age` in years, a plot identifier `pid`. the observed ndvi `nd`
and two plot level environmental variable `env1`, which is mean annual
precipitation, and `env2`, which is the summer maximum temperature.

Lets load up our Stan model which codes the model described above. This
is not a particularly clever or efficient way of coding the model, but
it is nice and readable and works fine on this example dataset

How long did that take?

``` r
model_fit$time()$total
```

    ## [1] 118.7171

``` r
params=c("tau","alpha_mu","gamma_b2","gamma_b1","lambda_b1","lambda_b2")
posteriors=model_fit$draws(params) %>% 
  as_tibble() %>% 
  gather(parameter)
ggplot(posteriors,aes(x=value))+
  geom_density(fill="grey")+
  facet_wrap(~parameter,scales = "free")
```

![](index_files/figure-gfm/p1-1.png)<!-- -->

## Plot

``` r
posterior_summary %>% 
#  filter(pid %in% as.numeric(sample(levels(as.factor(posterior_summary$pid)),20))) %>% # just show a few
  ggplot(aes(x=age)) +
  geom_line(aes(y=mean),colour="blue") +
  geom_line(aes(y=ndvi),colour="black",lwd=0.5,alpha=0.3) +
#  geom_ribbon(aes(ymin=q5,ymax=q95),alpha=0.5)+
#  facet_wrap(~pid) +
  xlim(c(0,20))+
  labs(x="time since fire (years)",y="NDVI") +
  theme_bw()
```

    ## Warning: Removed 112 row(s) containing missing values (geom_path).
    ## Removed 112 row(s) containing missing values (geom_path).

![](index_files/figure-gfm/plot-1.png)<!-- -->

# Spatial Predictions

This section is not yet working - need to get the coordinates in the
original data set.

``` r
stan_spatial <- posterior_summary %>% 
  left_join(data,by="cellID") %>% #pid=gsub("[]]","",gsub(".*[[]","",variable))) %>% 
  select(cellID,x,y,date, age,ndvi,mean,q5,q95)


# pred_rast <- foreach(t=sort(unique(stan_spatial$date)),.combine=stack) %do% {
#   stan_spatial %>% 
#     filter(date==t) %>%
#     select(x,y,age,nd,mean,q5) %>% 
#     rasterFromXYZ()
# }

stan_spatial %>% 
  filter(date<as_date("2004-01-01")) %>% 
    ggplot(aes(x=x,y=y,fill=mean))+
    geom_tile()+
    facet_wrap(~date)+
  coord_sf()
```
