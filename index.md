EMMA Prototype
================
true
10-13-2021

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

## Fitting Overview

### Geographic region

``` r
betas=model_results %>% 
  filter(type=="beta")

ggplot(betas,aes(y=xname, xmin=q5,x=median,xmax=q95))+
  geom_pointrange(fill="grey")+
  facet_wrap(~parameter,nrow=1)+
  geom_vline(xintercept=0,col="grey")+
  xlab(expression(beta*" (regression coefficient +/- 95% CI)"))
```

![](index_files/figure-gfm/p1-1.png)<!-- -->

## Plot

``` r
model_results %>% 
  filter(pid %in% as.numeric(sample(levels(as.factor(posterior_summary$pid)),20))) %>% # just show a few
  ggplot(aes(x=age)) +
 geom_line(aes(y=mean),colour="blue") +
  geom_line(aes(y=ndvi),colour="black",lwd=0.5,alpha=0.3) +
  geom_ribbon(aes(ymin=q5,ymax=q95),alpha=0.5)+
  facet_wrap(~pid) +
#  xlim(c(0,25))+ ylim(c(0,10))+
  labs(x="time since fire (years)",y="NDVI") +
  theme_bw()
```

# Spatial Predictions

Maps of spatial parameters.

``` r
plot(spatial_outputs)
```

![](index_files/figure-gfm/compare_data2-1.png)<!-- -->
