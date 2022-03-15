EMMA Prototype
================
true

# Model Overview

The details are given in
\[@slingsby_near-real_2020;@wilson_climatic_2015\], but in short what we
do is estimate the age of a site by calculating the years since the last
fire. We then fit a curve to model the recovery of vegetation (measured
using NDVI) as a function of it’s age. For this we use a negative
exponential curve with the following form:

![\\mu\_{i,t}=\\alpha_i+\\gamma_i\\Big(1-e^{-\\frac{age\_{i,t}}{\\lambda_i}}\\Big)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_%7Bi%2Ct%7D%3D%5Calpha_i%2B%5Cgamma_i%5CBig%281-e%5E%7B-%5Cfrac%7Bage_%7Bi%2Ct%7D%7D%7B%5Clambda_i%7D%7D%5CBig%29 "\mu_{i,t}=\alpha_i+\gamma_i\Big(1-e^{-\frac{age_{i,t}}{\lambda_i}}\Big)")

where
![\\mu\_{i,t}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_%7Bi%2Ct%7D "\mu_{i,t}")
is the expected NDVI for site
![i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;i "i")
at time
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")

The observed greenness
![NDVI\_{i,t}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;NDVI_%7Bi%2Ct%7D "NDVI_{i,t}")
is assumed to follow a normal distribution with mean
![\\mu\_{i,t}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_%7Bi%2Ct%7D "\mu_{i,t}")

![NDVI\_{i,t}\\sim\\mathcal{N}(\\mu\_{i,t},\\sigma\_)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;NDVI_%7Bi%2Ct%7D%5Csim%5Cmathcal%7BN%7D%28%5Cmu_%7Bi%2Ct%7D%2C%5Csigma_%29 "NDVI_{i,t}\sim\mathcal{N}(\mu_{i,t},\sigma_)")

An additional level models the parameters of the negative exponential
curve as a function of environmental variables. This means that sites
with similar environmental conditions should have similar recovery
curves. The full model also includes a sinusoidal term to capture
seasonal variation, but lets keep it simple here.

## Workflow

This repository was developed using the Targets framework as follows.

## Results

### Regression Coefficients

These parameters represent the relationship of the following
environmental variables to the recovery trajectory.

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

## Recovery Trajectories

``` r
cells_with_long_records<-
  model_prediction %>% 
  group_by(cellID) %>% 
  summarize(n=n()) %>% 
  filter(n>100)

model_prediction %>% 
  filter(cellID%in%cells_with_long_records$cellID) %>% 
#  filter(pid %in% as.numeric(sample(levels(as.factor(posterior_summary$pid)),20))) %>% # just show a few
  ggplot(aes(x=age)) +
 geom_line(aes(y=median),colour="blue") +
  geom_line(aes(y=y_obs),colour="black",lwd=0.5,alpha=0.3) +
  geom_ribbon(aes(ymin=q5,ymax=q95),alpha=0.5)+
  facet_wrap(~cellID) +
  labs(x="time since fire (years)",y="NDVI") +
  theme_bw()
```

![](index_files/figure-gfm/plot-1.png)<!-- -->

## Spatial Predictions

Maps of spatial parameters in the model.

``` r
plot(spatial_outputs)
```

![](index_files/figure-gfm/compare_data2-1.png)<!-- -->
