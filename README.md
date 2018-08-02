# bRema: Building Energy Modeling and Analysis in R
[![Build Status](https://travis-ci.org/tinnaing347/bRema.svg?branch=master)](https://travis-ci.org/tinnaing347/bRema.svg?branch=master)
## Overview
**bRema** is Building Energy Modeling and Analysis package developed in R by **CUNY Building Performance Lab**. bRema allows users to call multiple functions and build desired temperature dependent change-point models from scratch and use their own statistical threshold for choosing the best model. Also, one can take advantage of built-in plotting functions to visualize models and perform additional statistical analysis using built-in stats functions. Moreover, bRema can model both retrofitted and unretrofitted building energy data.

## Installation

```r
install.packages('devtools')
#bRema uses plotly to plot parameter model graphs and timeseries
install.packages('plotly')
devtools::install_github('tinnaing347/bRema')
```
## Usage
On the very basic level, given a simple data set of outside air temperature and energy usage (either electricity or fuel) of a single building, bRema will perform segmented linear regression, spit out five different linear regression models that fit the data set and find the best model. On more advanced level, if one uses input that is structured in a similar format as built-in sample data set (See `bRema::unretrofit_utility`), bRema can batch-run hundreds of buildings in a matter of a few minutes and do lean analysis. The following is a very basic simple example that shows how bRema can model given a data frame of outside air temperature and usage:

```r
#monthly average outside air temperature
x <- c(78.61290, 78.77419, 73.73333, 58.06452, 53.46667, 50.35484,
        35.64516, 38.24138, 48.35484, 52.46667, 62.87097, 73.13333,
        80.25806, 80.80645, 73.56667, 60.70968, 52.13333, 40.45161,
        39.54839, 42.21429, 40.64516, 57.21212, 62.70968, 73.40000)
#monthly average electricity usage normalized by square footage
y <- c(0.05996058, 0.05959213, 0.05592924, 0.03800914, 0.03652459,
        0.03190152, 0.03549965, 0.03550670, 0.03681476, 0.03813243,
        0.04048421, 0.05412409, 0.06184637, 0.06345264, 0.05440611,
        0.03750763, 0.03192324, 0.03497471, 0.03475597, 0.03480360,
        0.03242668, 0.03065114, 0.03763059, 0.04835995)
#constructing utility data frame
util <- data.frame(OAT = x, usage = y, energy_type = 'Elec')
#segmented linear regression, modeling
result = bRema::run_model(util, plot_flag = TRUE)
#finding the best fitted model
best_model_list = bRema::main_best_model_func(result, energy_n = 'Elec')
best_model_list$figure
```
![](https://github.com/tinnaing347/bRema/blob/master/man/figures/readme_example_plot_1.png "A 3PC model for electricity")

If one has a dataset of multiple buildings energy usage similar to sample data provided in the package, one can use `bRema::batch_run` function to  batch run multiple buildings and perform more analysis.
```r
library(bRema)
#batch running and modelling
batch_result = batch_run(unretrofit_utility)
#a data frame that includes information about models and statistics
best_model_df = batch_result$best_result_df
#performing post model calculations such as heating load, and lean analysis ranking 
post_df = main_post_model(unretrofit_utility, best_model_df, rank_flag = TRUE)
```
License
----
MIT
