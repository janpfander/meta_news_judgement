---
title: "Analyze simulated data fake news review"
bibliography: bibliography.bib
author: "Anonymous"
date: "2023-04-27"
output: 
  html_document: 
    keep_md: yes
---


```r
# load required packages
library("lme4")        # model specification / estimation
library("lmerTest")    # provides p-values in the output
library("tidyverse")   # data wrangling and visualisation
library("afex")        # anova and deriving p-values from lmer
library("broom")       # extracting data from model fits 
library("metafor")       # doing mata analysis
```

### Why this document?

The aim of this document is to inform the analysis we will eventually pre-register. We want to compare the performance of different meta-analytic models here.

In our `simulation_generate_data` document, we have generated two data frames: 

1. `raw.csv`, participant-level data with several studies (each containing several samples)
2. `meta_wide.csv`, summarized, sample-level data of the former

We will compare different meta-analytic models run on the summarized data to the benchmark of a linear mixed model run on the raw data. We will interpret all models with regard to how well they recover the parameters we generated our data with. 

##  Benchmark: raw (participant-level) data


```r
# read data
raw_meta_data <- read_csv("./data_from_simulation/raw.csv")

# we use the original accuracy measure that we generated our data with
raw_meta_data <- raw_meta_data %>% 
  select(- (contains("accuracy") & !contains("original"))
         ) %>% 
  rename(accuracy = accuracy_original)
```

The parameters we used to generate the data were:


```r
# define all the parameters as in the data generating function
n_subj  = 200 # number of participants (control + treatment)
n_fake  = 5   # number of fake news items
n_true  = 5   # number of true news items
beta_0  = 0.5 # intercept; i.e., the grand mean accuracy rating
beta_v  = 0.3 # main effect of veracity
beta_c  = 0.1 # main effect of condition
beta_vc = 0.05 # interaction between veracity and condition
subj_0   = 0.1 # by-subject random intercept sd
subj_1   = 0.1 # by-subject random slope sd
subj_rho     =  .2 # correlation between intercept and slope 
sigma   =  .2
samp_rho = 0.2 # correlation between intercept and slope by sample
samp_0 = 0.1 # by-subject random intercept sd
samp_1 = 0.1 # by-subject random slope sd
```

Now, we run the same model on the simulated data that we used to generate that data in the first place. This provides us with estimates of the parameters. 


```r
# Attention, running this model takes a couple of minutes. 
# We therefor store the results in a data frame that we can reload. 
filename <- "data_from_simulation/mixed_model_results_simulation.csv" # change for new analyses
if (!file.exists(filename)) {
  
  # fit a linear mixed-effects model with by sample AND by participants random effects
  data_generating_model <- lmer(accuracy ~ 1 + veracity_effect_code + 
                                  condition_effect_code + 
                                  veracity_effect_code*condition_effect_code + 
                                  (1 + veracity | unique_sample_id/subject_id),
                                data = raw_meta_data) %>% 
    tidy()
  
  write_csv(data_generating_model, filename)
}

# read saved model results
data_generating_model <- read_csv(filename)
```

```
## Rows: 11 Columns: 8
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): effect, group, term
## dbl (5): estimate, std.error, statistic, df, p.value
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

We can then compare our estimates with the parameters.


|effect   |group                       |term                                       |parameter | value| estimate| std.error| statistic|      df| p.value|
|:--------|:---------------------------|:------------------------------------------|:---------|-----:|--------:|---------:|---------:|-------:|-------:|
|fixed    |NA                          |(Intercept)                                |beta_0    |  0.50|    0.507|     0.007|    72.789| 146.232|    0.00|
|fixed    |NA                          |veracity_effect_code                       |beta_v    |  0.30|    0.292|     0.007|    40.305| 146.024|    0.00|
|fixed    |NA                          |condition_effect_code                      |beta_c    |  0.10|    0.101|     0.014|     7.256| 146.232|    0.00|
|fixed    |NA                          |veracity_effect_code:condition_effect_code |beta_vc   |  0.05|    0.010|     0.014|     0.692| 146.024|    0.49|
|ran_pars |subject_id:unique_sample_id |sd__(Intercept)                            |subj_0    |  0.10|    0.094|        NA|        NA|      NA|      NA|
|ran_pars |subject_id:unique_sample_id |cor__(Intercept).veracitytrue              |subj_rho  |  0.20|   -0.328|        NA|        NA|      NA|      NA|
|ran_pars |subject_id:unique_sample_id |sd__veracitytrue                           |subj_1    |  0.10|    0.091|        NA|        NA|      NA|      NA|
|ran_pars |unique_sample_id            |sd__(Intercept)                            |samp_0    |  0.10|    0.091|        NA|        NA|      NA|      NA|
|ran_pars |unique_sample_id            |cor__(Intercept).veracitytrue              |samp_rho  |  0.20|   -0.383|        NA|        NA|      NA|      NA|
|ran_pars |unique_sample_id            |sd__veracitytrue                           |samp_1    |  0.10|    0.088|        NA|        NA|      NA|      NA|
|ran_pars |Residual                    |sd__Observation                            |sigma     |  0.20|    0.184|        NA|        NA|      NA|      NA|

## Summarized meta data


```r
# store data
meta_wide <- read_csv("data_from_simulation/meta.csv")
```

```
## Rows: 266 Columns: 19
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (5): sample_id, replication, condition, accuracy_scale, unique_sample_id
## dbl (14): paper_id, n_observations, n_subj, n_news, accuracy_scale_numeric, ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# we use the original accuracy measure that we generated our data with
meta_wide <- meta_wide %>% 
  select(- (contains("accuracy") & !contains("original"))
         ) %>% 
  rename_with(~str_remove(., "original_"), contains("original"))
```

### Calculate effect size

We will calculate four different effect sizes, along two axes: 

1. Assuming independence vs. assuming dependence

An _independence_ assuming effect size is not exactly adequate, since they were measured _within_ participants. A _dependence_ assuming effect size takes into account the correlation coefficient between true and fake news. The problem is that we don't have the actual correlation coefficient for each effect size. Instead, we need to impute an average value that we extract from the raw data.

2. Standardized vs. Raw

We calculate both _standardized_ and _raw_ effect sizes. Standardized means that effects are measured in standard deviations. It is a neat and common way to compare across different scales of measurement. Raw means that the estimate is on the original scale of the measure (in our simulated case, 0 to 1). 

#### Assuming independence

```r
# we use the escalc function from the metafor package
# standardized mean difference (SMD)
meta_independent_d <- escalc(measure="SMD", 
                                 # diff = true (m1i) - fake (m2i)
                                 m2i= mean_accuracy_fake, sd2i=sd_accuracy_fake, n2i=n_subj,
                                       m1i=mean_accuracy_true, sd1i=sd_accuracy_true, n1i=n_subj, 
                                       data = meta_wide) 

# (un-standardized/raw) mean difference (MD)
meta_independent_d_raw <- escalc(measure="MD", 
                                 # diff = true (m1i) - fake (m2i)
                                 m2i= mean_accuracy_fake, sd2i=sd_accuracy_fake, n2i=n_subj,
                                       m1i=mean_accuracy_true, sd1i=sd_accuracy_true, n1i=n_subj, 
                                       data = meta_wide) 
```


#### Assuming dependence


```r
# calculate the average correlation between fake news accuracy ratings and true news accuracy ratings for each sample.
correlations_by_sample <- raw_meta_data %>%
  # step 1: for each participant, calculate means of fake and true
  group_by(paper_id, sample_id, subject_id, veracity, condition, replication) %>% 
  summarize(mean_accuracy = mean(accuracy)) %>% 
  pivot_wider(names_from = veracity, values_from = mean_accuracy, names_prefix = "accuracy_") %>% 
  # step 2: for each sample, calculate correlations of means of fake and true
  group_by(paper_id, sample_id, condition) %>% 
  summarize(r = cor(accuracy_fake, accuracy_true)) 
```

```
## `summarise()` has grouped output by 'paper_id', 'sample_id', 'subject_id',
## 'veracity', 'condition'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'paper_id', 'sample_id'. You can override
## using the `.groups` argument.
```

```r
# step 3: get average intra-sample correlation
average_correlation <- correlations_by_sample %>% 
    ungroup() %>% 
  summarize(average_r = mean(r)) %>% 
  pull(average_r)
average_correlation
```

```
## [1] 0.4202184
```

```r
ggplot(correlations_by_sample, aes(x = r)) + 
  geom_histogram() +
  geom_vline(xintercept = average_correlation, colour = "red")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](simulation_analyze_data_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

With this correlation, we can proceed to calculating the standardized and un-standardized/raw mean differences _accounting for dependence_. 


```r
# "SMCC" for the standardized mean change using change score standardization 
# use escalc function
meta_dependent_d <- escalc(measure="SMCC", 
                      # diff = true (m1i) - fake (m2i)
                      m2i= mean_accuracy_fake, 
                      sd2i=sd_accuracy_fake, ni=n_subj, m1i=mean_accuracy_true,
                      sd1i=sd_accuracy_true, data = meta_wide %>% 
                        mutate(r = average_correlation), ri = r)

# "MC" for the raw mean change
# use escalc function
meta_dependent_d_raw <- escalc(measure="MC", 
                          # diff = true (m1i) - fake (m2i)
                          m2i= mean_accuracy_fake, 
                          sd2i=sd_accuracy_fake, ni=n_subj, m1i=mean_accuracy_true,
                          sd1i=sd_accuracy_true, data = meta_wide %>% 
                            mutate(r = average_correlation), ri = r)
```

### Calculate meta models

The aim of this section is to compare the estimates generated by different meta-analysis models.

The parameter of interest is `beta_v` (the effect of `veracity`, i.e. the difference between true and fake news). We will progress to the 'most complete' model specification by comparing along 4 axes: 
* assuming _independence_ vs. _dependence_ regarding `veracity`
* unstandardized/raw mean differences vs. standardized mean differences
* fixed vs. random vs. multilevel random effects model 


Models with (raw) mean differences assuming _independence_. 

```r
# Fixed effect model
fixed_independent <- metafor::rma(yi, vi, data = meta_independent_d_raw, method = "FE")
# Random effect model
random_independent <- metafor::rma(yi, vi, data = meta_independent_d_raw)
# Multilevel random effect model
multilevel_independent_raw <-  metafor::rma.mv(yi, vi, random = ~ 1 | unique_sample_id / observation_id, data=meta_independent_d_raw)
```

Models with (raw) mean differences assuming _dependence_. 

```r
# Fixed effect model
fixed_dependent <- metafor::rma(yi, vi, data = meta_dependent_d_raw, method = "FE")
# Random effect model
random_dependent <- metafor::rma(yi, vi, data = meta_dependent_d_raw)
# Multilevel random effect model
multilevel_dependent_raw <-  metafor::rma.mv(yi, vi, random = ~ 1 | unique_sample_id / observation_id, data=meta_dependent_d_raw)
```

Models with _standardized_ mean differences assuming _independence_. 

```r
# Fixed effect model
fixed_independent <- metafor::rma(yi, vi, data = meta_independent_d, method = "FE")
# Random effect model
random_independent <- metafor::rma(yi, vi, data = meta_independent_d)
# Multilevel random effect model
multilevel_independent <-  metafor::rma.mv(yi, vi, random = ~ 1 | unique_sample_id / observation_id, data=meta_independent_d)
```

Models with _standardized_ mean differences assuming _dependence_. 

```r
# Fixed effect model
fixed_dependent <- metafor::rma(yi, vi, data = meta_dependent_d, method = "FE")
# Random effect model (at observation level)
random_dependent <- metafor::rma(yi, vi, data = meta_dependent_d)
# Multilevel random effect model
multilevel_dependent <-  metafor::rma.mv(yi, vi, random = ~ 1 | unique_sample_id / observation_id, data=meta_dependent_d)
```

### Compare meta models

Focusing on the standardized and dependency accounting effect sizes, we can compare the multilevel model to the fixed to the random one.


```r
modelsummary::modelsummary(list(
  "Fixed" = fixed_dependent,
  "Random" = random_dependent,
  "Multilevel" = multilevel_dependent), 
  title = 'Standardized Effect sizes')
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Standardized Effect sizes</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Fixed </th>
   <th style="text-align:center;"> Random </th>
   <th style="text-align:center;"> Multilevel </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> overall </td>
   <td style="text-align:center;"> 1.192 </td>
   <td style="text-align:center;"> 1.286 </td>
   <td style="text-align:center;"> 1.289 </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1.5px">  </td>
   <td style="text-align:center;box-shadow: 0px 1.5px"> (0.004) </td>
   <td style="text-align:center;box-shadow: 0px 1.5px"> (0.026) </td>
   <td style="text-align:center;box-shadow: 0px 1.5px"> (0.034) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 266 </td>
   <td style="text-align:center;"> 265 </td>
   <td style="text-align:center;"> 265 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AIC </td>
   <td style="text-align:center;"> 11420.5 </td>
   <td style="text-align:center;"> 306.5 </td>
   <td style="text-align:center;"> −186.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BIC </td>
   <td style="text-align:center;"> 11424.1 </td>
   <td style="text-align:center;"> 313.7 </td>
   <td style="text-align:center;"> −175.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> i2 </td>
   <td style="text-align:center;"> 0.97860574904228 </td>
   <td style="text-align:center;"> 0.980718374091535 </td>
   <td style="text-align:center;">  </td>
  </tr>
</tbody>
</table>

The multilevel model has the best fit: The Akaike (AIC) and Bayesian Information Criterion (BIC) are lower (more negative) for the _dependency_ model, which indicates favorable performance.

Focusing on multilevel models only, we compare our _standardized effect_ models regarding their respective estimate of `veracity` (labeled as `overall` below).


```r
modelsummary::modelsummary(list("Accounting for Dependency" = multilevel_dependent, 
                                "Assuming Independence" = multilevel_independent), 
                           title = 'Standardized Effect sizes')
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Standardized Effect sizes</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Accounting for Dependency </th>
   <th style="text-align:center;">  Assuming Independence </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> overall </td>
   <td style="text-align:center;"> 1.289 </td>
   <td style="text-align:center;"> 1.396 </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1.5px">  </td>
   <td style="text-align:center;box-shadow: 0px 1.5px"> (0.034) </td>
   <td style="text-align:center;box-shadow: 0px 1.5px"> (0.037) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 265 </td>
   <td style="text-align:center;"> 265 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AIC </td>
   <td style="text-align:center;"> −186.2 </td>
   <td style="text-align:center;"> −126.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BIC </td>
   <td style="text-align:center;"> −175.5 </td>
   <td style="text-align:center;"> −115.7 </td>
  </tr>
</tbody>
</table>

We can see that accounting for dependency, in this case, leads to a smaller standardized effect. The Akaike (AIC) and Bayesian Information Criterion (BIC) are lower (more negative) for the _dependency_ model, which indicates favorable performance. 

Taken together, accounting for _dependency_ thus seems the better, more conservative option to go for.

We do the same comparison for our _raw_ (non-standardized effect) models.  


```r
modelsummary::modelsummary(list("Accounting for Dependency" = multilevel_dependent_raw, 
                                "Assuming Independence" = multilevel_independent_raw), 
                           title = 'Raw Effect sizes')
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Raw Effect sizes</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Accounting for Dependency </th>
   <th style="text-align:center;">  Assuming Independence </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> overall </td>
   <td style="text-align:center;"> 0.292 </td>
   <td style="text-align:center;"> 0.292 </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1.5px">  </td>
   <td style="text-align:center;box-shadow: 0px 1.5px"> (0.007) </td>
   <td style="text-align:center;box-shadow: 0px 1.5px"> (0.007) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 265 </td>
   <td style="text-align:center;"> 265 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AIC </td>
   <td style="text-align:center;"> −1063.9 </td>
   <td style="text-align:center;"> −1008.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BIC </td>
   <td style="text-align:center;"> −1053.2 </td>
   <td style="text-align:center;"> −998.0 </td>
  </tr>
</tbody>
</table>
We can see that the effect estimates are the same for both models. The Akaike (AIC) and Bayesian Information Criterion (BIC) are, again, slightly lower for the _dependence_ model. 

## Compare sample-level meta models to participant-level linear mixed models

### Raw effect

We will use the _raw_ effect size model assuming _dependence_ (`multilevel_dependent_raw`) and the _sample and participant_ participant random effects model (data_generating_model). 

The effect of interest is `veracity`, corresponding to our parameter `beta_v`. 


```r
# compare estimates to parameters

# meta data analysis: get a tidy table of results
meta_model <- broom.mixed::tidy(multilevel_dependent_raw) %>% 
  select(estimate, std.error) %>% 
  mutate(model = "meta")

# raw (participant-level) data analysis: get a tidy table of results
linear_model <- data_generating_model %>% 
  filter(term == "veracity_effect_code") %>% 
  select(estimate, std.error) %>% 
  mutate(model = "linear")

rbind(linear_model, meta_model) %>% 
    mutate_if(is.numeric, round, 3) %>%
  mutate(parameter = "beta_v", 
         value = rep(beta_v, times = nrow(.))) %>%
  select(3, 1:2, 4:5) %>% 
  knitr::kable()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> model </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:left;"> parameter </th>
   <th style="text-align:right;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> linear </td>
   <td style="text-align:right;"> 0.292 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:left;"> beta_v </td>
   <td style="text-align:right;"> 0.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> meta </td>
   <td style="text-align:right;"> 0.292 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:left;"> beta_v </td>
   <td style="text-align:right;"> 0.3 </td>
  </tr>
</tbody>
</table>

### Standardized effect

In our actual analysis, we will use standardized mean differences as effect measures for veracity. In order to test the performance of this model, we set a new benchmark participant-level model on a standardized (divided by standard deviation within sample and within veracity) measure of accuracy. 


```r
# make standardized measure of accuracy
raw_meta_data <- raw_meta_data %>% 
  mutate(unique_sample_id = paste(paper_id, sample_id, sep = "_")
         ) %>% 
  group_by(unique_sample_id) %>% 
  mutate(accuracy_std = accuracy/sd(accuracy), 
         sd_by_sample = sd(accuracy)) %>% 
  ungroup()

# Attention, running this model takes a couple of minutes. 
# We therefor store the results in a data frame that we can reload. 
filename <- "data_from_simulation/std_mixed_model_results_simulation.csv" # change for new analyses
if (!file.exists(filename)) {
  
  # fit a linear mixed-effects model on standardized outcome variable
linear_std <- lmer(accuracy_std ~ 1 + veracity + (1 + veracity | unique_sample_id/subject_id),
                data = raw_meta_data) %>% 
    tidy()
  
  write_csv(linear_std, filename)
}

# read saved model results
linear_std <- read_csv(filename)
```

```
## Rows: 9 Columns: 8
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): effect, group, term
## dbl (5): estimate, std.error, statistic, df, p.value
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
# compare estimates to parameters

# meta data analysis: get a tidy table of results
meta_model <- broom.mixed::tidy(multilevel_dependent) %>% 
  select(estimate, std.error) %>% 
  mutate(model = "meta")

# raw (participant-level) data analysis: get a tidy table of results
linear_model <- linear_std %>% 
  filter(term == "veracitytrue") %>% 
  select(estimate, std.error) %>% 
  mutate(model = "linear")

# calculate standard deviation for accuracy from raw and meta data respectively 
# to be able to standardize the parameter
mean_sd_meta <- (mean(meta_wide$sd_accuracy_true) + mean(meta_wide$sd_accuracy_fake))/2
mean_sd_raw <- mean(raw_meta_data$sd_by_sample)

rbind(linear_model, meta_model) %>% 
    mutate_if(is.numeric, round, 3) %>%
  mutate(parameter = "beta_v", 
         value_original = beta_v, 
         # we set in the sd value from the meta data here
         # if we do so for the raw data, the results change
         value_std = beta_v / mean_sd_meta) %>%
  select(3, 1:2, 4:6) %>% 
  rename(estimate_std = estimate) %>% 
  knitr::kable()
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> model </th>
   <th style="text-align:right;"> estimate_std </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:left;"> parameter </th>
   <th style="text-align:right;"> value_original </th>
   <th style="text-align:right;"> value_std </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> linear </td>
   <td style="text-align:right;"> 1.108 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:left;"> beta_v </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 1.429502 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> meta </td>
   <td style="text-align:right;"> 1.289 </td>
   <td style="text-align:right;"> 0.034 </td>
   <td style="text-align:left;"> beta_v </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 1.429502 </td>
  </tr>
</tbody>
</table>







