---
title: "cran-comments"
output: html_document
---

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
* checking R code for possible problems ... NOTE
ann: no visible global function definition for 'runif'
ann: no visible global function definition for 'optim'
ann: no visible global function definition for 'predict'
observed.ann: no visible global function definition for 'fitted'
observed.ann: no visible global function definition for 'residuals'
observed.nnet: no visible global function definition for 'fitted'
observed.nnet: no visible global function definition for 'residuals'
plot.validann: no visible global function definition for
  'dev.interactive'
plot.validann: no visible global function definition for
  'devAskNewPage'
plot.validann: no visible global function definition for 'layout'
plot.validann: no visible global function definition for 'par'
plot.validann: no visible global function definition for 'plot'
plot.validann: no visible global function definition for 'colors'
plot.validann: no visible global function definition for 'abline'
plot.validann: no visible global function definition for 'qqplot'
plot.validann: no visible global function definition for 'points'
plot.validann: no visible global function definition for 'title'
plot.validann: no visible global function definition for 'legend'
plot.validann: no visible global function definition for 'hist'
plot.validann: no visible global function definition for 'dnorm'
plot.validann: no visible global function definition for 'lines'
plot.validann: no visible global function definition for 'qqnorm'
plot.validann: no visible global function definition for 'acf'
plot.validann: no visible global function definition for 'pacf'
plot.validann: no visible global function definition for 'qnorm'
predict.ann: no visible global function definition for 'fitted'
predictive_valid: no visible global function definition for 'median'
predictive_valid: no visible global function definition for 'sd'
predictive_valid: no visible global function definition for 'cor'
profile_sa: no visible binding for global variable 'quantile'
profile_sa: no visible global function definition for 'quantile'
profile_sa: no visible global function definition for 'predict'
profile_sa: no visible binding for global variable 'median'
stats: no visible global function definition for 'var'
stats: no visible global function definition for 'sd'
validann.ann: no visible global function definition for 'fitted'
validann.nnet: no visible global function definition for 'fitted'
Undefined global functions or variables:
  abline acf colors cor dev.interactive devAskNewPage dnorm fitted hist
  layout legend lines median optim pacf par plot points predict qnorm
  qqnorm qqplot quantile residuals runif sd title var
Consider adding
  importFrom("grDevices", "colors", "dev.interactive", "devAskNewPage")
  importFrom("graphics", "abline", "hist", "layout", "legend", "lines",
             "par", "plot", "points", "title")
  importFrom("stats", "acf", "cor", "dnorm", "fitted", "median", "optim",
             "pacf", "predict", "qnorm", "qqnorm", "qqplot", "quantile",
             "residuals", "runif", "sd", "var")
to your NAMESPACE file.
