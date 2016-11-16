---
title: "cran-comments"
output: html_document
---
## Resubmission
This is a resubmission. In this version I have:

* Modified the validann functions 'cw_fn' and 'structural_valid' such that 
  the MCW input relative importance measure is only returned for models
  of class 'ann' with "tanh" hidden activation function (file validann.R).

## Test environments
* local Windows 7 install, R 3.2.2 
* ubuntu 12.04 (on travis-ci), R 3.3.0
* CRAN win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Greer B. Humphrey <greer.humphrey@student.adelaide.edu.au>'