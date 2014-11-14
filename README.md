RSurveillance
=============

`RSurveillance` provides a range of functions for the design and
    analysis of disease surveillance activities. These functions were
    originally developed for animal health surveillance activities but can be
    equally applied to aquatic animal, wildlife, plant and human health
    surveillance activities. Utilities are included for sample size calculation
    and analysis of representative surveys for disease freedom, risk-based
    studies for disease freedom and for prevalence estimation.

You can track (and contribute to) development of `RSurveillance`
at https://github.com/evansergeant/RSurveillance.

Installation
------------

RSurveillance has not yet been released on CRAN.

To install the development version of RSurveillance, copy the current version of the R Library 
from https://github.com/evansergeant/RSurveillanceLibrary to your local R library.

Usage
-----
`RSurveillance` functions are organised into three broad areas of surveillance, namely representative freedom surveys,
risk-based freedom surveys, probability of freedom estimation and prevalence estimation. Within these areas functions can be further grouped according to purpose (depending on surveillance area/purpose), such as sample size calculation, population sensitivity estimation, prevalence estimation and background functions. Specific functions are summarised below according to these categories.

###Representative freedom surveys
####Population sensitivity estimation
**sep.binom()**  
Calculates population sensitivity for detecting disease,
assuming imperfect test sensitivity and specificity and representative sampling,
using binomial distribution (assumes large or unknown population size and that 
cut-point number of reactors for a positive result = 1)  

*Usage*  
`sep<- sep.binom(n, pstar, se = 1, sp = 1)`  
n = sample size = number of units tested (integer), scalar or vector    
pstar = design prevalence as a proportion (scalar or vector of same length as n)  
se = unit sensitivity of test (proportion), default = 1 (scalar or vector of same length as n)  
sp = unit specificity of test (proportion), default = 1 (scalar or vector of same length as n)  

Returns a vector of population-level sensitivities  

sep.hypergeo

sep.exact

spp

sep

sep.var.se

sep.sys

####Sample size estimation

####Functions allowing for imperfect specificity

###Risk-based freedom surveys 
####Population sensitivity estimation

####Sample size estimation

####Background functions

###Probability of freedom estimation
####Probability of freedom

####Miscellaneous functions

####Background functions

###Prevalence estimation
####Prevalence and CI estimation

####Sample size calculation

