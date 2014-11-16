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
#####sep.binom()
**Binomial Population sensitivity**  
Calculates population sensitivity for detecting disease, assuming imperfect test sensitivity and specificity and representative sampling, using binomial distribution (assumes large or unknown population size and that cut-point number of reactors for a positive result = 1). Used by function sep().  

*Usage*  
`sep.binom(n, pstar, se = 1, sp = 1)`  

#####sep.hypergeo() 
**Hypergeometric Population sensitivity**  
Calculates population sensitivity for detecting disease, assuming imperfect test sensitivity, perfect test specificity and representative sampling, using hypergeometric approximation (assumes known population size). Used by function sep().  

*Usage*  
`sep.hypergeo(N, n, d, se = 1)`  

#####sep.exact()
**Population sensitivity for census (all units tested)**  
Calculates population sensitivity for detecting disease assuming imperfect test sensitivity, perfect test specificity and a census of all units in the population.  

*Usage*  
`sep.exact(d=1, se = 1)`  

#####spp()
**Population specificity**  
Calculates population specificity assuming representative sampling.  

*Usage*  
`spp(n, sp)`  

#####sep()
**Population sensitivity **  
Calculates population sensitivity using appropriate method, depending on whether or not N provided (hypergeometric if N provided, binomial otherwise), assuming perfect test specificity and representative sampling. Uses functions sep.() and sep.hypergeo() for calculations.  

*Usage*  
`sep(N = NA, n, pstar, se=1)`  

#####sep.var.se()
**Population sensitivity for varying unit sensitivity**  
Calculates population-level sensitivity where unit sensitivity varies and using the appropriate method, depending on whether or not N provided (hypergeometric if N provided, binomial otherwise), assuming perfect test specificity and representative sampling.  

*Usage*  
`sep.var.se(N=NA, se, pstar)`  

#####sep.sys()
**2-stage population sensitivity**  
Calculates population-level (system) sensitivity for representative 2-stage sampling (sampling of clusters and units within clusters), assuming imperfect test sensitivity and perfect test specificity.  

*Usage*  
`sep.sys<- function(H=NA, N=NA, n, pstar.c, pstar.u, se=1)`  

####Sample size estimation
#####n.binom()
**Binomial sample size**  
Calculates sample size for demonstrating freedom or detecting disease using binomial approach and assuming imperfect test sensitivity, perfect test specificity and representative sampling.  

*Usage*  
`n.binom(sep, pstar, se = 1)`  

#####n.hypergeo()
**Hypergeometric sample size**  
Calculates sample size for demonstrating freedom or detecting disease using hypergeometric approximation and assuming imperfect test sensitivity, perfect test specificity and representative sampling.  

*Usage*  
`n.hypergeo(sep, N, d, se = 1)`  

#####n.freedom()
**Freedom sample size **  
Calculates sample size for demonstrating freedom or detecting disease using the appropriate method, depending on whether or not N provided (hypergeometric if N provided, binomial otherwise), assuming imperfect test sensitivity, perfect test specificity and representative sampling.  

*Usage*  
`n.freedom(N=NA, sep=0.95, pstar,se=1)`  

#####n.2stage()
**2-stage freedom sample size**  
Calculates sample sizes for a 2-stage representative survey (sampling of clusters and units within clusters) for disease freedom or detection, assuming imperfect test sensitivity, perfect test specificity and representative sampling.  

*Usage*  
`n.2stage(H=NA, N=NA, sep.sys=0.95, sep.c, pstar.c, pstar.u, se=1)`  

####Miscellaneous functions
#####pstar.calc()
**Design prevalence back-calculation**  
Calculates design prevalence required for given sample size and desired population-level sensitivity, assuming imperfect test sensitivity, perfect test specificity and representative sampling.

*Usage*  
`pstar.calc(N=NA, n, sep, se)`  

####Functions allowing for imperfect specificity

###Risk-based freedom surveys 
####Population sensitivity estimation
pfree.1

pfree.calc

pfree.equ

####Sample size estimation
n.pfree

####Miscellaneous functions
disc.prior

sep.pfree

sep.prior

###Probability of freedom estimation
####Probability of freedom

####Miscellaneous functions

####Background functions

###Prevalence estimation
####Prevalence and CI estimation

####Sample size calculation

###Combining tests
se.series

se.parallel

sp.series

sp.parallel

###Pooled testing
sep.pooled

n.pooled
