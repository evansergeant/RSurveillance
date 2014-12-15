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

To install the latest release on CRAN

```r
install.packages('rmatio')
```

To install the development version of RSurveillance, install the `devtools` package from CRAN and run
the following in R:

```r
library(devtools)
install_github("evansergeant/RSurveillance")
```

Usage
-----
`RSurveillance` functions are organised into several broad areas of surveillance, namely: 
* representative freedom surveys,
* freedom methods for imperfect tests and finite populations,
* risk-based freedom surveys, 
* probability of freedom estimation,  
* prevalence estimation,
* combined testing and
* pooled testing for freedom. 

Within these areas functions can be further grouped according to purpose (depending on surveillance area/purpose), such as sample size calculation, population sensitivity estimation, prevalence estimation and background functions. Specific functions are summarised below according to these categories and additional information and examples are available in R using the `help()` and `example()` functions.

###1. Representative freedom surveys
####1.1. Population sensitivity estimation
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
**Population sensitivity**  
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

####1.2. Sample size estimation
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
**Freedom sample size**  
Calculates sample size for demonstrating freedom or detecting disease using the appropriate method, depending on whether or not N provided (hypergeometric if N provided, binomial otherwise), assuming imperfect test sensitivity, perfect test specificity and representative sampling.  

*Usage*  
`n.freedom(N=NA, sep=0.95, pstar,se=1)`  

#####n.2stage()
**2-stage freedom sample size**  
Calculates sample sizes for a 2-stage representative survey (sampling of clusters and units within clusters) for disease freedom or detection, assuming imperfect test sensitivity, perfect test specificity and representative sampling.  

*Usage*  
`n.2stage(H=NA, N=NA, sep.sys=0.95, sep.c, pstar.c, pstar.u, se=1)`  

####1.3. Miscellaneous functions
#####pstar.calc()
**Design prevalence back-calculation**  
Calculates design prevalence required for given sample size and desired population-level sensitivity, assuming imperfect test sensitivity, perfect test specificity and representative sampling.

*Usage*  
`pstar.calc(N=NA, n, sep, se)`  

###2. Freedom methods for imperfect specificity and finite populations (FreeCalc)
####2.1. Population sensitivity estimation
#####sep.freecalc()
**FreeCalc population sensitivity  for imperfect test**  
Calculates population sensitivity for a finite population and allowing for imperfect test sensitivity and specificity, using Freecalc method.  

*Usage*  
`sep.freecalc(N,n,c=1,se,sp=1,pstar)`  

#####sep.hp()
**Hypergeometric (HerdPlus) population sensitivity for imperfect test**  
Calculates population sensitivity for a finite population and allowing for imperfect test sensitivity and specificity, using Hypergeometric distribution.  

*Usage*  
`sep.hp(N,n,c=1,se,sp=1,pstar)`  

#####sep.binom.imperfect()
**Binomial population sensitivity for imperfect test**  
Calculates population sensitivity for a large or unknown population and allowing for imperfect test sensitivity and specificity, using Binomial distribution an allowing for a variable cut-point number of positives to classify as positive.  

*Usage*  
`sep.binom.imperfect(n, c=1, se, sp=1, pstar)`  

####2.2. Population specificity estimation
#####sph.binom()
**Binomial population specificity for imperfect test **  
Calculates population specificity for a large or unknown population, using the Binomial distribution and adjusting for cut-point number of positives.  

*Usage*  
`sph.binom(n, c=1, sp)`  

#####sph.hp()
**Hypergeometric population specificity calculation**  
Calculates population specificity for a finite population and imperfect test, using Hypergeometric distribution.  

*Usage*  
`sph.hp(N,n,c=1,sp)`  

####2.3. Sample size estimation
#####n.freecalc()
**Freecalc sample size for a finite population and specified cut-point number of positives**  
Calculates sample size required for a specified population sensitivity, for a given population size, cut-point number of positives and other parameters, using Freecalc algorithm. All paramaters must be scalars.  

*Usage*  
`n.freecalc(N,sep=0.95,c=1,se,sp=1,pstar, minSpH=0.95)`  

#####n.hp()
**Hypergeometric (HerdPlus) sample size for finite population and specified cut-point number of positives**  
Calculates sample size to achieve specified population sensitivity with population specificity >= specified minimum value, for given population size, cut-point number of positives and other parameters, all paramaters must be scalars. 

*Usage*  
`n.hp(N,sep=0.95,c=1,se,sp=1,pstar, minSpH=0.95)`  

#####n.c.freecalc()
**Freecalc optimum sample size and cut-point number of positives**  
Calculates optimum sample size and cut-point number of positives to achieve specified population sensitivity, for given population size and other parameters, using freecalc algorithm, all paramaters must be scalars.  

*Usage*  
`n.c.freecalc(N,sep=0.95,c=1,se,sp=1,pstar, minSpH=0.95)`  

#####n.c.hp()
**Hypergeometric (HerdPlus) optimum sample size and cut-point number of positives**  
Calculates optimum sample size and cut-point positives to achieve specified population sensitivity, for given population size and other parameters, all paramaters must be scalars.  

*Usage*  
`n.c.hpn(N,sep=0.95,c=1,se,sp=1,pstar, minSpH=0.95)`  

###3. Risk-based freedom surveys 
####3.1. Population sensitivity estimation
#####sep.rb.bin()
**Binomial risk-based population sensitivity**  
Calculates risk-based population sensitivity with a single risk factor, using binomial method (assumes a large population), allows for unit sensitivity to vary among risk strata.  

*Usage*  
`sep.rb.bin(pstar, rr, ppr, n, se)`  

#####sep.rb.hypergeo()
**Hypergeometric risk-based population sensitivity**  
Calculates risk-based population sensitivity with a single risk factor, using the hypergeometric method (assuming a finite and known population size), allows for unit sensitivity to vary among risk strata.  

*Usage*  
`sep.rb.hypergeo(pstar, rr, N, n, se)`  

#####sep.rb.bin.varse()
**Binomial risk-based population sensitivity for varying unit sensitivity**  
Calculates population sensitivity for a single risk factor and varying unit sensitivity using binomial method (assumes large population).  

*Usage*  
`sep.rb.bin.varse(pstar, rr, ppr, df)`  

#####sep.rb.hypergeo.varse()
**Hypergeometric risk-based population sensitivity for varying unit sensitivity**  
Calculates population sensitivity for a single risk factor and varying unit sensitivity using hypergeometric approximation method (assumes known population size).  

*Usage*  
`sep.rb.hypergeo.varse<- function(pstar, rr, N, df)`  

#####sep.rb2.bin()
**Binomial risk-based population sensitivity for 2 risk factors**  
Calculates risk-based population sensitivity for two risk factors, using binomial method (assumes a large population).  

*Usage*  
`sep.rb2.binom(pstar, rr1, ppr1, rr2, ppr2, n, se)`  

#####sep.rb2.hypergeo()
**Hypergeometric risk-based population sensitivity for 2 risk factors**  
Calculates risk-based population sensitivity for two risk factors, using hypergeometric approximation method (assumes a known population size).

*Usage*  
`sep.rb2.hypergeo(pstar, rr1, rr2, N, n, se)`  

#####sse.rb.2stage()
**Two-stage risk-based system sensitivity**  
Calculates system sensitivity for 2 stage risk-based sampling, llowing for a single risk factor at each stage and using either binomial or hypergeometric approxiation.  

*Usage*  
`sse.rb.2stage(C=NA, pstar.c, pstar.u, rr.c, ppr.c, rr.u, ppr.u, N=NA, n, rg, se)`  

#####sse.combined()
**System sensitivity by combining multiple surveillance components**  
Calculates overall system sensitivity for multiple components, accounting for lack of independence  (overlap) between components.  

*Usage*  
`sse.combined(C = NA, pstar.c, rr, ppr, sep)`  


####3.2. Sample size estimation
#####n.rb()
**Risk-based sample size**  
Calculates sample size for risk-based sampling for a single risk factor and using binomial method.  

*Usage*  
`n.rb(pstar, rr, ppr, spr, se, sep)`  

#####n.rb.varse()
**Risk-based sample size for varying unit sensitivity**  
Calculates sample size for risk-based sampling for a single risk factor and varying unit sensitivity, using binomial method.  

*Usage*  
`n.rb.varse(pstar, rr, ppr, spr, se, spr.rg, sep)`  


####3.3. Miscellaneous functions
#####adj.risk()
**Adjusted risk**  
Calculates adjusted risk for given relative risk and population proportions. This is an intermediate calculation in the calculation of effective probability of infection for risk-based surveillance activities. Used by `epi.calc()`, `sep.rb2.binom()` and `sep.rb2.hypergeo()` functions.  

*Usage*  
`adj.risk(rr, ppr)`

#####epi.calc()
**Effective probability of infection (EPI)**  
Calculates effective probability of infection (EPI; adjusted design prevalence) for each risk group for risk-based surveillance activities. Uses `adj.risk` (this package) function for calculations.     

*Usage*  
`epi.calc(pstar, rr, ppr)`  


###4. Probability of freedom estimation
####4.1. Probability of freedom
#####pfree.1()
**Probability of freedom for single time period**  
Calculates the posterior probability (confidence) of disease freedom (negative predictive value) for a single time period.  

*Usage*  
`pfree.1(sep, p.intro, prior=0.5)`  

#####pfree.calc()
**Probability of freedom over time**  
Calculates the probability (confidence) of disease freedom for given prior, sep and p.intro over 1 or more time periods.  

*Usage*  
`pfree.calc(sep, p.intro, prior=0.5)`  

#####pfree.equ()
**Equilibrium probability of freedom**  
Calculates equilibrium probability of disease freedom and equilibrium prior probability of freedom, after discounting for probability of introduction.  

*Usage*  
`pfree.equ(sep, p.intro)`  

####4.2. Miscellaneous functions
#####n.pfree()
**Sample size to achieve desired (posterior) probability of freedom**  
Calculates the sample size required to achieve a given value for probability of disease freedom.  

*Usage*  
`n.pfree(pfree, prior, p.intro, pstar, se, N = NA)`  

#####sep.pfree()
**Population sensitivity to achieve desired (posterior) probability of freedom**  
Calculates the population sensitivity required to achieve a given value for probability of disease freedom.  

*Usage*  
`sep.pfree(prior, pfree)` 

#####sep.prior()
**Population sensitivity to achieve desired prior probability of freedom**  
Calculates the population sensitivity required to achieve a given value for the prior (discounted) probability of disease.   freedom

*Usage*  
`sep.prior(prior, p.intro)`  

####4.3. Background functions
#####disc.prior()
**Discounted prior probability of freedom**  
Calculates the discounted prior probability of disease freedom, after adjusting for the probability of disease exceeding the design prevalence during the time period of the surveillance data being analysed.  

*Usage*  
`disc.prior(prior, p.intro)`  

###5. Prevalence estimation
####5.1. Apparent Prevalence and CI estimation
#####ap()
**Apparent prevalence**  
Estimates apparent prevalence and confidence limits for given sample size and result, assuming representative sampling. Calls functions `binom.cp()`, `binom.agresti()` and `binom.jeffreys()` (this package) and `binom.approx()` and `binom.wilson()` (`epitools` package) to calculate respective confidence limits.

*Usage*  
`ap(x, n, type = "wilson", conf = 0.95)`  

#####binom.agresti()
**Agresti-Coull confidence limits**  
Calculates Agresti-Coull confidence limits for a simple proportion (apparent prevalence). Used by function `ap()`.   

*Usage*  
`binom.agresti(x, n, conf=0.95)`  

#####binom.jeffreys()
**Jeffreys confidence limits**  
Calculates Jeffreys confidence limits for a simple proportion (apparent prevalence). Used by function `ap()`.  

*Usage*  
`binom.jeffreys(x, n, conf=0.95)`  

#####binom.cp()
**Clopper-Pearson exact confidence limits**  
Calculates Clopper-Pearson exact binomial confidence limits for a simple proportion (apparent prevalence). Used by function `ap()`.  

*Usage*  
`binom.cp(x, n, conf=0.95)`  

#####n.ap()
**Sample size for apparent prevalence**  
Calculates sample size for estimating apparent prevalence (simple proportion).  

*Usage*  
`n.ap(p, precision, conf=0.95)`  


####5.2. True Prevalence and CI estimationn
#####tp()
**True prevalence**  
Estimates true prevalence and confidence limits for given sample size and result, according to specified method. Uses `epi.prev()` function (`epiR` package) to calculate Clopper-Pearson, Wilson, Blaker and Sterne confidence limits and `tp.normal` (this package) to calculate normal approximation confidence limits for the true prevalence estimate.

*Usage*  
`tp(x, n, se, sp, type = "blaker", conf=0.95)`  

#####tp.normal()
**Normal approximation confidence limits for true prevalence**  
Estimates true prevalence and confidence limits for estimates based on normal approximation. Uses function `sd.tp()` (this package) to calculate normal approximation confidence limits for the true prevalence estimate and `binom.wilson()` function (`epitools` package) to calculate Wilson confidence limits for the apparent prevalence estimate.

*Usage*  
`tp.normal(x, n, se, sp, conf=0.95)`  

#####n.tp()
**Sample size for true prevalence**  
 size for estimating true prevalence using normal approximation.  

*Usage*  
`n.tp(p, se, sp, precision, conf=0.95)`

#####sd.tp()
**Standard deviation of true prevalence estimate**  
Calculates the standard deviation of true prevalence estimate assuming se and sp known exactly, used in function `tp.normal()` to calculate normal approximation CI for estimate.  

*Usage*  
`sd.tp(x, n, se, sp)`  


###6. Combining tests
#####se.series()
**Sensitivity of tests in series**  
Calculates the combined sensitivity for multiple tests interpreted in series (assuming independence).  

*Usage*  
`se.series(se)`  

#####se.parallel()
**Sensitivity of tests in parallel**  
Calculates the combined sensitivity for multiple tests interpreted in parallel (assuming independence).  

*Usage*  
`se.parallel(se)`  

#####sp.series()
**Specficity of tests in series**  
Calculates the combined specificity for multiple tests interpreted in series (assuming independence).  

*Usage*  
`sp.series(sp)`  

#####sp.parallel()
**Specificity of tests in parallel**  
Calculates the combined specificity for multiple tests interpreted in parallel (assuming independence).  

*Usage*  
`sp.parallel(sp)`  

###7. Pooled testing for disease freedom
#####sep.pooled()
**Pooled population sensitivity**  
Calculates population sensitivity (sep) and population specificity (spp) assuming pooled sampling and allowing for imperfect sensitivity and specificity of the pooled test.  

*Usage*  
`sep.pooled(r, k, pstar, pse, psp=1)`  

#####n.pooled()
**Sample size for pooled testing for freedom**  
Calculates sample size to achieve desired population-level sensitivity, assuming pooled sampling and allowing for imperfect sensitivity and specificity of the pooled test.  

*Usage*  
`n.pooled(sep, k, pstar, pse, psp=1)`  


