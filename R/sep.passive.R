##' Passive surveillance sensitivity
##' @description Estimates the population sensitivity of a passive surveillance system. 
##' Assumes comprehensive population coverage and samling of representative affected units 
##' from infected clusters 
##' @param step.p vector of detection probabilities for each step in the detection process
##' @param p.inf.u the probability of infection in units sampled, 
##' equivalent to the positive predictive value of clinical signs of disease (for a given prior probability of infection).
##' @param se unit sensitivity of test (proportion), scalar 
##' @param N population size, scalar 
##' @param n number of units tested per cluster reporting suspected disease, scalar 
##' @param pstar.c cluster-level design prevalence (proportion), scalar
##' @return a list of 2 elements, the estimated cluster-level and population-level sensitivities
##' @keywords methods
##' @export
##' @examples
##' # examples for sep.passive
##' sep.passive(c(0.1, 0.2, 0.9, 0.99), 0.98, 0.9, 1000, 5, 0.01)
##' sep.passive(c(0.1, 0.5, 0.95, 0.99), 0.98, 0.9, 1000, 5, 0.01)
sep.passive<- function(step.p, p.inf.u, se, N, n, pstar.c) {
  sep.c<- prod(step.p) * (1 - (1 - p.inf.u * se)^n)
  sep.sys<- 1 - (1 - sep.c)^(pstar.c*N)
  return(list(sep.c, sep.sys))
}

