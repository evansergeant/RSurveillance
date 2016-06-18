##' Passive surveillance sensitivity
##' @description Estimates the population sensitivity of a passive surveillance system. 
##' Assumes comprehensive population coverage and samling of representative affected units 
##' from infected clusters 
##' @param step.p vector or matrix of detection probabilities for each step in the detection process.
##' If a vector each value represents a step probability for a single calculation. If a matrix, columns
##' are step probabilities and rows are simulation iterations.
##' @param p.inf.u the probability of infection in units sampled, 
##' equivalent to the positive predictive value of clinical signs of disease 
##' (for a given prior probability of infection). Either a scalar or vector with length equal 
##' to number of rows in step.p.
##' @param se unit sensitivity of test (proportion). Either a scalar or vector with length equal 
##' to number of rows in step.p.
##' @param N population size. Either a scalar or vector with length equal 
##' to number of rows in step.p
##' @param n number of units tested per cluster reporting suspected disease. Either a scalar or vector with length equal 
##' to number of rows in step.p
##' @param pstar.c cluster-level design prevalence (proportion). Either a scalar or vector with length equal 
##' to number of rows in step.p
##' @return a list of 2 elements, the estimated cluster-level and population-level sensitivities.
##' If step.p is a vector, values are scalars, if step.p is a matrix, values are vectors with 
##' length equal to the number of rows in step.p
##' @keywords methods
##' @export
##' @examples
##' # examples for sep.passive
##' sep.passive(c(0.1, 0.2, 0.9, 0.99), 0.98, 0.9, 1000, 5, 0.01)
##' sep.passive(c(0.1, 0.5, 0.95, 0.99), 0.98, 0.9, 1000, 5, 0.01)
##' step.p<- matrix(runif(30), nrow=10)
##' p.inf.u<- runif(10, 0.98, 0.999)
##' se<- mc2d::rpert(10, 0.9, 0.95, 0.98)
##' sep.passive(step.p, p.inf.u, se, 10000, 10, 0.02)
sep.passive<- function(step.p, p.inf.u, se, N, n, pstar.c) {
  if (is.matrix(step.p)) {
    tmp<- apply(step.p, FUN=prod, MARGIN=1)
  } else {
    tmp<- prod(step.p)   
  }
  sep.c<- tmp * (1 - (1 - p.inf.u * se)^n)
  sep.sys<- 1 - (1 - sep.c)^(pstar.c*N)
  return(list(sep.c, sep.sys))
}



