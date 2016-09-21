##' Two-stage risk-based system sensitivity
##' @description Calculates system sensitivity for 2 stage risk-based 
##'   sampling, llowing for a single risk factor at each stage and
##'   using either binomial or hypergeometric approxiation
##' @param C Population size (number of clusters), NA = unknown (default)
##' @param pstar.c cluster level design prevalence (scalar)
##' @param pstar.u unit level design prevalence (scalar)
##' @param rr.c cluster level relative risks (vector with length 
##'   corresponding to the number of risk strata), 
##'   use rr.c = c(1,1) if risk factor does not apply  
##' @param rr.u unit level relative risks (vector with length 
##'   corresponding to the number of risk strata), 
##'   use rr.u = c(1,1) if risk factor does not apply  
##' @param ppr.c cluster level population proportions for risk 
##'   categories (vector), NA if no cluster level risk factor
##' @param N population size per risk group for each cluster, 
##'   NA or matrix of N for each risk group 
##'   for each cluster, N=NA means cluster sizes not provided
##' @param rg vector of cluster level risk group (index) for each cluster
##' @param n sample size per risk group for each cluster sampled,
##'   matrix, 1 row for each cluster, columns = unit level risk groups
##' @param ppr.u unit level population proportions for each risk group (optional) 
##'   matrix, 1 row for each cluster, columns = unit level risk groups, 
##'   not required if N is provided
##' @param se unit sensitivity for each cluster, scalar or 
##'   vector of values for each cluster, equal in length to n
##' @return list of 2 elements, a scalar of population-level (surveillance system) 
##'   sensitivity and a vector of cluster-level sensitivities
##' @keywords methods
##' @export
##' @examples 
##' # examples for sse.rb.2stage
##' pstar.c<- 0.02
##' pstar.u<- 0.1
##' rr.c<- c(5, 1)
##' ppr.c<- c(0.1, 0.9)
##' rr.u<- c(3, 1)
##' se<- 0.9
##' n<- cbind(rep(10, 50), rep(5, 50))    
##' rg<- c(rep(1, 30), rep(2, 20))
##' ppr.u<- cbind(rep(0.2, 50), rep(0.8, 50))
##' N<- cbind(rep(30, 50), rep(120, 50))
##' C<- 500        
##' sse.rb.2stage(C=NA, pstar.c, pstar.u, rr.c, ppr.c, rr.u, ppr.u, N=NA, n, rg, se) 
##' sse.rb.2stage(C, pstar.c, pstar.u, rr.c, ppr.c, rr.u, ppr.u, N=NA, n, rg, se) 
##' sse.rb.2stage(C=NA, pstar.c, pstar.u, rr.c, ppr.c, rr.u, ppr.u, N, n, rg, se) 
##' sse.rb.2stage(C, pstar.c, pstar.u, rr.c, ppr.c, rr.u, ppr.u, N, n, rg, se) 
sse.rb.2stage<- function(C=NA, pstar.c, pstar.u, rr.c, ppr.c, rr.u, ppr.u, N=NA, n, rg, se) {
  if (length(se) == 1) se<- rep(se, nrow(n))
  sep<- numeric(nrow(n))
  # calculate sep for all clusters
  if (length(N) == 1)  {
    # cluster sizes not provided so use binomial for all clusters
    for (i in 1:nrow(n)) {
      sep[i]<- sep.rb.bin(pstar.u, rr.u, ppr.u[i,], n[i,], se[i])[[1]]
    } 
  } else {
    # cluster sizes provided so use hypergeometric nless NA for specific clusters
    for (i in 1:nrow(n)) {
      if (is.na(N[i,1])) {
        sep[i]<- sep.rb.bin(pstar.u, rr.u, ppr.u[i,], n[i,], se[i])[[1]]
      } else { 
        sep[i]<- sep.rb.hypergeo(pstar.u, rr.u, N[i,], n[i,], se[i])[[1]]
      }  
    }
  } 
  # calculate system sensitivity
  if (is.na(C)) {  
    # Population size unnown, use binomial
    sse<- sep.rb.bin.varse(pstar.c, rr.c, ppr.c, df=cbind(rg, sep, 1))
  } else {
    sse<- sep.rb.hypergeo.varse(pstar.c, rr.c, C*ppr.c, df=cbind(rg, sep, 1))
  }
  return(list("System sensitivity" = sse[[1]], 
              "Cluster sensitivity" = sep))
}


