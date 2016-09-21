##' sample size for 2-stage risk-based surveillance, risk factor at cluster level only
##' @description Calculates sample size required (clusters and units)
##' for a 2-stage risk-based survey with a single risk factor at the 
##' cluster level only.
##' @param rr relative risk values (vector of values, corresponding 
##' to the number of risk strata)
##' @param ppr population proportions corresponding to 
##'   rr values (vector of equal length to rr)
##' @param spr planned surveillance proportions corresponding to 
##'   rr values - the proportions of the total sample to be collected 
##'   from each risk stratum (vector of equal length to rr).
##' @param pstar.c cluster (herd) level design prevalence, scalar,
##'   either proportion or integer
##' @param pstar.u unit (animal) level design prevalence, scalar,
##'   either proportion or integer
##' @param se unit sensitivity of test (proportion), scalar, default = 1
##' @param sep.c desired cluster-level sensitivity (proportion), scalar, default = 0.95
##' @param sep.sys desired population-level sensitivity (proportion), scalar, default = 0.95
##' @return A list of seven elements: 1) a vector (of the same length as rr) of the 
##' numbers of clusters to sample from each risk stratum, 2) the total number of clusters
##' to be sampled, 3) a vector of EPI values for each risk stratum, 
##' 4) a vector of adjusted risk values for each risk stratum,
##' 5) the number of untis to be sampled per cluster
##' 6) a vector of the total numbers of units to be sampled for each risk stratum
##' 7) the overall total number of units to be sampled
##' @keywords methods
##' @export
##' @examples 
##' rr<- c(5,3,1)
##' ppr<- c(0.1, 0.2, 0.7)
##' spr<- c(0.4, 0.4, 0.2)
##' n.rb.2stage.1(rr, ppr, spr, pstar.c=0.01, pstar.u=0.1, se =0.9, sep.c=0.8, sep.sys=0.95) 
##' n.rb.2stage.1(c(3,1), c(0.2,0.8), c(0.7,0.3),0.05, 0.1, 0.9, 0.95, 0.99)
n.rb.2stage.1<- function(rr, ppr, spr,pstar.c, pstar.u, se=1, sep.c=0.95, sep.sys=0.95) {
  n.u<- n.binom(sep.c, pstar.u, se)
  n<- n.rb(pstar.c, rr, ppr, spr, sep.c, sep.sys)
  n$units<- n.u
  n$risk.units<- n$n*n.u
  n$total.units<- n$total*n.u
  return(n)
}


##' Sample size for 2-stage risk-based surveillance, allowing for risk factors at either or both cluster and unit level
##' @description Calculates sample size required (clusters and units)
##' for a 2-stage risk-based survey with risk factors at either 
##' cluster level or unit level, or both.
##' @param rr.c relative risk values at the cluster level (vector of values, corresponding 
##' to the number of risk strata)
##' @param ppr.c population proportions at the cluster level, corresponding to 
##'   rr.c values (vector of equal length to rr.c)
##' @param spr.c planned surveillance proportions at the cluster level, corresponding to 
##'   rr.c values - the proportions of the total sample to be collected 
##'   from each risk stratum (vector of equal length to rr.c).
##' @param pstar.c cluster (herd) level design prevalence, scalar,
##'   either proportion or integer
##' @param rr.u relative risk values at the unit level (vector of values, corresponding 
##' to the number of risk strata)
##' @param ppr.u population proportions at the unit level, corresponding to 
##'   rr.u values (vector of equal length to rr.u)
##' @param spr.u planned surveillance proportions at the unit level, corresponding to 
##'   rr.u values - the proportions of the total sample to be collected 
##'   from each risk stratum (vector of equal length to rr.u).
##' @param pstar.u unit (animal) level design prevalence, scalar,
##'   either proportion or integer
##' @param se unit sensitivity of test (proportion), scalar, default = 1
##' @param sep.c desired cluster-level sensitivity (proportion), scalar, default = 0.95
##' @param sep.sys desired population-level sensitivity (proportion), scalar, default = 0.95
##' @return A list of cluster and unit level results number of clusters/units to 
##' sample per risk stratum, the total number of clusters or units per cluster to be sampled and
##' vectors of EPI and adjusted risk values for each risk stratum.
##' @keywords methods
##' @export
##' @examples 
##' rr.c<- c(5,3,1)
##' ppr.c<- c(0.1, 0.2, 0.7)
##' spr.c<- c(0.4, 0.4, 0.2)
##' rr.u<- c(4,1)
##' ppr.u<- c(0.1, 0.9)
##' spr.u<- c(1, 0)
##' n.rb.2stage.2(rr.c, ppr.c, spr.c, pstar.c=0.02, rr.u, ppr.u, 
##'   spr.u, 0.1, se=0.9, sep.c=0.5, sep.sys=0.95) 
##' n.rb.2stage.2(c(3,1), c(0.2,0.8), c(0.7,0.3), pstar.c=0.05, 
##'   pstar.u=0.1, se=0.9, sep.c=0.95, sep.sys=0.99)
n.rb.2stage.2<- function(rr.c, ppr.c, spr.c, pstar.c, 
                         rr.u=1, ppr.u=1, spr.u=1, pstar.u, 
                         se=1, sep.c=0.95, sep.sys=0.95) {
  n.u<- n.rb(pstar.u, rr.u, ppr.u, spr.u, se, sep.c)
  n.c<- n.rb(pstar.c, rr.c, ppr.c, spr.c, sep.c, sep.sys)
  n<- list(clusters = n.c, units = n.u)
  return(n)
}


