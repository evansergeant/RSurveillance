##' Adjusted risk for simulation models
##' @description Calculates adjusted risk for given 
##'   relative risk and population proportions. This is an intermediate calculation
##'   in the calculation of effective probability of infection for risk-based 
##'   surveillance activities. This function is similar to Adj.risk, except is 
##'   adapted for use with multiple simulations instead of single RR values.
##' @param rr relative risk values (matrix of values, columns corresponding 
##' to the number of risk strata, rows corresponding to number of iterations for simulation )
##' @param ppr population proportions corresponding to 
##'   rr values (vector of equal length to columns in rr)
##' @return matrix of adjusted risk values (in order corresponding to rr)
##' @keywords methods
##' @export
##' @examples 
##' # examples for adj.risk.sim
##' its<- 10
##' risk.cat<- 3
##' rr<- matrix(0, nrow=its, ncol=risk.cat)
##' rr[,1]<- mc2d::rpert(its, 5,10,20)
##' rr[,2]<- mc2d::rpert(its, 2,3,5)
##' rr[,3]<- 1
##' ppr<- c(0.05, 0.2, 0.75)
##' adj.risk.sim(rr, ppr)
##' adj.risk.sim(matrix(c(5, 3, 1), nrow=1), matrix(c(0.1, 0.1, 0.8), nrow=1))
adj.risk.sim<- function(rr, ppr) {
  tmp<- rr
  ar<- rr
  for (r in 1:ncol(rr)) tmp[,r]<- rr[,r]*ppr[r]
  sum.prod<- apply(tmp, FUN = sum, MARGIN = 1)
  for (r in 1:ncol(rr)) ar[,r]<- rr[,r]/sum.prod
  return(ar)
}


  