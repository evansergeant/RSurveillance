# inp.dist
##' @description Generate input distributions for a simulation model according to 
##' specified distribution types and  parameters
##' @param its Number of iterations required = number of rows in distribution table
##' @param dist Vector of distribution types to be created. Currently supports 
##' "beta", "pert", "unif" and "fixed".
##' @param labels Vector of labels corresponding to each distribution, vector of same length as dist.
##' @param param matrix of distribution parameters, number of rows equal to length of dist
##' and 3 columns for parameter values. Unused values are NA
##' @return matrix of distribution values, columns = distributions and rows = iterations
##' @keywords methods
##' @export
##' @examples
##' dist<- c("beta", "beta", "pert", "fixed", "unif")
##' labels<- c("beta1", "beta2", "pert", "fixed", "unif")
##' parm<- c(91, 11, NA, 5, 5, NA, 2, 3, 5, 1, NA, NA, 0, 1, NA)
##' parm<- matrix(parm, byrow = T, nrow = 5)
##' inp.dist(100, dist, labels, param)
##' 
inp.dist<- function(its = 1000, dist, labels = NA, param) {
  dist.vals<- array(NA, dim = c(its, length(dist)))
  colnames(dist.vals)<- labels
  dist.types<- c("beta", "pert", "unif", "fixed")
  for (i in 1:length(dist)) {
    if (dist[i] == "beta") {
      dist.vals[,i]<- rbeta(its, param[i,1], param[i,2])
    } else if (dist[i] == "pert") {
      dist.vals[,i]<- mc2d::rpert(its, param[i,1], param[i,2], param[i,3])
    } else if (dist[i] == "unif") {
      dist.vals[,i]<- runif(its, param[i,1], param[i,2])
    } else if (dist[i] == "fixed") {
      dist.vals[,i]<- param[i,1]
    } else {
      dist.vals[,i]<- NA
    }
  }
  return(dist.vals)
}
  
its<- 10
dist<- c("beta", "beta", "pert", "fixed", "unif")
labels<- c("beta1", "beta2", "pert", "fixed", "unif")
parm<- c(91, 11, NA, 5, 5, NA, 2, 3, 5, 1, NA, NA, 0, 1, NA)
parm<- matrix(parm, byrow = T, nrow = 5)
