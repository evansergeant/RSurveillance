#############################################################
# Freedom functions for imperfect tests
##############################################################
# sep.freecalc - Calculate herd/population sensitivitiy for imperfect tests and known population size using Freecalc method (Cameron and Baldock 1999)
#       calculates for specified population size, sample size, cut-point number of positives, se and sp and design prevalence
# sep.hp - as for sep.freecalc except uses "HerdPlus" algorithm, developed by Matthias Greiner but so far unpublished (I believe)
# sep.binom.imperfect - as above but uses binomial approach assuming large population
# sph.hp - herd specificity for variable cut-point and finite popualtion size using herdplus approach
# sph.binom - herd specificity for variable cut-point assuming binomial sampling
# n.hp - sample size calculation for specified cut-point value using herdplus algorithm iterative process to find optimum sample size
# n.c.hp - as above but finds the optimium cut-point rather than using a specified value
# n.freecalc - as for n.hp but uses freecalc algorithm
# n.c.freecalc - as for n.c.hp but uses freecalc algorithm
# note freecalc and hp methods do give slightly different answers for both sep and for n


##' FreeCalc population sensitivity  for imperfect test
##' @description Calculates population sensitivity for a finite population 
##'   and allowing for imperfect test sensitivity and specificity, using Freecalc method
##' @param N population size (scalar)
##' @param n sample size (scalar)
##' @param c The cut-point number of positives to classify a cluster
##'   as positive, default=1, if positives < c result is negative, 
##'   >= c is positive (scalar)
##' @param se test unit sensitivity (scalar)
##' @param sp test unit specificity, default=1 (scalar)
##' @param pstar design prevalence as a proportion - assumed or target prevalence for 
##'   detection of disease in the population (scalar)
##' @return population-level sensitivity
##' @keywords methods
##' @export
##' @examples 
##' # examples of sep.freecalc
##' sep.freecalc(150, 30, 2, 0.9, 0.98, 0.1)
##' sep.freecalc(150, 30, 1, 0.9, 0.98, 0.1)
##'
sep.freecalc <- function(N,n,c=1,se,sp=1,pstar) {           # code checked by Evan
  d <- round(max(1,N*pstar),0)     # try ceiling or floor
  maxy <- min(d,n)              # y cannot be greater than d or n
  prod <- 0
  for (x in 0:(c-1)) {        # note: at cutpoint c, the herd is positive
    for (y in 0:maxy) {
      if ((d>=y)*((N-d)>=(n-y))) {
        minxy  <- min(x,y)
        fact <- 0
        for (j in 0:minxy) {
          if ((y>=j)*((n-y)>=(x-j))*(d>=y)*(N>=n)) {            # modified to avaoid illegal ranges
            fact <- fact + choose(y,j)*se^j*(1-se)^(y-j)*choose(n-y,x-j)*(1-sp)^(x-j)*sp^(n-x-y+j)
          } else {fact <- 0}
        }
      } else { fact <- 0}
      newprod <- stats::dhyper(y,d,N-d,n,log=F)*fact
      prod <- prod + newprod
    }
  }
  SeHCam <- 1-prod
  return(SeHCam)
}



##' Hypergeometric (HerdPlus) population sensitivity for imperfect test
##' @description Calculates population sensitivity for a finite population 
##'   and allowing for imperfect test sensitivity and specificity, 
##'   using Hypergeometric distribution
##' @param N population size (scalar or vector of same length as n)
##' @param n sample size (scalar or vector)
##' @param c The cut-point number of positives to classify a cluster
##'   as positive, default=1, if positives < c result is negative, 
##'   >= c is positive (scalar)
##' @param se test unit sensitivity (scalar)
##' @param sp test unit specificity, default=1 (scalar)
##' @param pstar design prevalence as a proportion  (scalar)
##' @return a vector of population-level sensitivities
##' @keywords methods
##' @export
##' @examples 
##' # examples of sep.hp
##' sep.hp(150, 1:5*10, 2, 0.9, 0.98, 0.1)
##' sep.hp(150, 30, 2, 0.9, 0.98, 15)
##' sep.hp(150, 30, 1, 0.9, 0.98, 15)
##' sep.hp(150, 30, 1, 0.9, 0.98, 0.1)
sep.hp <- function(N,n,c=1,se,sp=1,pstar) {
  pa.int<- ifelse(pstar > 0 & pstar < 1, F, T)
  vals<- length(N)
  EX <- ifelse(rep(pa.int, vals), pmin(rep(max(1,pstar), vals), N), pmax(1,N*pstar))             # E(number of infected animals in the herd) herdacc doesn't use 1 as minimum
  EY <- EX*se+(N-EX)*(1-sp)     # E(number of test pos animals in the herd)
  Y  <- floor(EY)
  m  <- EY-Y                   # modulus(EY,1)
  SeH <-m*stats::phyper(c-1,Y+1,N-Y-1,n,lower.tail=F)+(1-m)*stats::phyper(c-1,Y,N-Y,n,lower.tail=F)
  return(SeH)
}


##' Binomial population sensitivity for imperfect test
##' @description Calculates population sensitivity for a large or unknown population 
##'   and allowing for imperfect test sensitivity and specificity, 
##'   using Binomial distribution an allowing for a variable cut-point number of 
##'   positives to classify as positive
##' @param n sample size (scalar or vector)
##' @param c The cut-point number of positives to classify a cluster
##'   as positive, default=1, if positives < c result is negative, 
##'   >= c is positive (scalar or vector of same length as n)
##' @param se test unit sensitivity (scalar or vector of same length as n)
##' @param sp test unit specificity, default=1 (scalar or vector of same length as n)
##' @param pstar design prevalence as a proportion (scalar or vector of same length as n)
##' @return a vector of population-level sensitivities
##' @keywords methods
##' @export
##' @examples 
##' # examples for sep.imperfect.binom
##' sep.binom.imperfect(1:10*5, 2, 0.95, 0.98, 0.1)
##' sep.binom.imperfect(50, 1:5, 0.95, 0.98, 0.1)
##' sep.binom.imperfect(30, 2, 0.9, 0.98, 0.1)
##' sep.binom.imperfect(30, 1, 0.9, 0.98, 0.1)
sep.binom.imperfect<- function(n, c=1, se, sp=1, pstar) {
  P.Pos<- pstar * se + (1 - pstar)*(1 - sp)
  sep<- 1 - stats::pbinom(c-1, n, P.Pos)
  return(sep)
}


##' Hypergeometric population specificity calculation
##' @description Calculates population specificity for a finite population 
##' and imperfect test, using Hypergeometric distribution
##' @param N population size (scalar or vector of same length as n)
##' @param n sample size (scalar or vector)
##' @param c The cut-point number of positives to classify a cluster
##'   as positive, default=1, if positives < c result is negative, 
##'   >= c is positive (scalar or vector of same length as n)
##' @param sp test unit specificity (scalar or vector of same length as n)
##' @return a vector of population-level specificities
##' @keywords methods
##' @export
##' @examples 
##' # examples of sph.hp
##' sph.hp(150, 30, 2, 0.98)
##' sph.hp(150, 30, 1, 0.98)
##' sph.hp(150, 1:5*10, 2, 0.98)
##' sph.hp(500, 30, 2, 95:100/100)
sph.hp <- function(N,n,c=1,sp) {
  EY <- N*(1-sp)                # E(number of test pos animals in the herd)
  Y  <- floor(EY)
  m  <- EY-Y                   # modulus(EY,1)
  SpH <-m*stats::phyper(c-1,Y+1,N-Y-1,n)+(1-m)*stats::phyper(c-1,Y,N-Y,n)
  return(SpH)
}


##' Binomial population specificity for imperfect test 
##' @description Calculates population specificity for a large or unknown population, 
##'   using the Binomial distribution and adjusting for cut-point number of positives
##' @param n sample size (scalar or vector)
##' @param c The cut-point number of positives to classify a cluster
##'   as positive, default=1, if positives < c result is negative, 
##'   >= c is positive (scalar or vector of same length as n)
##' @param sp test unit specificity (scalar or vector of same length as n)
##' @return a vector of population-level specificities
##' @keywords methods
##' @export
##' @examples 
##' # examples for sph.imperfect.sp
##' sph.binom(30, 2, 0.98)
##' sph.binom(30, 1, 0.98)
##' sph.binom(1:5*10, 2, 0.98)
##' sph.binom(100, 1:5, 0.98)
##' sph.binom(100, 3, 95:100/100)
##' sph.binom(c(5, 10, 15, 20, 30, 50, 100, 200), 2, 0.98)
sph.binom<- function(n, c=1, sp) {
  sph<- stats::pbinom(c-1, n, 1 - sp)
  return(sph)
}


##' Hypergeometric (HerdPlus) sample size for finite population and specified cut-point number of positives
##' @description Calculates sample size to achieve specified population sensitivity with
##'   population specificity >= specified minimum value,
##'   for given population size, cut-point number of positives and other parameters, 
##'   all paramaters must be scalars
##' @param N population size
##' @param sep target population sensitivity
##' @param c The cut-point number of positives to classify a cluster
##'   as positive, default=1, if positives < c result is negative, >= c is positive
##' @param se test unit sensitivity
##' @param sp test unit specificity, default=1
##' @param pstar design prevalence as a proportion or integer (number of infected units)
##' @param minSpH minimium desired population specificity
##' @return A list of 2 elements, a dataframe with 1 row and six columns for
##'   the recommended sample size and corresponding values for population sensitivity (SeP),
##'   population specificity (SpP), N, c and pstar and a dataframe of n rows 
##'   with SeP and SpP values for each value of n up to the recommended value.
##'   Returns sample size for maximum achievable sep if it is not possible to 
##'   achieve target sep AND SpP>= minSpH.
##' @keywords methods
##' @export
##' @examples 
##' # examples for n.hp
##' n.hp(65,0.95,c=1,se=0.95,sp=0.99,pstar=0.05, minSpH=0.9)[[1]]
##' n.hp(65,0.95,c=2,se=0.95,sp=0.99,pstar=0.05, minSpH=0.9)
n.hp<- function(N,sep=0.95,c=1,se,sp=1,pstar, minSpH=0.95) {
  pa.int<- ifelse(pstar > 0 & pstar < 1, F, T)
  SeH <- 0
  SpH <- 0
  lastSeH <- -1
  done<- F
  for (n in 1:N) {
    SeH[n] <- sep.hp(N,n,c,se,sp,pstar)
    SpH[n] <-sph.binom(n, c, sp)
    newSeH <- (SpH[n] >= minSpH)*SeH[n]
    if ((newSeH>lastSeH)*!done) {
      lastSeH <- newSeH
      lastSpH <- SpH[n]
      lastn <- n
      if (newSeH>=sep) done<- TRUE
    }
  }
  results<- data.frame(n=lastn, SeP=lastSeH, SpP=lastSpH, N, c=c, pstar=pstar)
  return(list("Suggested n"=results, "Detailed results"=data.frame(n=1:n, SeP=SeH, SpP=SpH)))
}


##' Hypergeometric (HerdPlus) optimum sample size and cut-point number 
##' of positives
##' @description Calculates optimum sample size and cut-point positives 
##'   to achieve specified population sensitivity, for 
##'   given population size and other parameters, all paramaters must be scalars
##' @param N population size
##' @param sep target population sensitivity
##' @param c The maximum allowed cut-point number of positives to classify a cluster
##'   as positive, default=1, if positives < c result is negative, >= c is positive
##' @param se test unit sensitivity
##' @param sp test unit specificity, default=1
##' @param pstar design prevalence as a proportion or integer (number of infected units)
##' @param minSpH minimium desired population specificity
##' @return a list of 3 elements, a dataframe with 1 row and six columns for
##'   the recommended sample size and corresponding values for population sensitivity (SeP),
##'   population specificity (SpP), N, c and pstar, a vector of SeP values
##'   and a vector of SpP values, for n = 1:N
##' @keywords methods
##' @export
##' @examples 
##' # examples for n.c.hp
##' n.c.hp(65,0.95,c=5,se=0.95,sp=0.99,pstar=0.05, minSpH=0.9)[[1]]
##' tmp<- n.c.hp(120,0.95,c=5,se=0.9,sp=0.99,pstar=0.1, minSpH=0.9)
n.c.hp<- function(N,sep=0.95,c=1,se,sp=1,pstar, minSpH=0.95) {
  pa.int<- ifelse(pstar > 0 & pstar < 1, F, T)
  SeH <- matrix(nrow=N, ncol=c)
  SpH <- SeH
  lastSeH <- -1
  done<- F
  for (n in 1:N) {
    for (x in c:1) {
      SeH[n, x] <- sep.hp(N,n,x,se,sp,pstar)
      SpH[n, x] <-sph.binom(n, x, sp)
      newSeH <- (SpH[n,x] >= minSpH)*SeH[n,x]
      if ((newSeH>lastSeH)*!done) {
        lastSeH <- newSeH
        lastSpH <- SpH[n,x]
        lastn <- n
        lastc<- x
        if (newSeH>=sep) done<- TRUE
      }
    }
  }
  results<- data.frame(n=lastn, SeP=lastSeH, SpP=lastSpH, N, c=lastc, pstar=pstar)
  return(list("Suggested n"=results, SeP=SeH,SpP=SpH))
}


##' Freecalc sample size for a finite population and specified cut-point number of positives
##' @description Calculates sample size required for a specified population sensitivity,
##' for a given population size, cut-point number of positives and other parameters, 
##' using Freecalc algorithm. All paramaters must be scalars
##' @param N population size
##' @param sep target population sensitivity
##' @param c The cut-point number of positives to classify a cluster
##'   as positive, default=1, if positives < c result is negative, >= c is positive
##' @param se test unit sensitivity
##' @param sp test unit specificity, default=1
##' @param pstar design prevalence as a proportion or integer (number of infected units)
##' @param minSpH minimium desired population specificity
##' @return a list of 2 elements, a dataframe with 1 row and six columns for
##'   the recommended sample size and corresponding values for population sensitivity (SeP),
##'   population specificity (SpP), N, c and pstar
##'   and a dataframe of n rows with SeP and SpP values for each value of n up to the recommended value
##' @keywords methods
##' @export
##' @examples 
##' # examples for n.freecalc
##' n.freecalc(65,0.95,sp=0.99,pstar=0.05, type1 = 0.05, type2 = 0.05)[[1]]
##' n.freecalc(N=100, se=0.95,sp=0.99,pstar=0.05, type1 = 0.05, type2 = 0.05)[[1]]
##' n.freecalc(N=200, se=0.95,sp=0.99,pstar=0.05, type1 = 0.05, type2 = 0.1)
n.freecalc<- function(N, se, sp, pstar, type1 = 0.05, type2 = 0.05, pop.threshold = 10000, maxSS = 3200) {
  method.lst<- c("Modified hypergeometric exact", "Simple Binomial")  
  digits<- 4
  pa.int<- ifelse(pstar > 0 & pstar < 1, F, T)  #
  prev<- ifelse(pa.int, pstar/N, pstar)
  dis<- ifelse(pa.int, pstar, max(1, round(pstar*N, 0)))
  method<- ifelse(N > pop.threshold, 2, method)
    N1<- min(N, maxSS)
    brks<- c(50, 100, 1000, 5000, 10000, 100000, Inf)
    steps<- c(5, 10, 50, 100, 200, 500)
    step<- steps[which(N1 < brks)[1]]
    ss<- seq(0, N1, by = step)
    ss[1]<- 1
    if (length(ss) == 1) ss[2]<- N1
    cp<- 0
    SpH<- 0
    SeH<- 0
    P1<- 0
    success<- F
    for (s in 1:length(ss)) {
      c.p<- 0
      Hspec<- 0
      while (Hspec < 1-type2) {
        c.p<- c.p+1                     # probability of observed result from diseas-free popn
        Hspec<- sph.binom(ss[s], c.p, sp)
      }
      cp[s]<- c.p
      SpH[s]<- Hspec
      if (identical(method, 2) || N > pop.threshold) {         # simple binomial
        P1[s]<- 1-sep.binom.imperfect(ss[s], cp[s], se, sp, prev)
      } else {                                               # modified hypergeometric exact
        P1[s]<- 1-sep.freecalc(N,ss[s], cp[s],se,sp,prev)
      }
      SeH[s]<- 1-P1[s]
      cp[s]<- cp[s]-1
      if (P1[s] <= type1) {       
          success<- T
          n1<- ss[s]              
          break
      }      
    }  # end of s loop
    if (success) {
        ss[(s+1):(s+step)]<- (ss[s-1]+1):(ss[s-1]+step)
        for (x in 1:step) {
            c.p<- 0
            Hspec<- 0
            while (Hspec < 1-type2) {
              c.p<- c.p+1                     # probability of observed result from diseas-free popn
              Hspec<- sph.binom(ss[s+x], c.p, sp)
            }
            cp[s+x]<- c.p
            SpH[s+x]<- Hspec
            if (identical(method, 2) || N > pop.threshold) {         # simple binomial
              P1[s+x]<- 1-sep.binom.imperfect(ss[s+x], cp[s+x], se, sp, prev)
            } else {                                               # modified hypergeometric exact
              P1[s+x]<- 1-sep.freecalc(N,ss[s+x], cp[s+x],se,sp,prev)
            }
            SeH[s+x]<- 1-P1[s+x]
            cp[s+x]<- cp[s+x]-1
            if (P1[s+x] <= type1) {       
                success<- T
                n1<- ss[s+x]
                break
            }
        } # end of x loop
        res<- numeric(6)
        names(res)<- c("Required sample size:", "Cut-point number of positives:", 
        "Type I error:", "Type II error:", "Population-level sensitivity:", 
        "Population-level specificity:")
        res[1]<- n1
        res[2]<- cp[s+x]
        res[3]<- round(1-SeH[s+x], digits)
        res[4]<- round(1-SpH[s+x], digits)
        res[5]<- round(SeH[s+x], digits)
        res[6]<- round(SpH[s+x], digits)
        result<- list(res, "Interpretation" = paste("If a random sample of <b>", n1, "</b> units is taken from a population of <b>",
                        N, "</b> and <b>", cp[s+x], "</b> or fewer reactors are found, the probability that the population is diseased at a prevalence of <b>",
                        prev, "</b> is <b>", round(1-SeH[s+x], digits), ".", sep = ""),
                    "Method" = method.lst[method])
    } else {
        result<- array("", dim = c(1, 1))
        result[1,1]<- ifelse(identical(N1, N), "Unable to achieve required accuracy by sampling every unit",
                          paste("Unable to achieve required accuracy by sampling every unit up to the maximum sample size of <b>", maxSS, "</b>.", sep = ""))
    } # end of if/else
    return(result)
} # end of n.freecalc function
