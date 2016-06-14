

###########
# sample size for 2-stage risk-based surveillance, risk factor at cluster level only
#############

n.rb.2stage.1<- function(rr, ppr, spr,pstar.c, pstar.u, se, sep.c, sep.sys) {
  n.u<- n.binom(sep.c, pstar.u, se)
  n<- n.rb(pstar.c, rr, ppr, spr, sep.c, sep.sys)
  n$units<- n.u
  n$risk.units<- n$n*n.u
  n$total.units<- n$total*n.u
  return(n)
}

rr<- c(5,3,1)
ppr<- c(0.1, 0.2, 0.7)
spr<- c(0.4, 0.4, 0.2)
pstar.c<- 0.01
pstar.u<- 0.01
se<- 0.9
sep.c<- 0.8
sep.sys<- 0.95

ss<- n.rb.2stage.1(rr, ppr, spr,pstar.c, pstar.u, se, sep.c, sep.sys) 
ss
ss<- n.rb.2stage.1(c(3,1), c(0.2,0.8), c(0.7,0.3),0.05, 0.1, 0.9, 0.95, 0.99)
ss

# sample size allowing for risk factors at either or both cluster and unit level
n.rb.2stage.2<- function(rr.c, ppr.c, spr.c, pstar.c, pstar.u, se, sep.c, sep.sys, rr.u=1, ppr.u=1, spr.u=1) {
  n.u<- n.rb(pstar.u, rr.u, ppr.u, spr.u, se, sep.c)
  n.c<- n.rb(pstar.c, rr.c, ppr.c, spr.c, sep.c, sep.sys)
  n<- list(clusters = n.c, units = n.u)
  return(n)
}

rr.c<- c(5,3,1)
ppr.c<- c(0.1, 0.2, 0.7)
spr.c<- c(0.4, 0.4, 0.2)
pstar.c<- 0.01
rr.u<- c(4,1)
ppr.u<- c(0.1, 0.9)
spr.u<- c(1, 0)
pstar.u<- 0.1
se<- 0.9
sep.c<- 0.8
sep.sys<- 0.95

ss<- n.rb.2stage.2(rr.c, ppr.c, spr.c, pstar.c, pstar.u, se, sep.c, sep.sys, rr.u, ppr.u, spr.u) 
ss
ss<- n.rb.2stage.2(c(3,1), c(0.2,0.8), c(0.7,0.3),0.05, 0.1, 0.9, 0.95, 0.99)
ss

# passive surveillance sensitivity
sep.passive<- function(step.p, p.inf.u, se, N, n, pstar) {
  if (is.vector(step.p)) {
    sep.c<- prod(step.p) * (1 - (1 - p.inf * se)^n)
    sep.sys<- 1 - (1 - sep.c)^(pstar*N)
  } else {
    
  }
}

step.p<- c(0.1, 0.2, 0.9, 0.99)


