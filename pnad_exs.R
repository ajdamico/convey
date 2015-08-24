# PNAD
# y - design object post-stratified
# v4621 - household per-capita income

library(survey)
library(RSQLite)

# data frame with variables of interest 
x <- dbGetQuery( db , 'select v4618 , v4617 , pre_wgt , v4609 ,
  v4621 ... from pnad2011' )

# estimate poverty threshold

# computes h from the whole population
htot<- h_fun(x$v4621, weights(y))

# estimate poverty threshold: .6*MED

povthresh <- svyarpt(~v4621, y, order = .50, percent =.6, htot, 
  ncom = nrow(x), comp = TRUE)

povthresh$value

# estimate the rate of poor people: v4621<= povthresh

povrate <- svyarpr(~v4621, y, order = .50, percent =.6, htot,
  ncom = nrow(x), comp = TRUE)

# value
pvrate$value

# note: the poverty threshold used was estimated. 
# so the se of povrate cannot be obtained directly using svyratio!

# use the linearized variable to estimate the se of povrate:

y <- update(y,linpovrate= povrate$lin )

SE(svytotal(~linpovrate, y ))



# estimate the relative median at-risk-of-poverty gap:

# The relative median at-risk-of-poverty gap (RMPG) is given by the
# difference between the median per-capita income of persons below
# the at-risk-of-poverty threshold and the at-risk-of -poverty:

# RMPG = 100* (medp- povthresh)/povthresh, where

# medp= median(pop$v4621[pop$v4621<=povthresh]), (pop - population data)

rmpg<-svyrmpg(~v4621, y, order = .50, percent =.6, htot,
  ncom = nrow(x), comp = TRUE, ARPT= povthresh)
rmpg$value

# get se
y<- update (y, linrmpg= rmpg$lin)
SE(svytotal(~linrmpg, y )) 

# Quintile share ratio (QSR): 

# is the ratio of the total income received by the 20% of the population with the   highest income to that received by the 20% of the population with the lowest income

# population: sum(pop$v4621[pop$v4621> q80])/ sum(pop$v4621[pop$v4621<= q20])


qsr <- svyqsr(~v4621, y, .20, htot, ncom = nrow(x), comp = TRUE, incvec = x$v4621)  
qsr$value
# se

y<- update (y, linqsr= qsr$lin)
SE(svytotal(~linqsr, y )) 

# Gini coeficient
# is the relationship of cumulative shares of the population
# arranged according to the level income, to the cumulative share 
# of  otal income received.

# Population: gini+1 = (2*sum(r*pop$v4621)-sum(pop$v4621))/N*sum(pop$v4621) 
#  r is the vetor of ranks of pop$v4621

gini <- gini(~v4621, y, ncom = nrow(x), ncom = nrow(x), comp=TRUE)

# gini$value

# se
y<- update (y, lingini= gini$lin)
SE(svytotal(~lingini, y )) 

# in the libraries laeken and vardpoor it is used the "equivalized disposable income".
# this variable has to be created in the PNAD.

##########################################
## Using replicated weights.
## gini.
###########################################

pnad.design.boot<-as.svrepdesign(y,type="bootstrap")
# matrix of replicated weights factors in compressed form
fact.peso.comp<-pnad.design.boot$repweights[[1]]

# psu indicator
ind.cong<-ppv1.se.design.jkn$repweights[[2]]
# expanded replicated weights factors
mat.fat.pesos<- fact.peso.comp[ind.cong,]

# replicated weights
# sample.pnad is the design object before post-stratification

mat.rep.pesos<-weights(sample.pnad)*mat.fat.pesos

# has to post-stratify each column of mat.rep.pesos and get mat.rep.pesos.pos


# to calculate only the gini indicator use

ginivalue <- function(incvar, w){
  w <- w[order(incvar)]
  incvar <- incvar[order(incvar)]
  # population size
  N <-  sum(w)
  # sample size
  n <- length(incvar)
  # total income
  T <- sum(incvar*w)
  # cumulative weight
  r <- cumsum(w)
  Num <- sum((2*r-1)*incvar*w)
  Den <- N*T
  # Gini coeficient
  Gini <- (Num/Den)-1
  Gini
}

# save space for estimate replicates
rep.est.gini<-numeric(ncol(mat.rep.pesos))

# compute estimate replicates
for (i in 1:ncol(mat.rep.pesos))rep.est.gini[i]<- ginivalue(incvar,
  mat.rep.pesos.pos[,i])

# mean of replicates  
mean.gini<-mean( rep.est.gini[pnad.design.boot$rscales>0])

# variance of estimate replicates
var.boot.gini<- sum((rep.est.gini-mean.gini)^2*
    pnad.design.boot$rscales)*pnad.design.boot$scale
# bootstrap gini se
sqrt(var.boot.gini)







 




