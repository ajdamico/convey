
svygini <-  function( formula , design , ... ){

  UseMethod( "svygini" , design )

}


svygini.survey.design<- function(formula, design, ncom, comp=TRUE,...){
  inc <- terms.formula(formula)[[2]]
  w<- weights(design)
  ind<-names(w)
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
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
  # cumulative distribution function
  F <- cumsum(w)/N

  # partial weighted function
  G <- cumsum(incvar*w)

  # Gini coefficient linearized variable
  lin_gini<-(2*(T-G+incvar*w+N*(incvar*F))-incvar-(Gini+1)*(T+N*incvar))/(N*T)
  # original order
  #lin_gini<- lin_gini[ind]
  # complete 0's
  names(lin_gini)<-names(w)
  lin_gini_comp <- complete(lin_gini, ncom)
  if(comp) res<-lin_gini_comp else res <-lin_gini
  list(gini_coef=Gini, lin = res)
}


svygini.svyrep.design<- function(formula, design,...){
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  ComputeGini <- function(x, w){
   w <- w[order(x)]
   x <- x[order(x)]
   N <-  sum(w)
   n<- length(x)
   T<- sum(x*w)
   r <- cumsum(w)
   Num <- sum((2*r-1)*x*w)
   Den <- N*T
   (Num/Den)-1
  }
  ws <- weights(design, "sampling")
  rval <- ComputeGini(incvar, ws)
  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) ComputeGini(incvar, wi))
  variance <- svrVar(qq,design$scale,design$rscales, mse = design$mse, coef = rval)
  list(value = rval, se = sqrt(variance))
}
