
complete<-function(x,n){
  ind.all<-as.character(1:n)
  x.comp<-rep(0,n)
  names(x.comp)<-ind.all
  x.comp[names(x)]<-x
  x.comp
}


######################################
# estimation of the density function
#####################################



# h  has to be computed from all values

h_fun<-function(inc_var, w){
  N<-sum(w)
  sd_inc <- sqrt((sum(w*inc_var*inc_var)-sum(w*inc_var)*sum(w*inc_var)/N)/N)
  h <-sd_inc/exp(0.2*log(sum(w)))
  h
 }


densfun <- function(formula, design, x, htot=NULL, fun=c("F","S"),...){
inc <- terms.formula(formula)[[2]]
 w<- weights(design)
 N<-sum(w)
 df <- model.frame(design)
 inc_var<-df[[as.character(inc)]]
 sd_inc <- sqrt((sum(w*inc_var*inc_var)-sum(w*inc_var)*sum(w*inc_var)/N)/N)
 h <-sd_inc/exp(0.2*log(sum(w)))
 if(!is.null(htot)) h<- htot
 u<-(x-inc_var)/h
 vectf<-exp(-(u^2)/2)/sqrt(2*pi)
 if(fun=="F") res <- sum(vectf*w)/(N*h) else {
 v <- w* inc_var
 res<- sum(vectf*v)/h
 }
 res
}

#' Linearization of the cdf function of a variable
#'
#' Computes the linearized variable of the cdf function in a point.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param ncom length of the income vector for the whole sample
#' @param comp logical variable \code{TRUE} if the linearized variable for domains
#' should be completed with zeros
#' @return a list with two components: the cdf estimate \code{value}
#' and the linearized cdf variable \code{lin}.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{arpr}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#'of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.

#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' icdf_eqIncome <-icdf(~eqIncome, design=des_eusilc, 20000, ncom=nrow(eusilc), comp=TRUE)
#'icdf_eqIncome$value
#' @export

icdf<-function(formula, design, x, ncom, comp, ...){
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
incvar<-df[[as.character(inc)]]
w <- weights(design)
ind<-names(w)
N <- sum (w)
poor<- (incvar<=x)*1
design <- update(design, poor = poor)
# rate of poor
cdf_fun <- coef(svymean(poor, design))
inf_fun<-(1/N)* ((incvar<=x)-cdf_fun)
names(inf_fun)<-ind
inf_fun_comp<- complete(inf_fun,ncom)
if(comp)lin<-inf_fun_comp else lin<-inf_fun
list(value= cdf_fun, lin=lin)
}



#' Linearization of a variable quantile
#'
#' Computes the linearized variable of a quantile of variable.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param alpha the order of the quantile
#' @param ncom length of the income vector for the whole sample
#' @param comp logical variable \code{TRUE} if the linearized variable for domains
#' should be completed with zeros
#' @param invec vector of sample values of the variable whose quantile is to be linearized
#' @return a list with two components: the quantile estimate \code{value}
#' and the linearized quantile variable \code{lin}.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{arpr}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#'of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.

#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' iqalpha_eqIncome <-iqalpha(~eqIncome, design=des_eusilc, .50, ncom=nrow(eusilc),
#' comp=TRUE, eusilc$eqIncome )
#'iqalpha_eqIncome$value
#' @export

iqalpha<- function(formula, design, alpha, h=NULL, ncom, comp, incvec=NULL, ...){
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
incvar<-df[[as.character(inc)]]
w <- weights(design)
ind<-names(w)
q_alpha<- svyquantile(x =formula, design =design, quantiles= alpha,
  method="constant")
q_alpha<- as.vector(q_alpha)
N <- sum (w)
Fprime<- densfun(formula = formula, design = design, q_alpha, htot=h, fun="F")
iq <- -(1/(N*Fprime))*((incvar<=q_alpha)-alpha)
if(!is.null(incvec)){ iq <- -(1/(N*Fprime))*((incvec<=q_alpha)-alpha); comp<-FALSE}
if(comp){
names(iq)<-ind
iq_comp<- complete(iq, ncom)
res=iq_comp
}else res<-iq
list(value=q_alpha, lin=res)
}

#' Linearization of the total above a quantile or below a quantile
#'
#' Computes the linearized variable of the total in the lower or upper tail of
#' the distribution of a variable.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param alpha the order of the quantile
#' @param type  \code{inf} for the lower tail and \code{sup} for the upper tail
#' @param h  bandwidth to estimate the derivative of the total in the tails
#' @param ncom length of the income vector for the whole sample
#' @param invec vector of sample values of the variable whose quantile is to be linearized
#' @return a list with two components: the tail total estimate \code{value}
#' and the linearized of the tail total \code{lin}.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{arpr}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#'of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.

#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' isq_eqIncome <-isq(~eqIncome, design=des_eusilc, .80, "sup", htot,
#' ncom=nrow(eusilc),  eusilc$eqIncome)
#'isq_eqIncome$value
#' @export


isq <- function(formula, design, alpha, type = c("inf","sup"), h=NULL, ncom, incvec,...){
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  w <- weights(design)
  ind<-names(w)
  q_alpha<- svyquantile(x =formula, design =design, quantiles = alpha, method="constant")
  q_alpha<- as.vector(q_alpha)
  if(type=="inf"){
    inc_inf<-(incvar<=q_alpha)*incvar
    tot<- sum(inc_inf*w)} else{
    inc_sup<-(incvar>q_alpha)*incvar
    tot<-sum(inc_sup*w)
  }
  Fprime<- densfun(formula = formula , design = design, q_alpha, htot= h, fun= "S")
  iq <- iqalpha(formula = formula, design = design, alpha, h = h, ncom = ncom, comp = TRUE, incvec = incvec)$lin
  isqalpha <- incvec*((incvec<=q_alpha))+ Fprime*iq
  if(type=="inf")ires<- isqalpha else ires<- incvec - isqalpha
  list(value=tot, lin=ires )
}



computeQuantiles<-function(xx, w, p=quantiles){
  if (any(is.na(xx))) return(NA*p)
  oo<-order(xx)
  cum.w<-cumsum(w[oo])/sum(w)
  cdf<-approxfun(cum.w,xx[oo],method="constant", f=1,
    yleft=min(xx),yright=max(xx),ties=min)
  cdf(p)
}




# function to extract the variance

SE_lin <- function(object, design){

  if(length(object)==2){
    t<-object$lin
    x<- update(design,t=t)
    res<-SE(svytotal(~t,design ))
  }
  else{
  lincomp<- object$statistic.lin
  list_se<-lapply(lincomp,
    function(t){
      x<- update(design,t=t)
      SE(svytotal(~t,design ) )
    }
  )
    names(list_se)<-object[[1]]
    res<- list_se
    }

  res
}













