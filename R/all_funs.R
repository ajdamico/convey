
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

#############################
# linearization of the cdf
##############################

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
cdf_fun <- svymean(poor, design)
inf_fun<-(1/N)* ((incvar<=x)-coef(cdf_fun))
names(inf_fun)<-ind
inf_fun_comp<- complete(inf_fun,ncom)
if(comp)lin<-inf_fun_comp else lin<-inf_fun
list(value= cdf_fun, lin=lin)
}



####################################
# linearization of the quantile
##################################

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

####################################################################
# linearization of the total <= quantile (inf)  or total> quantile (sup)
#################################################################

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





















