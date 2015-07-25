library(vardpoor)
data(eusilc)
library(survey)
des_eusilc <- svydesign(ids=~1, weights=~rb050, data=eusilc) 
percent<-.6
order<-.5

# auxilary functions:

# estimate density function
densfun <- function(formula, design, x, type=c("F","S")){
inc <- terms.formula(formula)[[2]] 
 w<- weights(design)
 N<-sum(w)
 df <- model.frame(design)
 inc_var<-df[[as.character(inc)]]
 sd_inc <- sqrt((sum(w*inc_var*inc_var)-sum(w*inc_var)*sum(w*inc_var)/N)/N)
 h <-sd_inc/(N^(-1/5))
 df <- model.frame(design)
inc_var<-df[[as.character(inc)]]
 u<-(x-inc_var)/h
 vectf<-exp(-(u^2)/2)/sqrt(2*pi)
 if(type=="F") res <- sum(vectf*w)/(N*h) else {
 v <- w* inc_var
 res<- sum(vectf*v)/h  
 }
 res
}

# linearization of cdf
icdf<-function(formula, design, x){
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
inc_var<-df[[as.character(inc)]]
w <- weights(design) 
N <- sum (w)
cdf_fun <- eval(bquote(svymean(~I((.(inc)<=x)*1), design)))
inf_fun<-(1/N)* ((inc_var<=x)-coef(cdf_fun))
inf_fun
}

# linearization of quantile
iqalpha<- function(formula, design, alpha){
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
inc_var<-df[[as.character(inc)]]
q_alpha<- eval(bquote(svyquantile(~.(inc), design, alpha)))
q_alpha<- as.vector(q_alpha)
w <- weights(design) 
N <- sum (w)
Fprime<- eval(bquote(densfun(~.(inc), design, q_alpha, type="F")))
iqalpha <- -(1/(N*Fprime))*((inc_var<=q_alpha)-alpha)
iqalpha
}

# linearization of total <= quantile (inf)  or  > quantile (sup)
isq <- function(formula, design, alpha,type=c("inf","sup")){
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  inc_var<-df[[as.character(inc)]]
  q_alpha<- eval(bquote(svyquantile(~.(inc), design, alpha)))
  q_alpha<- as.vector(q_alpha)
  w <- weights(design) 
  N <- sum (w)
  Fprime<- eval(bquote(densfun(~.(inc), design, q_alpha, type="S")))
  iqalpha<- eval(bquote(iqalpha(~.(inc), design, alpha)))
  isqalpha <- inc_var*((inc_var<=q_alpha))+ Fprime*iqalpha 
  if(type=="inf")ires<- isqalpha else ires<- inc_var - isqalpha
}


##########################################################################
# CALCULATION OF THE AT-RISK-OF-POVERTY THRESHOLD (ARPT)
####################################################################
  quantil_val<-svyquantile(~eqIncome,des_eusilc,order)
  quantil_val<-as.vector(quantil_val)
  ARPT <- percent* quantil_val
  lin_ARPT <- percent * iqalpha(~eqIncome,des_eusilc,order)
  
  # variance estimate 
  des_eusilc$variables$lin_ARPT<-lin_ARPT
  lin_ARPT_tot<-svytotal(~lin_ARPT,des_eusilc)
  var_ARPT<-attr(lin_ARPT_tot, "var")
  list_ARPT<-list(ARPT<-ARPT,se=sqrt(var_ARPT))
  list_ARPT
  
########################################################################
# LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE (ARPR)
############################################################  

  ARPR <- coef(svymean(~I((eqIncome<=ARPT)*1),des_eusilc))
    
  lin_ARPR <- icdf(~eqIncome,des_eusilc,ARPT) + 
  densfun(~eqIncome,des_eusilc,ARPT, type="F")*lin_ARPT
    
    # compute variance
	des_eusilc$variables$lin_ARPR<-lin_ARPR
    lin_ARPR_tot<-svytotal(~lin_ARPR,des_eusilc)
    var_ARPR<-attr(lin_ARPR_tot, "var")
  
    list_ARPR<-list(rate_val=ARPR,se=sqrt(var_ARPR))
    list_ARPR

#################################################################
# LINEARIZATION OF THE RELATIVE MEDIAN POVERTY GAP
##########################################################	

# median income of people whose income is lower than ARPT (RMPG)

MED_p <- svyquantile(~eqIncome,subset(des_eusilc,eqIncome<= ARPT) ,.5)
MED_p<-as.vector(MED_p)
RMPG<- 1 - (MED_p/ ARPT)
lin_median <- iqalpha(~eqIncome,des_eusilc,.5)

lin_RMPG <- (MED_p*lin_ARPT)/(ARPT*ARPT) - lin_median/ARPT

# compute variance
des_eusilc$variables$lin_RMPG<-lin_RMPG
lin_RMPG_tot <- svytotal(~lin_RMPG,des_eusilc )
var_RMPG<-attr(lin_RMPG_tot, "var")
list_RMPG<-list(RMPG=RMPG,se=sqrt(var_RMPG))
list_RMPG	
	
###############################################################################################
    
# LINEARIZATION OF S80/S20
######################################################################
   
  alpha<-.20
  alpha1<- alpha
  alpha2<- 1-alpha
  quant_inf <-  svyquantile(~eqIncome, des_eusilc, alpha1)
  quant_inf<-as.vector(quant_inf)
  quant_sup <- svyquantile(~ eqIncome, des_eusilc, alpha2)
  quant_sup<-as.vector(quant_sup)
  tot_var<- coef(svytotal(~eqIncome,des_eusilc))
  S80 <- coef(svytotal(~eqIncome,subset(des_eusilc,eqIncome>quant_sup)))
  S20 <- coef(svytotal(~eqIncome,subset(des_eusilc,eqIncome<=quant_inf)))
  share_ratio <- S80/S20
  
  
  # Linearization of S20
  lin_S20 <- isq(~eqIncome, des_eusilc, alpha1, type="inf")
  
  # Linearization of S80
    lin_S80 <- isq(~eqIncome, des_eusilc, alpha2, type="sup")
    
  # LINEARIZED VARIABLE OF THE SHARE RATIO
    
  lin_share_ratio<-(S20*lin_S80-S80*lin_S20)/(S20^2)
 
  # estimate variance
  
  des_eusilc$variables$lin_share_ratio <- lin_share_ratio
  lin_share_ratio_tot <- svytotal(~lin_share_ratio,des_eusilc)
  var_lin_share_ratio <- attr(lin_share_ratio_tot, "var")
  
  list_share_ratio <- list(share_ratio = share_ratio, se = sqrt(var_lin_share_ratio ))
  list_share_ratio

##########################################################################################

# LINEARIZATION OF THE GINI COEFFICIENT (LIN_GINI)
####################################################  
inc <- eusilc$eqIncome
 w <- eusilc$rb050
 
  inc<-inc[order(inc)]
  w<-w[order(inc)]
  # population size 
  N = sum(w)
  # sample size
  n<-length(inc)
  # total income
  T<-sum(inc*w)
  # cumulative weight
  r<-cumsum(w)
  Num<-sum((2*r-1)*inc*w)
  Den<-N*T
  # Gini coeficient
  Gini<-Num/Den-1
  
  # cumulative distribution function
  F<-cumsum(w/N)
  
  # partial weighted function 
  G<-cumsum(inc*w)
  
  # Gini coefficient linerized variable  
  lin_gini<-(2*(T-G+inc*w+N*(inc*F))-inc-(Gini+1)*(T+N*inc))/(N*T)
  
  # variance estimation
  
  des_eusilc$variables$lin_gini<-lin_gini
  lin_gini_tot<-svytotal(~lin_gini,des_eusilc)
  gini_var<-attr(lin_gini_tot, "var")
  list_gini<-list(gini_coef=Gini,gini_var=gini_var)
  list_gini

#########################################################################  
  










