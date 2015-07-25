library(vardpoor)
data(eusilc)
library(survey)
des_eusilc <- svydesign(ids=~1, weights=~rb050, data=eusilc) 
percent<-.6
order<-.5
# auxilary functions
# estimate density function
densfun <- function(formula, design, x, type=c("F","S")){
inc <- terms.formula(formula)[[2]] 
 w<- weights(design)
 N<-sum(w) 
 sd_inc <-eval(bquote(sqrt(coef(svyvar(~.(inc),design))))) 
 h <-sd_inc/(N^(-1/5))
 df <- model.frame(design)
inc_var<-df[[as.character(inc)]]
 u<-(x-inc_var)/h
 vectf<-exp(-(u^2)/2)/sqrt(2*pi)
 if(type="F") res <- sum(vectf*w)/(N*h) else {
 v <- w* inc_var
 res<- sum(vectf*v)/h  
 }
 res
}

ICDF<-function(formula, design, x){
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
inc_var<-df[[as.character(inc)]]
w <- weights(design) 
N <- sum (w)
cdf_fun <- eval(bquote(svymean(~I((.(inc)<=x)*1), design)))
inf_fun<-(1/N)* ((inc_var<=x)-coef(cdf_fun))
inf_fun
}


Iq_alpha<- function(formula, design, alpha){
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



##########################################################################
# CALCULATION OF THE AT-RISK-OF-POVERTY THRESHOLD (ARPT)
####################################################################
  quantil_val<-svyquantile(~eqIncome,des_eusilc,order)
  quantil_val<-as.vector(quantil_val)
  ARPT <- percent* quantil_val
  lin_ARPT <- percent * Iq_alpha(~eqIncome,des_eusilc,order)
  
  # variance estimate 
  des_eusilc$variables$lin_ARPT<-lin_ARPT
  lin_ARPT_tot<-svytotal(~lin_ARPT,des_eusilc)
  var_ARPT<-attr(lin_ARPT_tot, "var")
  list_ARPT<-list(var_ARPT<-var_ARPT,se=sqrt(var_ARPT))
  
########################################################################
# LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE (ARPR)
############################################################  

  ARPR <- coef(svymean(~I((eqIncome<=ARPT)*1),des_eusilc))
    
  lin_ARPR <- ICDF(~eqIncome,des_eusilc,ARPT) + 
  densfun(~eqIncome,des_eusilc,ARPT, type="F")*lin_ARPT
    
    # compute variance
	des_eusilc$variables$lin_ARPR<-lin_ARPR
    lin_ARPR_tot<-svytotal(~lin_ARPR,des_eusilc)
    var_ARPR<-attr(lin_ARPR_tot, "var")
  
    list_ARPR<-list(rate_val=rate_val,se=sqrt(var_ARPR))


#################################################################
# LINEARIZATION OF THE RELATIVE MEDIAN POVERTY GAP
##########################################################	

# median income of people whose income is lower than ARPT (RMPG)

MED_p <- svyquantile(~eqIncome,subset(des_eusilc,eqIncome<= ARPT) ,.5)
MED_p<-as.vector(MED_p)
RMPG<- 1 - (MED_p/ ARPT)
lin_median <- Iq_alpha(~eqIncome,des_eusilc,.5)

lin_RMPG <- (MED_p*lin_ARPT)/(ARPT*ARPT) - lin_median/ARPT

# compute variance
des_eusilc$variables$lin_RMPG<-lin_RMPG
lin_RMPG_tot <- svytotal(~lin_RMPG,des_eusilc )
var_RMPG<-attr(lin_RMPG_tot, "var")
list_RMPG<-list(RMPG=RMPG,se=sqrt(var_RMPG))
	
	
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
  
  den_val <- coef(svytotal(~I((eqIncome>quant_sup)*1),des_eusilc))
  num_val <- coef(svytotal(~I((eqIncome<=quant_inf)*1),des_eusilc))
  share_ratio <-  num_val/den_val
  
  
  # Linearization of the bottom quantile
  
    lin_inf<- Iq_alpha(~eqIncome,des_eusilc,alpha1)
  
  # Linearization of the top quantile
  
  lin_sup <- Iq_alpha(~eqIncome,des_eusilc,alpha2)
    
  # Linearization of the total income for the top quantile 
  
  f_quant3<- densfun( ~eqIncome,des_eusilc,quant_sup, type="S")
  
  lin_num <- inc-inc*(inc<=quant_sup)-f_quant3*lin_sup
  
  # Linearization of the total income for the bottom quintile
  f_quant4 <- densfun( ~eqIncome,des_eusilc,quant_inf, type="S")
  lin_den <- inc*(inc<=quant_inf)*1+f_quant4*lin_inf
  
  # LINEARIZED VARIABLE OF THE SHARE RATIO
  lin_share_ratio<-(den_val*lin_num-num_val*lin_den)/(den_val*den_val)
 
  # estimate variance
  
  des_eusilc$variables$lin_share_ratio<-lin_share_ratio
  lin_share_ratio_tot<-svytotal(~lin_share_ratio,des_eusilc)
  var_lin_share_ratio<-attr(lin_share_ratio_tot, "var")
  
  list(share_ratio=share_ratio, se=sqrt(var_lin_share_ratio ))

##########################################################################################

# LINEARIZATION OF THE GINI COEFFICIENT (LIN_GINI)
####################################################  

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
  
  list(Gini,lin)

#########################################################################  
  










