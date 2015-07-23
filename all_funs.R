library(vardpoor)
data(eusilc)
library(survey)
des_eusilc <- svydesign(ids=~1, weights=~rb050, data=eusilc) 
percent<-.6
order<-.5

quantfun<-function(x,inc=inc,h=h,w=w){
  N<-sum(w)
  u<-(x-inc)/h
  vectf<-exp(-u^2/2)/sqrt(2*pi)
  sum(vectf*w)/(N*h)
}



##########################################################################
# CALCULATION OF THE AT-RISK-OF-POVERTY THRESHOLD (ARPT)
####################################################################
  quantil_val<-svyquantile(~eqIncome,des_eusilc,order)
  quantil_val<-as.vector(thres_val)
  thres_val <- percent* quantil_val
  inc<-eusilc$eqIncome
  #bandwith parameter
  w<-weights(des_eusilc)
  N<-sum(w)
  sd_inc_w<-sqrt(coef(svyvar(~eqIncome,des_eusilc)))
  h<-sd_inc_w/(N^(-1/5))
  
  ## expression with quant_val
  f_quant1<-quantfun(quantil_val,inc=inc,h=h,w=w)
  
  
  lin_ARPT <- -(percent * (1/N)*((inc<=quantil_val)*1-order))/f_quant1
  
  # variance estimate 
  des_eusilc$variables$lin_ARPT<-lin_ARPT
  lin_ARPT_tot<-svytotal(~lin_ARPT,des_eusilc)
  var_ARPT<-attr(lin_ARPT_tot, "var")
  list_ARPT<-list(var_ARPT<-var_ARPT,se=sqrt(var_ARPT))
  
########################################################################
# LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE (ARPR)
############################################################  

  rate_val<-coef(svymean(~I((eqIncome<=thres_val)*1),des_eusilc))
  median_poor<-svyquantile(~eqIncome,subset(des_eusilc,eqIncome<=thres_val),.5)
  median_poor<-as.vector(median_poor)  
  
  #bandwith parameter
  
  # expression with tresh_ val
    f_quant2<-quantfun(thres_val,inc=inc,h=h,w=w)
    # expression with median_poor
    f_quant3<-quantfun(median_poor,inc=inc,h=h,w=w)
  lin_thres<--percent*(1/N)*((inc<=quant_val)*1-order/100)/f_quant1;
  lin_rate <-(1/N)*((inc<=thres_val)*1-rate_val)+f_quant2*lin_thres;
  lin_median_poor<-(0.5*lin_rate-(1/N)*((inc<=median_poor)*1-0.5*rate_val))/f_quant3;
  
  # linearized variable
    lin_ARPR <- median_poor*lin_thres/(thres_val*thres_val)-lin_median_poor/thres_val
    # compute variance
    lin_ARPR_tot<-svytotal(~lin_ARPR,des_eusilc)
    var_ARPR<-attr(lin_ARPR_tot, "var")
  
    list_ARPR<-list(rate_val=rate_val,se=sqrt(var_ARPR))
###############################################################################################
    
# LINEARIZATION OF THE RELATIVE MEDIAN AT-RISK-OF-POVERTY GAP (RMRPG)
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
  # linearization
  
  # Linearization of the bottom quantile
  
  f_quant1<-quantfun(quant_inf,inc=inc,h=h,w=w)
  lin_inf=-(1/N)*((inc<=quant_inf)*1-alpha1)/f_quant1
  
  # Linearization of the top quantile
  
  f_quant2<-quantfun(quant_sup,inc=inc,h=h,w=w)
  
  lin_sup=-(1/N)*((inc<=quant_sup)*1-alpha2)/f_quant2
  
  # Linearization of the total income for the top quintile 
  
  f_quant3<-quantfun(quant_sup,inc=inc,h=h,w=w)
  
  lin_num<-inc-inc*(inc<=quant_sup)*1-f_quant3*lin_sup
  
  # Linearization of the total income for the bottom quintile
  f_quant4<-quantfun(quant_inf,inc=inc,h=h,w=w)
  lin_den<-inc*(inc<=quant_inf)*1+f_quant4*lin_inf
  
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
  










