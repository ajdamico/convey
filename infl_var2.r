# inc: income variable
# order: Order of the income quantile (by default, 50%)
# percent: Percentage of the income quantile (by default, 60%)
# des: survey design object


# CALCULATION OF THE AT-RISK-OF-POVERTY THRESHOLD
library(survey)
ARPT<- function(inc, des, percent=.6, order=.5){
quant_val<-svyquantile(inc,des,order)
var_ARPT <- percent* quant_val
inc_vec<- model.frame(inc,des$variables)
#bandwith parameter
w<-weights(des)
N<-sum(w)
sd_inc_w<-sqrt(sum(w*inc^2)-(sum(w*inc))^2)/sum(w)
h_hat<-sd_inc_w/(N^(-1/5))
u<-(quant_val-inc)/h_hat
vect_f<-exp(-u^2/2)/sqrt(2*pi)
f_quant<-sum(vect_f*w)/(N*h_hat)
lin_IARPT <- -(percent * (1/N)*(inc<=quant_val-order))/f_quant
list(var_ARPT = var_ARPT, lin_ARPT = IARPT)
}

# LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE
ARPR<-function(inc,des, percent=.6,order=.5){
quant_val<-coef(svyquantile(inc,des,order))
thres_val <- percent* quant_val
rate_val<-coef(svymean(~I((inc<=treshold)*1),des))
#bandwith parameter
w<-weights(des)
N<-sum(w)
sd_inc_w<-sqrt(sum(w*inc^2)-(sum(w*inc))^2)/sum(w)
h_hat<-sd_inc_w/(N^(-1/5))
u1<-(quant_val-inc)/h_hat
vect_f1<-exp(-u1^2/2)/sqrt(2*pi)
f_quant1<-sum(vect_f1*w)/(N*h_hat)
u2<-(thres_val-inc)/h_hat
vect_f2<-exp(-u2^2/2)/sqrt(2*pi)
f_quant2<-sum(vect_f2*w)/(N*h_hat)
lin_thres <- -(percent * (1/N)*(inc<=quant_val-order))/f_quant1
lin_ARPT<-(1/N)*((inc<=thres_val)-rate_val)+f_quant2*lin_thres
list(rate_val=rate_val,lin_ARPT=lin_ARPT)
}

# LINEARIZATION OF THE RELATIVE MEDIAN AT-RISK-OF-POVERTY GAP

RMRPG<-function(inc,des, percent=.6,order=.5){
quant_val<-coef(svyquantile(inc,des,order))
thres_val <- percent* quant_val
rate_val<-coef(svymean(~I((inc<=treshold)*1),des))
median_poor<-coef(svyquantile(~I(I((inc<=treshold)*1)),des,.5))
var_RMRPG<-thres_val-median_poor
# linearization
w<-weights(des)
N<-sum(w)
sd_inc_w<-sqrt(sum(w*inc^2)-(sum(w*inc))^2)/sum(w)
h_hat<-sd_inc_w/(N^(-1/5))

u1<-(quant_val-inc)/h_hat
vect_f1<-exp(-u1^2/2)/sqrt(2*pi)
f_quant1<-sum(vect_f1*w)/(N*h_hat)

u2<-(thres_val-inc)/h_hat
vect_f2<-exp(-u2^2/2)/sqrt(2*pi)
f_quant2<-sum(vect_f2*w)/(N*h_hat)

u3<-(median_poor-inc)/h_hat
vect_f3<-exp(-u3^2/2)/sqrt(2*pi)
f_quant3<-sum(vect_f3*w)/(N*h_hat)

lin_thres<- -(percent * (1/N)*(inc<=quant_val-order))/f_quant1
lin_rate<- (1/N)*((inc<=thres_val)-rate_val)+f_quant2*lin_thres
lin_median_poor<- (0.5*lin_rate-(1/N)*((inc<= median_poor)-0.5*rate_val))/f_quant3

lin_RMRPG<-(median_poor*lin_thres/(thres_val*thres_val)-lin_median_poor/thres_val)
list(var_RMRPG=var_RMRPG,lin_RMRPG=lin_RMRPG)

}

# LINEARIZATION OF THE INCOME QUINTILE SHARE RATIO S80/S20

SRAT_S80_S20<- function(inc,des, alpha=.20){
alpha1<- alpha
alpha2<- 1-alpha
quant_inf <- coef(svyquantile(inc, des, alpha1))
quant_sup <- coef(svyquantile(inc, des, alpha2))
den_val <- coef(svytotal(~I((inc>quant_sup)*1),des))
num_val <- coef(svytotal(~I((inc<=quant_inf)*1),des))
share_ratio <-  num_val/den_val
# linearization
w <- weights(des)
N<-sum(w)
sd_inc_w<-sqrt(sum(w*inc^2)-(sum(w*inc))^2)/sum(w)
h_hat<-sd_inc_w/(N^(-1/5))
# Linearization of the bottom quantile
u1<-(quant_inf-inc)/h_hat
vect_f1<-exp(-u1^2/2)/sqrt(2*pi)
f_quant1<-sum(vect_f1*w)/(N*h_hat)
lin_inf=-(1/N)*((inc<=quant_inf)-alpha)/f_quant1

# Linearization of the top quantile

u2<-(quant_sup-inc)/h_hat
vect_f2<-exp(-u2^2/2)/sqrt(2*pi)
f_quant2<-sum(vect_f2*w)/(N*h_hat)

lin_sup=-(1/N)*((inc<=quant_sup)-alpha2/100)/f_quant2

# Linearization of the total income for the top quintile 

u3=(quant_sup-inc)/h_hat
vect_f3=exp(-(u3^2)/2)/sqrt(2*pi)
f_quant3=(vect_f3*v)/h_hat
lin_num=inc-inc*(inc<=quant_sup)-f_quant3*lin_sup

# Linearization of the total income for the bottom quintile

u4=(quant_inf-inc)/h_hat
vect_f4=exp(-(u4^2)/2)/sqrt(2*pi)
f_quant4=(vect_f4*v)/h_hat
lin_den=inc*(inc<=quant_inf)+f_quant4*lin_inf

# LINEARIZED VARIABLE OF THE IQ SHARE RATIO
lin_share_ratio<-((den_val)*lin_num-(num_val)*lin_den)/(den_val*den_val)
list(share_ratio=share_ratio, lin_share_ratio )
}


# LINEARIZATION OF THE GINI COEFFICIENT
  LIN_GINI<-function(inc, w){
    inc<-inc[order(inc)]
    wght<-w[order(inc)]
    # population size 
    N = sum(w)
    # sample size
    n<-length(inc)
    # total income
    T<-sum(inc*w)
    # cumulative weight
    r<-cumsum(wght)
    Num<-sum((2*r-1)*inc*w)
    Den<-N*T
        # Gini coeficient
    Gini<-Num/Den-1
    
    # cumulative distribution function
    F<-cumsum(w/N)
    
    # partial weighted function 
    G<-cumsum(inc*w)
    
    # Gini coefficient linerized variable  
    lin<-(2*(T-G+inc*w+N*(inc*F))-inc-(Gini+1)*(T+N*inc))/(N*T)
    list(Gini,lin)
  }
  
  ## testes 
  
  library(vardpoor)
  data(eusilc)
  library(survey)
  
  des_eusilc <- svydesign(ids=~1, weights=~rb050, data=eusilc) 
  
  eusilc_ARPT<-ARPT(inc=~eqIncome,des=des_eusilc)
  
  test_fun<-function(inc, des, percent=.6, order=.5){
    quant_val<-eval(bquote(svymean(~.(inc),des)))
    quant_val  
  }
  
  test_fun(eqIncome,des_eusilc)
  
  eval(bquote(svymean(~.(eqIncome),des_eusilc))
  
)
