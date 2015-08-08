###################################
# auxilary functions
#################################

######################################
# estimation of the density function
#####################################
densfun <- function(formula, design, x, type=c("F","S")){
inc <- terms.formula(formula)[[2]] 
 w<- weights(design)
 N<-sum(w)
 df <- model.frame(design)
 inc_var<-df[[as.character(inc)]]
 sd_inc <- sqrt((sum(w*inc_var*inc_var)-sum(w*inc_var)*sum(w*inc_var)/N)/N)
 h <-sd_inc/exp(0.2*log(sum(w))) 
 u<-(x-inc_var)/h
 vectf<-exp(-(u^2)/2)/sqrt(2*pi)
 if(type=="F") res <- sum(vectf*w)/(N*h) else {
 v <- w* inc_var
 res<- sum(vectf*v)/h  
 }
 res
}

#############################
# linearization of the cdf
##############################

icdf<-function(formula, design, x){
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
incvar<-df[[as.character(inc)]]
w <- weights(design) 
N <- sum (w)
poor<- (incvar<=x)*1
design$variables$poor <- poor
cdf_fun <- svymean(poor, design)
inf_fun<-(1/N)* ((incvar<=x)-coef(cdf_fun))
inf_fun
}

####################################
# linearization of the quantile
##################################

iqalpha<- function(formula, design, alpha){
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
incvar<-df[[as.character(inc)]]
q_alpha<- svyquantile(x =formula, design =design, quantiles= alpha, method="constant")
q_alpha<- as.vector(q_alpha)
w <- weights(design) 
N <- sum (w)
Fprime<- densfun(formula = formula, design = design, q_alpha, type="F")
iq <- -(1/(N*Fprime))*((incvar<=q_alpha)-alpha)
iq
}

####################################################################
# linearization of the total <= quantile (inf)  or total> quantile (sup)
#################################################################

isq <- function(formula, design, alpha,type=c("inf","sup")){
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  q_alpha<- svyquantile(x =formula, design =design, quantiles = alpha, method="constant")
  q_alpha<- as.vector(q_alpha)
  w <- weights(design) 
  N <- sum (w)
  Fprime<- densfun(formula = formula , design = design, q_alpha, type="S")
  iq<- iqalpha(formula = formula, design = design, alpha)
  isqalpha <- incvar*((incvar<=q_alpha))+ Fprime*iq 
  if(type=="inf")ires<- isqalpha else ires<- incvar - isqalpha
  ires
}

######### end of auxiliary functions #####################

##############################################################
# LINEARIZATION OF THE AT-RISK-OF-POVERTY THRESHOLD (ARPT)
#############################################################

svyarpt <- function(formula, design, order, percent ) {
  quant_val<- svyquantile(x = formula, design=design, 
  quantiles = order, method="constant")
  quant_val <- as.vector(quant_val)
  ARPT <- percent* quant_val
  lin_ARPT <- percent * iqalpha(formula = formula, design = design, alpha = order)
  # variance estimate 
  design$variables$lin_ARPT <- lin_ARPT
  lin_ARPT_tot <- svytotal(~lin_ARPT, design = design)
  var_ARPT <- attr(lin_ARPT_tot, "var")
  list(value = ARPT, se=sqrt(var_ARPT), lin = lin_ARPT)
  }


############################################################
# LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE (ARPR)
############################################################

svyarpr <- function(formula, design, order, percent){
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  list_ARPT <- svyarpt(formula = formula, design = design, 
  order = order, percent = percent)
  ARPT <- list_ARPT$value
  lin_ARPT <- list_ARPT$lin
  poor <- (incvar < ARPT) *1
  design$variables$poor <- poor
  ARPR <- svymean (~poor, design = design)
  ARPR <- coef (ARPR)
  lin_ARPR <- icdf(formula = formula, design = design, ARPT) + 
      densfun(formula = formula, design = design , ARPT, type="F")*lin_ARPT 
  # compute variance
  design$variables$lin_ARPR <- lin_ARPR
  lin_ARPR_tot<-svytotal(~lin_ARPR, design = design)
  var_ARPR<-attr(lin_ARPR_tot, "var")
  list(value = ARPR, se = sqrt(var_ARPR) , lin = lin_ARPR)
  }


#########################################################
# LINEARIZATION OF THE RELATIVE MEDIAN POVERTY GAP
########################################################	

svyrmpg <- function(formula, design, order, percent){
w<-weights(design) 
N<-sum(w)
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
incvar<-df[[as.character(inc)]]
list_ARPT <- svyarpt(formula=formula, design = design, 
order = order, percent = percent)
arpt <- list_ARPT$value
linarpt <- list_ARPT$lin
list_ARPR <- svyarpr(formula=formula, design = design, 
  order = order, percent = percent)
arpr <- list_ARPR$value
linarpr <- list_ARPR$lin
dsub<- subset (design, subset= (incvar<= arpt))
medp <- svyquantile(x = formula, dsub, .5) 
medp<-as.vector(medp)
RMPG <- 1 - (medp/ arpt)    
design$variables$poor<- (incvar<= arpt)*1
medprate<-coef(svymean(~poor,design = design))       
Fprimemedp <- densfun(formula = formula, design = design , medp, type="F")      
Fprimearpt<-  densfun(formula = formula, design = design, arpt, type="F")
# linearize cdf for medp 
ifmedp <- icdf(formula = formula, design = design, medp)
# linearize medp
linmedp <- (.5*linarpr-ifmedp)/Fprimemedp
# linearize RMPG
linrmpg <- (medp*linarpt/(arpt*arpt))-(linmedp/arpt)
# compute variance
design$variables$linrmpg <-linrmpg
linrmpgtot <- svytotal(~linrmpg, design= design)
varrmpg<-attr(linrmpgtot, "var")
list(value = RMPG, se=sqrt(varrmpg), lin = linrmpg  )
} 


######################################
# LINEARIZATION OF S80/S20
#####################################

  svyqsr <- function(formula, design, alpha= .20) {
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]  
  alpha1<- alpha
  alpha2<- 1-alpha  
  quant_inf <-  svyquantile(x = formula, design = design, quantiles = alpha1,
    method = "constant")
  quant_inf<-as.vector(quant_inf)
  quant_sup <- svyquantile(x = formula, design = design, quantiles= alpha2,
    method = "constant")
  quant_sup <- as.vector(quant_sup)
  tot_var<- coef(svytotal(x=formula, design = design))
  dsub80<- subset (design, subset= (incvar > quant_sup))
  S80 <- coef(svytotal(x= formula ,dsub80))
  dsub20<- subset (design, subset= (incvar<= quant_inf))
  S20 <- coef(svytotal(x= formula,dsub20))
  qsr<- S80/S20
  # Linearization of S20
  lin_S20 <- isq(formula = formula, design = design, alpha1, type="inf")
  
  # Linearization of S80
  lin_S80 <- isq(formula = formula, design = design, alpha2, type="sup")
  
  # LINEARIZED VARIABLE OF THE SHARE RATIO
  
  lin_qsr<-(S20*lin_S80-S80*lin_S20)/(S20*S20)
  
  # estimate variance
  
  design$variables$lin_qsr <- lin_qsr
  lin_qsr_tot <- svytotal(~lin_qsr,design= design)
  var_qsr <- attr(lin_qsr_tot, "var")
  list(value = qsr, se = sqrt(var_qsr ),lin = lin_qsr )
  }


###############################################
# LINEARIZATION OF THE GINI COEFFICIENT 
##############################################
  
svygini<- function(formula, design){
  inc <- terms.formula(formula)[[2]] 
  w<- weights(design)
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  w <- w[order(incvar)]
  incvar <- incvar[order(incvar)]
  # population size 
   N <-  sum(w)
   # sample size
   n <- length(inc)
   # total income
   T <- sum(incvar*w)
   # cumulative weight
   r <- cumsum(w)
   Num <- sum((2*r-1)*incvar*w)
   Den <- N*T
   # Gini coeficient
   Gini <- (Num/Den)-1
    # cumulative distribution function
   F <- cumsum(w/N)
   
   # partial weighted function 
   G <- cumsum(incvar*w)
   
   # Gini coefficient linerized variable  
   lin_gini<-(2*(T-G+incvar*w+N*(incvar*F))-incvar-(Gini+1)*(T+N*incvar))/(N*T)
     # variance estimation
   
   design$variables$lin_gini <- lin_gini
   lin_gini_tot<-svytotal(~lin_gini,design = design)
   gini_var<-attr(lin_gini_tot, "var")
   list(gini_coef=Gini,gini_var=gini_var, lin = lin_gini)
   } 


######################################  
# tests:  functions vs library vardpoor
######################################

library(vardpoor)
data(eusilc)
library(survey)
des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc) 
percent<-.6
order<-.5

################
# ARPT
###############
# library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
d1vardpoor <- linarpt(Y="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
  dataset = dati, percentage = 60, order_quant=50)
d1vardpoor$value
summary(d1vardpoor$lin)

#  function svyarpt
d1 <-  svyarpt(~eqIncome, des_eusilc, .5, .6)
d1$value
summary(d1$lin)


#######################
# ARPR
#######################
# library vardpoor

d2vardpoor <- linarpr(Y="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
  dataset = dati, percentage = 60, order_quant=50)
d2vardpoor$value
summary(d2vardpoor$lin)

# function svyarpr 
d2 <-svyarpr(~eqIncome, des_eusilc, .5, .6)
d2$value
summary(d2$lin)

##################
# RMPG
##################
# library vardpoor
d3vardpoor<-linrmpg(Y="eqIncome", id="IDd", weight="rb050", Dom=NULL,
  dataset=dati, percentage=60, order_quant=50)
d3vardpoor$value
summary(d3vardpoor$lin)

# function svyrmpg 
d3<- svyrmpg(~eqIncome, des_eusilc, .5, .6)
d3$value
summary(d3$lin)


###################
## S80/S20
###################
# library vardpoor
d4vardpoor <- linqsr(Y="eqIncome", id="IDd", weight="rb050",
  Dom=NULL, dataset= dati, alpha=20)
d4vardpoor$value
summary(d4vardpoor$lin)

# function svyqsr
d4<-svyqsr(~eqIncome, des_eusilc, .20)
d4$value
summary(d4$lin)

##################
# GINI
#################

d5vardpoor<-lingini(Y="eqIncome", id="IDd", weight="rb050", dataset=dati)

d5vardpoor$value
summary(d5vardpoor$lin)

# function svygini
d5 <- svygini(~eqIncome, des_eusilc)
d5$gini_coef
summary(d5$lin)












