###################################
# auxilary functions
#################################

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



# h  has to computed from all values

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




######### end of auxiliary functions #####################

##############################################################
# LINEARIZATION OF THE AT-RISK-OF-POVERTY THRESHOLD (ARPT)
#############################################################

svyarpt <- function(formula, design, order = .50, percent =.6, h, ncom, comp,...) {
  w <- weights(design)
  ind<-names(w)
  quant_val<- svyquantile(x = formula, design=design,
  quantiles = order, method="constant")
  quant_val <- as.vector(quant_val)
  ARPT <- percent* quant_val
  lin_ARPT <- percent * iqalpha(formula = formula, design = design,
    alpha = order,h=h, ncom=ncom, comp=FALSE,incvec = NULL)$lin
  names(lin_ARPT)<-ind
  lin_ARPT_comp<-complete(lin_ARPT, ncom)
  if(comp)lin<-lin_ARPT_comp else lin<-lin_ARPT
  list(value = ARPT, lin = lin)
  }


############################################################
# LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE (ARPR)
############################################################

svyarpr <- function(formula, design, order = .50, percent =.6, h, ARPT, ncom, ...){
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  w <- weights(design)
  ARPT_val <- ARPT$value
  lin_ARPT <- ARPT$lin
  poor <- (incvar < ARPT_val) *1
  design <- update (design, poor = poor)
  ARPRC <- svymean (~poor, design = design)
  ARPRC <- coef (ARPRC)
  lin_ARPR <- icdf(formula = formula, design = design, ARPT_val, ncom=ncom, comp=TRUE)$lin +
      densfun(formula = formula, design = design , ARPT_val, htot=h, fun="F")*lin_ARPT
  list(value = ARPRC, lin = lin_ARPR)
  }


#########################################################
# LINEARIZATION OF THE RELATIVE MEDIAN POVERTY GAP
########################################################

svyrmpg <- function(formula, design, order =.50, percent = .60, ncom , h, comp, ARPT, ...){
w<-weights(design)
ind<-names(w)
N<-sum(w)
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
incvar<-df[[as.character(inc)]]
arpt <- ARPT$value
linarpt <- ARPT$lin
arpr <-sum((incvar<=arpt)*w)/N
dsub<- subset (design, subset= (incvar<= arpt))
medp <- svyquantile(x = formula, dsub, .5, method="constant")
medp<-as.vector(medp)
RMPG <- 1 - (medp/ arpt)
Fprimemedp <- densfun(formula = formula, design = design , medp, htot=h, fun="F")
Fprimearpt<-  densfun(formula = formula, design = design, arpt, htot=h, fun="F")
 # linearize cdf of ARPT
ifarpr0<-(1/N)*((incvar<=arpt)-arpr)
names(ifarpr0)<-names(w)
ifarpr0<-complete(ifarpr0,ncom)
ifarpr <- ifarpr0+Fprimearpt*linarpt
#ifarpt<- icdf(formula = formula, design = design, arpt, ncom=ncom, comp=TRUE)$lin+
# linearize cdf of medp
ifmedp <-(1/N)*((incvar<=medp)-0.5*arpr)
names(ifmedp) <- names(w)
ifmedp<- complete(ifmedp, ncom)
# linearize median of poor
linmedp <- (0.5*ifarpr-ifmedp)/Fprimemedp
# linearize RMPG
linrmpg <- (medp*linarpt/(arpt*arpt))-(linmedp/arpt)
list(value = RMPG, lin = linrmpg)
}


######################################
# LINEARIZATION OF S80/S20
#####################################

  svyqsr <- function(formula, design, alpha= .20, ncom,comp,incvec, ...) {
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  w<-weights(design)
  ind<-names(w)
  alpha1<- alpha
  alpha2<- 1-alpha
  quant_inf <-  svyquantile(x = formula, design = design, quantiles = alpha1,
    method = "constant")
  quant_inf<-as.vector(quant_inf)
  quant_sup <- svyquantile(x = formula, design = design, quantiles= alpha2,
    method = "constant")
  quant_sup <- as.vector(quant_sup)
  tot_var<- sum(incvar*w)
  rich<- (incvar > quant_sup)*incvar
  S80 <- sum(rich*w)
  poor<- (incvar <= quant_inf)*incvar
  S20 <- sum(poor*w)
  qsr<- S80/S20
    # Linearization of S20
  lin_S20 <- isq(formula = formula, design = design, alpha1, type="inf", h=NULL,
    ncom= ncom, comp = FALSE, incvec = incvec)$lin
  # Linearization of S80
  lin_S80 <- isq(formula = formula, design = design, alpha2, type="sup", h=NULL,
    ncom= ncom, comp=FALSE, incvec=incvec)$lin

  # LINEARIZED VARIABLE OF THE SHARE RATIO

  lin_qsr<-(S20*lin_S80-S80*lin_S20)/(S20*S20)

  names(lin_qsr)<-ind
  lin_qsr_comp<-complete(lin_qsr, ncom)
  if(comp) lin<-lin_qsr_comp else lin<-lin_qsr
  list(value = qsr, lin = lin_qsr)
  }


###############################################
# LINEARIZATION OF THE GINI COEFFICIENT
##############################################

svygini<- function(formula, design, ncom, comp,...){
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
   lin_gini<- lin_gini[ind]
   names(lin_gini)<-names(w)
   lin_gini_comp <- complete(lin_gini, ncom)
   if(comp) res<-lin_gini_comp else res <-lin_gini

   list(gini_coef=Gini, lin = res)
   }










