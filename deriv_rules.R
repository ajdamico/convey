# influence function for constant

iconst<-function(formula, design){
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]  
  list(value=0, lin=rep(0,length(incvar)))
}

# infuence function for total
itot<- function(formula, design){
inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
incvar<-df[[as.character(inc)]]  
value<-coef(svytotal(x=formula,design=design))
lin<- incvar 
list(value=value, lin=lin)  
}



## derivation rules for influence functions of functionals
## linear combination of functionals
##  a, b - scalars
#  T, S - lists with two components: value and lin
# IF  - list with with two components
# Fprime - real function  

cl_inf<-function(a, b, T, S){
  lin<-a*T$lin+b*S$lin
  value<- a*T$value+b*S$value
 }

# product of o two functionals
prod_inf<-function(T, S){
  
value <- T$value*S$value  
lin <-T$value*S$lin+S$value*T$lin
list(value=value, lin=lin)
}

# ratio of functionals

ratio_inf<-function(T, S){
value <- T$value/S$value  
lin <- (S$value*T$lin-T$value*S$lin)/((S$value)^2)
list(value=value, lin=lin)
}

# composition of two functionals ????
comp_inf <- function(T,S){
 itsm<-rep(S$value,length(T$lin)) 
 itsm*S$lin 
}

# function of a functional
fun_par_inf<- function(S, IF, Fprime){
value<- IF  
n<-length(S$lin)
if(is.null(S$lin))lin<-solve(diag(Fprime(S$value),n,n),-IF(S$value))
else lin<-IF(S$value)+ Fprime(S$value)*S$lin
list(value= value, lin=lin)
}

##############################################################
# examples
# compare ratio_inf and icdf
#########################################################

# create variables
des_eusilc <- update(des_eusilc, num = (eqIncome<= 20000)*1)
des_eusilc <- update(des_eusilc, den= rep(1,nrow(model.frame(des_eusilc))))

# get infuence function of numerator and denominator 
NUM <- itot(~num, des_eusilc)
DEN <- itot(~den, des_eusilc)

# influence function of ratio
resul<- ratio_inf(NUM,DEN)
str(resul)

# drectly using function icdf

resul_icdf<-icdf(~eqIncome,des_eusilc, 20000)
str(resul_icdf)


icdf1<-function(formula, design, x,...){
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  poor<- (incvar<=x)*1
  one <- rep(1, length(incvar))
  design <- update(design, poor = poor, one=one)
  NUM <- itot(~num, des_eusilc)
  DEN <- itot(~den, des_eusilc)
  cdf_fun <- NUM$value/DEN$value
  inf_fun<-ratio_inf(NUM,DEN)$lin
  list(value=cdf_fun, lin=inf_fun)
}

icdf1(~eqIncome,des_eusilc, 20000)->lixo

## quantile: median

MED<-svyquantile(~eqIncome,des_eusilc,quantiles=.5)
MED<-as.vector(MED)
SMED<-list(value=MED,lin=NULL)
cdf <- function(x){
  
}
  
  

  


fun_par_inf(SMED,)










