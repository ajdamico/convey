######################################
# tests:  functions vs library vardpoor
######################################

library(vardpoor)
data(eusilc)
dati=data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"
library(survey)
des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
percent<-.6
order<-.5
htot<- h_fun(eusilc$eqIncome,eusilc$rb050)
################
# ARPT
###############
# library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_arptw <- linarpt(Y="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
  dataset = dati, percentage = 60, order_quant=50)
vardpoor_arptw$value
table(vardpoor_arptw$lin$lin_arpt)

## whole population
#  function svyarpt
 fun_arptw <-  svyarpt(~eqIncome, design=des_eusilc, .5, .6, h=htot,
  ncom=nrow(des_eusilc$variables), comp=TRUE)

fun_arptw$value
table(fun_arptw$lin)



#  domains
#  library vardpoor
vardpoor_arptd <- linarpt(Y="eqIncome", id="IDd", weight="rb050", Dom="db040",
  dataset=dati, percentage=60, order_quant=50)

vardpoor_arptd$value
table(vardpoor_arptd$lin$lin_arpt__db040.Tyrol)

#  function

fun_arptd<- svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyarpt, order = .50, percent =.6,
  h= htot,ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE, keep.names = TRUE)

fun_arptd$statistic.value
table(fun_arptd$statistic.lin[[6]])


#######################
# ARPR
#######################

# entire population
# library vardpoor
vardpoor_arprw <- linarpr(Y="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
  dataset = dati, percentage = 60, order_quant=50)


# function svyarpr
fun_arprw <-svyarpr(~eqIncome, des_eusilc, .5, .6, h=htot, ARPT=fun_arptw,  ncom=nrow(des_eusilc$variables))

# test
vardpoor_arprw$value==fun_arprw$value*100
all.equal(vardpoor_arprw$lin$lin_arpr,(fun_arprw$lin)*100)

# domains

#library vardpoor
vardpoor_arprd <- linarpr(Y="eqIncome", id="IDd", weight = "rb050", Dom = "db040",
  dataset = dati, percentage = 60, order_quant=50)

vardpoor_arprd$value
table(vardpoor_arprd$lin$lin_arpr__db040.Burgenland)

# svyarpr

fun_arprd<-svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyarpr, order = .50, percent =.6, h= htot, ARPT=fun_arptw, ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE)

unlist(fun_arprd$statistic.value)*100

table(fun_arprd$statistic.lin[[1]]*100)

##################
# RMPG
##################
data(eusilc)
dati=data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"

# whole population
# library vardpoor
vardpoor_rmpgw<-linrmpg(Y="eqIncome", id="IDd", weight="rb050", Dom=NULL,
  dataset=dati, percentage=60, order_quant=50)

vardpoor_rmpgw$value
table(vardpoor_rmpgw$lin$lin_rmpg)

# function svyrmpg
fun_rmpgw<- svyrmpg(~eqIncome, des_eusilc, .5, .6, h=htot, ncom=nrow(des_eusilc$variables), ARPT=fun_arptw)

fun_rmpgw$value*100
table(fun_rmpgw$lin*100)


# domains

# By domains
# library vardpoor
vardpoor_rmpgd<-linrmpg(Y="eqIncome", id="IDd", weight = "rb050", Dom="db040",
  dataset=dati, percentage=60, order_quant=50)

vardpoor_rmpgd$value
table(vardpoor_rmpgd$lin$lin_rmpg__db040.Tyrol)
# svyrmpg

fun_rmpgd<-svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyrmpg, order = .50, percent =.6,
  h= htot, ARPT=fun_arptw,  ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE)

unlist(fun_rmpgd$statistic.value)*100
table(fun_rmpgd$statistic.lin[[6]]*100)


###################
## S80/S20
###################
#  whole population

# library vardpoor
vardpoor_qsrw <- linqsr(Y="eqIncome", id="IDd", weight="rb050",
  Dom=NULL, dataset= dati, alpha=20)

vardpoor_qsrw$value
summary(vardpoor_qsrw$lin)

# function svyqsr
fun_qsrw<-svyqsr(~eqIncome, des_eusilc, .20, h=htot, ncom=nrow(des_eusilc$variables), 
  comp=TRUE, incvec = eusilc$eqIncome)

vardpoor_qsrw$value$QSR ==fun_qsrw$value

all.equal(vardpoor_qsrw$lin$lin_qsr,fun_qsrw$lin)

# domain

vardpoor_qsrd <- linqsr(Y="eqIncome", id="IDd", weight="rb050",
  Dom="db040", dataset= dati, alpha=20)
vardpoor_qsrd$value
summary(vardpoor_qsrd$lin$lin_qsr__db040.Tyrol)


fun_qsrd<- svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyqsr, alpha=.20,  h= htot, ncom=nrow(des_eusilc$variables), comp=TRUE, incvec= eusilc$eqIncome, deff=FALSE, keep.var=FALSE)

fun_qsrd$statistic.value
summary(fun_qsrd$statistic.lin[[6]])


##################
# GINI
#################
# whole population
# library vardpoor

vardpoor_giniw<-lingini(Y="eqIncome", id="IDd", weight="rb050", dataset=dati)

vardpoor_giniw$value$Gini
summary(vardpoor_giniw$lin$lin_gini)



# function svygini
fun_giniw <- svygini(~eqIncome, des_eusilc, ncom = nrow(eusilc), comp=TRUE)

fun_giniw$gini_coef
summary(fun_giniw$lin)



# domain
# using vardpoor
vardpoor_ginid <- lingini(Y="eqIncome", id="IDd", weight="rb050", Dom=c("db040"), dataset=dati)
vardpoor_ginid$value
summary(vardpoor_ginid$lin$lin_gini__db040.Tyrol)

# using function
fun_ginid<- svyby(~eqIncome,by=~db040, design=des_eusilc, FUN=svygini, ncom = nrow(eusilc),
  comp=TRUE, deff=FALSE, keep.var=FALSE)

unlist(fun_ginid$statistic.gini_coef)*100
summary(fun_ginid$statistic.lin[[6]]*100)

########################################end tests########################################


# variance estimes for the whole population

# include linearized variables in the design object
des_eusilc <- update(des_eusilc,linarpt=fun_arptw$lin, linarpr=fun_arprw$lin, linrmg=fun_rmpgw$lin,
   linqsr=fun_qsrw$lin, lingini=fun_giniw$lin)

SE(svytotal(~linarpt+linarpr+linrmg+linqsr+lingini, des_eusilc))

res <- data.frame(indicator= c("arpt", "arpr", "rmpg", "qsr", "gini"),
    value= c(fun_arptw$value,fun_arprw$value,fun_rmpgw$value, fun_qsrw$value, fun_giniw$gini_coef),
     se=SE(svytotal(~linarpt+linarpr+linrmg+linqsr+lingini, des_eusilc))
    )

res$cv<- (res$se/res$value)*100
 res
 
# variance estimates for domains
# RMPG

list_rmpg_lin<- fun_rmpgd$statistic.lin 
names(list_rmpg_lin)<- fun_rmpgd$db040
frame_rmpg_lin<-as.data.frame(list_rmpg_lin)
eusilc<-cbind(eusilc,frame_rmpg_lin)
des_eusilc<-svydesign(ids=~db040, weights=~rb050, data=eusilc)
form_dom<-make.formula(names(frame_rmpg_lin))
res_var_rmpg<- data.frame(domains=names(frame_rmpg_lin),
  values=unlist(fun_rmpgd$statistic.value),
se=SE(svytotal(form_dom,des_eusilc))
    )
res_var_rmpg$cv<-100*res_var_rmpg$se/res_var_rmpg$values
res_var_rmpg









