######################################
# tests:  convey vs vardpoor
######################################

library(vardpoor)
data(eusilc)
dati=data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"
library(survey)
# create a design object
des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)


################
# ARPT
###############

# linearize the indicator arpt:  whole sample
#  library vardpoor
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_arptw <- linarpt(Y="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
  dataset = dati, percentage = 60, order_quant=50)
# show results from vardpoor
  # arpt estimate
vardpoor_arptw$value
  # linearized arpt for whole sample
table(vardpoor_arptw$lin$lin_arpt)


#  library convey
# whole sample
 htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
 fun_arptw <-  svyarpt(~eqIncome, design=des_eusilc, .5, .6, h=htot,
  ncom=nrow(des_eusilc$variables), comp=TRUE)
# show results from convey
  # arpt estimate
fun_arptw$value
  # linearized arpt for whole sample
table(fun_arptw$lin)


#  Linearize the indicator arpt: domains
#  library vardpoor
vardpoor_arptd <- linarpt(Y="eqIncome", id="IDd", weight="rb050", Dom="db040",
  dataset=dati, percentage=60, order_quant=50)
# show results from library convey:
  # arpt estimate
vardpoor_arptd$value
  # linearized arpt for the domain Tyrol
table(vardpoor_arptd$lin$lin_arpt__db040.Tyrol)


#  library convey
fun_arptd<- svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyarpt, order = .50, percent =.6,
  h= htot,ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE, keep.names = TRUE)
# show results from library convey
   # arpt estimates
fun_arptd$statistic.value
   # linearized arpt for the domain Tyrol
table(fun_arptd$statistic.lin[[6]])



#######################
# ARPR
#######################

# linearize the indicator arpr:  whole sample
# vardpoor
vardpoor_arprw <- linarpr(Y="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
  dataset = dati, percentage = 60, order_quant=50)
# show results from vardpoor
  # arpr estimate
vardpoor_arptw$value
  # linearized arpr for whole sample
table(vardpoor_arptw$lin$lin_arpt)

# convey
fun_arprw <-svyarpr(~eqIncome, des_eusilc, .5, .6, h=htot, ARPT=fun_arptw,
  ncom=nrow(des_eusilc$variables))

# compare results from vardpoor and convey
vardpoor_arprw$value==fun_arprw$value*100
all.equal(vardpoor_arprw$lin$lin_arpr,(fun_arprw$lin)*100)

#  Linearize the indicator arpr: domains

#library vardpoor
vardpoor_arprd <- linarpr(Y="eqIncome", id="IDd", weight = "rb050", Dom = "db040",
  dataset = dati, percentage = 60, order_quant=50)
# show results from vardpoor
 #  estimates of arpr
vardpoor_arprd$value
# linearized arpr for the domain Tyrol
table(vardpoor_arprd$lin$lin_arpr__db040.Burgenland)

# convey
fun_arprd<-svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyarpr, order = .50, percent =.6, h= htot, ARPT=fun_arptw, ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE)
# show results from convey
 # arpr estimates
unlist(fun_arprd$statistic.value)*100
# linearized arpr for Burgenland
table(fun_arprd$statistic.lin[[1]]*100)

##################
# RMPG
##################

# linearize the indicator rmpg:  whole sample
# library vardpoor
vardpoor_rmpgw<-linrmpg(Y="eqIncome", id="IDd", weight="rb050", Dom=NULL,
  dataset=dati, percentage=60, order_quant=50)
# show results from library vardpoor
 # rmpg estimate
vardpoor_rmpgw$value
 # linearized rmpg
table(vardpoor_rmpgw$lin$lin_rmpg)

# library convey
fun_rmpgw<- svyrmpg(~eqIncome, des_eusilc, .5, .6, h=htot, ncom=nrow(des_eusilc$variables),
  ARPT=fun_arptw)
# show results from library convey
  # rmpg estimate
fun_rmpgw$value*100
 # rmpg linearized variable
table(fun_rmpgw$lin*100)


# Linearize the indicator rmpg: domains


# library vardpoor
vardpoor_rmpgd<-linrmpg(Y="eqIncome", id="IDd", weight = "rb050", Dom="db040",
  dataset=dati, percentage=60, order_quant=50)
# show results from library vardpoor
  # rmpg estimates
vardpoor_rmpgd$value
  # rmpg linearizd variable for the domain Tyrol
table(vardpoor_rmpgd$lin$lin_rmpg__db040.Tyrol)

# library convey
fun_rmpgd<-svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyrmpg, order = .50, percent =.6,
  h= htot, ARPT=fun_arptw,  ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE)

# show results from library convey
  # rmpg estimates
unlist(fun_rmpgd$statistic.value)*100
  # rmpg linearizd variable for the domain Tyrol
table(fun_rmpgd$statistic.lin[[6]]*100)


###################
## S80/S20
###################
# linearize the indicator qsr:  whole sample
# library vardpoor

vardpoor_qsrw <- linqsr(Y="eqIncome", id="IDd", weight="rb050",
  Dom=NULL, dataset= dati, alpha=20)
# show results from the library vardpoor
# qsr estimate
vardpoor_qsrw$value
#  summary of the linearized qsr
summary(vardpoor_qsrw$lin)

# library convey
fun_qsrw<-svyqsr(~eqIncome, des_eusilc, .20, h=htot, ncom=nrow(des_eusilc$variables),
  comp=TRUE, incvec = eusilc$eqIncome)

# compare results from vardpoor and convey
  # qsr estimate
vardpoor_qsrw$value$QSR ==fun_qsrw$value
  # linearized qsr
all.equal(vardpoor_qsrw$lin$lin_qsr,fun_qsrw$lin)

# Linearize the indicator qsr: domains

# library vardpoor
vardpoor_qsrd <- linqsr(Y="eqIncome", id="IDd", weight="rb050",
  Dom="db040", dataset= dati, alpha=20)
# Show results from library vardpoor
 # qsr estimate
vardpoor_qsrd$value
 # linearized qsr
summary(vardpoor_qsrd$lin$lin_qsr__db040.Tyrol)

# library convey
fun_qsrd<- svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyqsr, alpha=.20,  h= htot, ncom=nrow(des_eusilc$variables), comp=TRUE, incvec= eusilc$eqIncome, deff=FALSE, keep.var=FALSE)
# Show results from library convey
 # qsr estimate
unlist(fun_qsrd$statistic.value)
 # linearized qsr
summary(fun_qsrd$statistic.lin[[6]])

##################
# GINI
#################
# linearize the indicator gini:  whole sample
# library vardpoor

vardpoor_giniw<-lingini(Y="eqIncome", id="IDd", weight="rb050", dataset=dati)
# show the results from library vardpoor
# gini estimate
vardpoor_giniw$value$Gini
# linearized gini
summary(vardpoor_giniw$lin$lin_gini)

# library convey
fun_giniw <- svygini(~eqIncome, des_eusilc, ncom = nrow(eusilc), comp=TRUE)
# Show the results from library convey
  # gini estimate
fun_giniw$gini_coef
  # linearized gini
summary(fun_giniw$lin)

# Linearize the indicator qsr: domains
# library vardpoor
vardpoor_ginid <- lingini(Y="eqIncome", id="IDd", weight="rb050", Dom=c("db040"), dataset=dati)
# show results from library vardpoor
# gini estimates
vardpoor_ginid$value
# linearized gini for the Tyrol domain
summary(vardpoor_ginid$lin$lin_gini__db040.Tyrol)

# library convey
fun_ginid<- svyby(~eqIncome,by=~db040, design=des_eusilc, FUN=svygini, ncom = nrow(eusilc),
  comp=TRUE, deff=FALSE, keep.var=FALSE)
# show results from library convey
  # gini estimates
unlist(fun_ginid$statistic.gini_coef)*100
  # linearized gini for the Tyrol domain
summary(fun_ginid$statistic.lin[[6]]*100)


###########################################################
## se estimation
############################################################

#  indicator arpt, whole sample
# library vardpoor
data(eusilc)
dataset <- data.frame(1:nrow(eusilc),eusilc)
colnames(dataset)[1] <- "IDd"
dataset1 <- dataset[1:1000,]

aa<-varpoord(Y = "eqIncome", w_final = "rb050",
  Y_thres = NULL, wght_thres = NULL,
  ID_household = "db030", id = "IDd",
  H = "db040", PSU = "rb030", N_h = NULL,
  sort = NULL, Dom = NULL,
  gender = NULL, X = NULL,
  X_ID_household = NULL, g = NULL,
  datasetX = NULL,
  q = rep(1, if (is.null(datasetX))
    nrow(as.data.frame(H)) else nrow(datasetX)),
  dataset =  dataset1, percentage=60, order_quant=50,
  alpha = 20, confidence = .95, outp_lin = TRUE,
  outp_res = FALSE, several.ok=FALSE, type="linarpt")

# Show results from library vardpoor
   # arpt estimate
aa$all_result$value
   # se of the arpt estimate
aa$all_result$se

# library convey
dataset1<- eusilc[1:1000,]

# introduce fpc in the design object:

N_h <- aggregate(dataset1[,"rb050"],list(db040=dataset1$db040),FUN= sum)
names(N_h)[2]<-"Nh"
n_h<- aggregate(dataset1[,"rb050"],list(db040=dataset1$db040),FUN= length)
names(n_h)[2]<-"nh"
f_h<-merge(N_h,n_h)
dataset1<-merge(dataset1,f_h)


des_eusilc0<- svydesign(id=~rb030, strata = ~db040, weights = ~rb050,
  data = dataset1, fpc=~I(nh/Nh), nest = TRUE)

# estimate bandwdth using the whole sample
htot <- h_fun(dataset1$eqIncome, dataset1$rb050)

test_arpt<-svyarpt(~eqIncome, des_eusilc0, .5, .6, h=htot, ncom=nrow(dataset1), comp=TRUE )
#show results from convey
   # arpt estimate
test_arpt$value
  # se of the arpt estimate
SE_lin(test_arpt, des_eusilc0 )


##  indicator arpt, domains

##vardpoor
data(eusilc)
dataset <- data.frame(1:nrow(eusilc),eusilc)
colnames(dataset)[1] <- "IDd"
dataset1 <- dataset[1:1000,]
aa1 <-varpoord(Y = "eqIncome", w_final = "rb050",
  Y_thres = NULL, wght_thres = NULL,
  ID_household = "db030", id = "IDd",
  H = "db040", PSU = "rb030", N_h = NULL,
  sort = NULL, Dom = "db040",
  gender = NULL, X = NULL,
  X_ID_household = NULL, g = NULL,
  datasetX = NULL,
  q = rep(1, if (is.null(datasetX))
    nrow(as.data.frame(H)) else nrow(datasetX)),
  dataset =  dataset1, percentage=60, order_quant=50,
  alpha = 20, confidence = .95, outp_lin = TRUE,
  outp_res = FALSE, several.ok=FALSE, type="linarpt")

# Show results from vardpoor
res_vardpoor_arpt<- as.data.frame(aa1$all_result)
res_vardpoor_arpt<- res_vardpoor_arpt[,c("type","db040","value", "se")]
as.data.frame(as.data.frame(aa1$all_result))[,c("type","db040","value", "se")]

## convey

test_arpt_dom1<-svyby(~eqIncome, by=~db040, FUN=svyarpt, design = des_eusilc0, .5, .6,
  h=htot, ncom=nrow(dataset1), comp=TRUE, deff=FALSE, keep.var=FALSE)
se_arpt_dom1 <- SE_lin(test_arpt_dom1,des_eusilc0)

# show resuts from convey
res_convey_arpt<- data.frame(domain=test_arpt_dom1$db040, type= rep("arpt",length(se_arpt_dom1)),
  value= unlist(test_arpt_dom1$statistic.value), se=unlist(se_arpt_dom1))


# Note: the linearized variable by convey and vardpoor coincide
# the difference in the se value is due to the difference in the estimation of the of  variance of
# the total


# all indicators, domains

# vardpoor

aa2 <- varpoord(Y = "eqIncome", w_final = "rb050",
  Y_thres = NULL, wght_thres = NULL,
  ID_household = "db030", id = "IDd",
  H = "db040", PSU = "rb030", N_h = NULL,
  sort = NULL, Dom = "db040",
  gender = NULL, X = NULL,
  X_ID_household = NULL, g = NULL,
  datasetX = NULL,
  q = rep(1, if (is.null(datasetX))
    nrow(as.data.frame(H)) else nrow(datasetX)),
  dataset =  dataset, percentage=60, order_quant=50,
  alpha = 20, confidence = .95, outp_lin = TRUE,
  outp_res = TRUE, several.ok=FALSE, type="all_choices")

# Show results from vardpoorresult for ARPT from example 3:

# convey
N_h <- aggregate(eusilc[,"rb050"],list(db040=eusilc$db040),FUN= sum)
names(N_h)[2]<-"Nh"
n_h<- aggregate(eusilc[,"rb050"],list(db040=eusilc$db040),FUN= length)
names(n_h)[2]<-"nh"
f_h<-merge(N_h,n_h)
eusilc<-merge(eusilc,f_h)


des_eusilc<- svydesign(id=~rb030, strata = ~db040, weights = ~rb050,
  data = eusilc, fpc=~I(nh/Nh), nest = TRUE)
htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
arptw<- svyarpt(~eqIncome, design=des_eusilc, .5, .6, h=htot,
  ncom=nrow(des_eusilc$variables), comp=TRUE)


# arpt domains
test_arpt_dom<-svyby(~eqIncome, by=~db040, FUN=svyarpt, design = des_eusilc, order=.5,
  percent=.6, h=htot, ncom=nrow(eusilc), comp=TRUE, deff=FALSE, keep.var=FALSE)
se_arpt_dom <- SE_lin(test_arpt_dom,des_eusilc)
# organize results for arpt
frame_arpt<- data.frame(domain=test_arpt_dom$db040, type= rep("arpt",length(se_arpt_dom)),
  value= unlist(test_arpt_dom$statistic.value), se=unlist(se_arpt_dom))

# arpr domains
test_arpr_dom<-svyby(~eqIncome, by=~db040, FUN=svyarpr, design = des_eusilc, order=.5,
  percent=.6,  h=htot, ARPT=arptw, ncom=nrow(eusilc), comp=TRUE, deff=FALSE, keep.var=FALSE)
se_arpr_dom <- SE_lin(test_arpr_dom,des_eusilc)
# organize results for arpr
frame_arpr<- data.frame(domain=test_arpr_dom$db040, type= rep("arpr",length(se_arpr_dom)),
  value= unlist(test_arpr_dom$statistic.value), se=unlist(se_arpr_dom))

# rmpg domains
test_rmpg_dom<-svyby(~eqIncome, by=~db040, FUN=svyrmpg, design = des_eusilc, order=.5,
  percent=.6, h=htot, ncom=nrow(eusilc), comp=TRUE, ARPT=test_arpt, deff=FALSE, keep.var=FALSE)
se_rmpg_dom <- SE_lin(test_rmpg_dom,des_eusilc)
# organize results for rmpg
frame_rmpg<- data.frame(domain=test_rmpg_dom$db040, type= rep("rmpg",length(se_rmpg_dom)),
  value= unlist(test_rmpg_dom$statistic.value), se=unlist(se_rmpg_dom))

# qsr domains
test_qsr_dom<-svyby(~eqIncome, by=~db040, FUN=svyqsr, design = des_eusilc, alpha=.20,
  ncom=nrow(eusilc), comp=TRUE, incvec= eusilc$eqIncome,deff=FALSE, keep.var=FALSE)
se_qsr_dom <- SE_lin(test_qsr_dom,des_eusilc)
# organize results for qsr
frame_qsr<- data.frame(domain=test_qsr_dom$db040, type= rep("qsr",length(se_arpr_dom)),
  value= unlist(test_qsr_dom$statistic.value), se=unlist(se_qsr_dom))

# gini domains
test_gini_dom<-svyby(~eqIncome, by=~db040, FUN=svygini, design = des_eusilc,
  ncom=nrow(eusilc), comp=TRUE,deff=FALSE, keep.var=FALSE)
se_gini_dom <- SE_lin(test_gini_dom,des_eusilc)
# organize results for gini
frame_gini<- data.frame(domain=test_gini_dom$db040, type= rep("gini",length(se_gini_dom)),
  value= unlist(test_gini_dom$statistic.gini_coef), se=unlist(se_gini_dom))


# compare results from vardpoor and convey
# arpt
frame_arpt
subset(as.data.frame(aa2$all_result)[,c("type","db040","value", "se")],type=="ARPT")

# arpr
frame_arpr
subset(as.data.frame(aa2$all_result)[,c("type","db040","value", "se")],type=="ARPR")


# rmpg
frame_rmpg
subset(as.data.frame(aa2$all_result)[,c("type","db040","value", "se")],type=="RMPG")


# qsr
frame_qsr
subset(as.data.frame(aa2$all_result)[,c("type","db040","value", "se")],type=="QSR")

# gini
frame_gini
subset(as.data.frame(aa2$all_result)[,c("type","db040","value", "se")],type=="GINI")




