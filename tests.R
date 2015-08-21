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
d1vardpoor <- linarpt(Y="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
  dataset = dati, percentage = 60, order_quant=50)
str(d1vardpoor)
d1vardpoor$value
table(d1vardpoor$lin$lin_arpt)

## entire population
#  function svyarpt
d1 <-  svyarpt(~eqIncome, design=des_eusilc, .5, .6, h=htot,
  ncom=nrow(des_eusilc$variables), comp=TRUE)

d1$value
summary(d1$lin)
all.equal(d1vardpoor$lin$lin_arpt, d1$lin)


#  domains
#  using library vardpoor
dd <- linarpt(Y="eqIncome", id="IDd", weight="rb050", Dom="db040",
  dataset=dati, percentage=60, order_quant=50)

data.frame(db040=dd$value$db040, value=dd$value$threshold)
dd$quantile$quantile
# #  using functions

arpt_by_db040<- svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyarpt, order = .50, percent =.6, h= htot,ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE, keep.names = TRUE)


str(arpt_by_db040)
arpt_by_db040[, c("db040","statistic.value")]

for(i in (1:length(arpt_by_db040$statistic.lin)))print(table(arpt_by_db040$statistic.lin[[i]]))
for(i in 2:10 ) print(table(data.frame(dd$lin)[,6]))
str(data.frame(dd$lin))

# estimates match with the obtained by library vardpoor
str(as.data.frame(arpt_by_db040$statistic.lin))


#######################
# ARPR
#######################

# entire population
# library vardpoor
d2vardpoor <- linarpr(Y="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
  dataset = dati, percentage = 60, order_quant=50)


# function svyarpr
d2 <-svyarpr(~eqIncome, des_eusilc, .5, .6, h=htot,ncom=nrow(des_eusilc$variables), comp=TRUE)
# test

d2vardpoor$value==d2$value*100
all.equal(d2vardpoor$lin$lin_arpr,(d2$lin)*100)

# domains

d2vardpoor_db040 <- linarpr(Y="eqIncome", id="IDd", weight = "rb050", Dom = "db040",
  dataset = dati, percentage = 60, order_quant=50)
str(d2vardpoor_db040)
d2vardpoor_db040$value
table(d2vardpoor_db040$lin$lin_arpr__db040.Burgenland)

d2_db040<-svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyarpr, order = .50, percent =.6, h= htot, ARPT=d1,
  ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE)

table(d2_db040$statistic.lin[[1]])
# svyarpr OK!



##################
# RMPG
##################
data(eusilc)
dati=data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"

# full population
# library vardpoor
d3vardpoor<-linrmpg(Y="eqIncome", id="IDd", weight="rb050", Dom=NULL,
  dataset=dati, percentage=60, order_quant=50)
d3vardpoor$value

table(d3vardpoor$lin$lin_rmpg)

# function svyrmpg
d3<- svyrmpg(~eqIncome, des_eusilc, .5, .6, h=htot, ncom=nrow(des_eusilc$variables),ARPT=ARPT_pop  )
d3$value
table(d3$lin)


# domains
# By domains
dd<-linrmpg(Y="eqIncome", id="IDd", weight = "rb050", Dom="db040",
  dataset=dati, percentage=60, order_quant=50)
dd$value

table(dd$lin$lin_rmpg__db040.Tyrol)

##
ARPT_pop<-svyarpt(~eqIncome, design=des_eusilc, order = .50, percent =.6,
  h=htot, ncom=nrow(des_eusilc$variables), comp=TRUE)

dd1<-svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyrmpg, order = .50, percent =.6,
  h= htot, ARPT=ARPT_pop,  ncom=nrow(des_eusilc$variables), comp=TRUE, deff=FALSE, keep.var=FALSE)

dd1$statistic.value
table(dd1$statistic.lin[[6]])

###################
## S80/S20
###################
# for the whole population
# library vardpoor
d4vardpoor <- linqsr(Y="eqIncome", id="IDd", weight="rb050",
  Dom=NULL, dataset= dati, alpha=20)

d4vardpoor$value
summary(d4vardpoor$lin)

# function svyqsr
d4<-svyqsr(~eqIncome, des_eusilc, .20, h=htot, ncom=nrow(des_eusilc$variables), comp=TRUE)

#test
d4vardpoor$value$QSR==d4$value
all.equal(d4vardpoor$lin$lin_qsr,d4$lin)

summary(d4vardpoor$lin$lin_qsr)
summary(d4$lin)


# domain

dd4vardpoor <- linqsr(Y="eqIncome", id="IDd", weight="rb050",
  Dom="db040", dataset= dati, alpha=20)
dd4vardpoor$value
summary(dd4vardpoor$lin$lin_qsr__db040.Tyrol)


dd4<- svyby(~eqIncome, by= ~db040, design=des_eusilc, FUN=svyqsr, alpha=.20,  h= htot, ncom=nrow(des_eusilc$variables), comp=TRUE, incvec= eusilc$eqIncome, deff=FALSE, keep.var=FALSE)

dd4$statistic.value
summary(dd4$statistic.lin[[6]])
str(dd4)
(dd4$db040)[1:20]


## Tyrol


dd_Tyrol<-svyqsr(~eqIncome,  subset(des_eusilc,db040=="Tyrol"), .20, h=htot,
  ncom=nrow(eusilc),comp=TRUE, incvec = eusilc$eqIncome)

dd_Tyrol$value
summary(dd_Tyrol$lin)


##################
# GINI
#################

d5vardpoor<-lingini(Y="eqIncome", id="IDd", weight="rb050", dataset=dati)

d5vardpoor$value$Gini
summary(d5vardpoor$lin$lin_gini)
d5vardpoor$lin$lin_gini[1:50]
d5vardpoor$lin$IDd[1:50]

# function svygini
d5 <- svygini(~eqIncome, des_eusilc)

d5$gini_coef
summary(d5$lin)
# test
d5vardpoor$value$Gini==d5$gini_coef*100
all.equal(d5vardpoor$lin$lin_gini,  d5$lin*100)

d5_Tyrol <- svygini(~eqIncome, subset(des_eusilc, db040=="Tyrol"))
d5_Tyrol$gini_coef
length(d5_Tyrol$lin)

# domain
# using vardpoor
dd5vardpoor <- lingini(Y="eqIncome", id="IDd", weight="rb050", Dom=c("db040"), dataset=dati)
dd5vardpoor$value
summary(dd5vardpoor$lin$lin_gini__db040.Tyrol)

# using function
dd5<- svyby(~eqIncome,by=~db040, design=des_eusilc, FUN=svygini, ncom = nrow(eusilc),
  comp=TRUE, deff=FALSE, keep.var=FALSE)

dd5$statistic.gini_coef
all.equal(dd5$statistic.lin[[6]],dd5vardpoor$lin$lin_gini__db040.Tyrol)

dd5$statistic.lin[[6]][1:100]
dd5vardpoor$lin$lin_gini__db040.Tyrol[1:100]





# check variance estimates

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
  alpha = 20, confidence = .95, outp_lin = FALSE,
  outp_res = FALSE, several.ok=FALSE, type="linarpt")


data.frame(db040=aa$all_result$db040,value=aa$all_result$value, se=aa$all_result$se)


# using functions
des_eusilc1 <- svydesign(ids=~rb030, strata= ~db040, weights =~rb050, data=dataset1)

arpt_dom1 <- svyby(formula=~eqIncome, by=~db040, design=des_eusilc1, FUN = svyarpt,
  keep.var= FALSE, deff=FALSE)

arpt_dom1[,c("db040","statistic.value","statistic.se")]

# not ok!

############################################################################
## new form svyby to get the lin variable:

svybylin_svyrpt<-function(formula, by, design, FUN,...){
  byfactors<-model.frame(by, model.frame(design), na.action=na.pass)
  byfactor<-do.call("interaction", byfactors)
  uniquelevels<-sort(unique(byfactor))
  uniques <- match(uniquelevels, byfactor)
  # run for icdf0 for each group
  all.meds<- lapply(uniques,
    function(i) svyquantile(formula,design[byfactor%in%byfactor[i]],quantiles=.5,method="constant"))
  names(all.meds)<-uniquelevels
  all.meds<-lapply(all.meds,as.vector)
  all.lins<- lapply(uniques,
    function(i) FUN(formula,design[byfactor%in%byfactor[i]])$lin)
  # back to the original dimension
  all.lins.correct<-lapply(uniques,function(i){
    lin<-rep(0,length(byfactor))
    lin[byfactor%in%byfactor[i]]<-all.lins[[byfactor[i]]]
    lin
  })
  names(all.lins.correct)<-uniquelevels
  all.lins.correct
}

## example

# estimated using functions
by_db040_lin<- svybylin_svyrpt(formula=~eqIncome,by=~db040,design=des_eusilc,
  FUN=svyarpt,order = .50, percent =.6)

table(by_db040_lin$Tyrol)

# estimated using library vardpoor

table( d1$lin$lin_arpt__db040.Tyrol )

# not exactly equal!! needs checking
