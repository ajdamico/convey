# new arpr
############################################
svyarpr.survey.design1 <- function(formula, design, h, ARPT, ncom,...){
  ARPR<-fun_par_inf(ARPT, "icdf", "densfun", formula=formula ,design= design,
    ncom=ncom ,  comp= TRUE, htot=h, fun="F")
  list(value = ARPR$value, lin = ARPR$lin)
}
############################################

# new icdf

icdf1 <- function(formula, design, x, ncom, comp, invec=NULL...) {
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar <- df[[as.character(inc)]]
  w <- weights(design)
  ind <- names(w)
  poor <- (incvar <= x) * 1
  num_obj <- list(value=sum(poor*w), lin=poor)
  den_obj<-list(value=sum(w), lin=rep(1,length(w)))
  if(!is.null(invec)){
    num_obj<- list(value=sum(poor*w),lin=(invec<=x)*1)
    den_obj<- list(value=sum(w), rep(1,length(invec)) )
  }
  cdflin<-ratio_inf(num_obj,den_obj)
  cdf_fun <- cdflin$value
  inf_fun <- cdflin$lin
  names(inf_fun) <- ind
  inf_fun_comp <- complete(inf_fun, ncom)
  if (comp)
  lin <- inf_fun_comp else lin <- inf_fun
  list(value = cdf_fun, lin = lin)
}


## tests:
# whole sample
des_eusilc <- svydesign(ids = ~db040, weights = ~rb050, data = eusilc)
icdf_eqIncome <-icdf(~eqIncome, design=des_eusilc, 20000, ncom=nrow(eusilc), comp=TRUE)
str(icdf_eqIncome)
icdf_eqIncome$value
SE_lin(icdf_eqIncome,des_eusilc )

icdf1_eqIncome <-icdf1(~eqIncome, design=des_eusilc, 20000, ncom=nrow(eusilc), comp=TRUE)
str(icdf1_eqIncome)
icdf1_eqIncome$value
SE_lin(icdf1_eqIncome,des_eusilc)

# subset design
# compare icdf and icdf1
des_Tyrol<-subset(des_eusilc, db040=="Tyrol")

icdf_eqIncome_Tyrol <-icdf(~eqIncome, design=des_Tyrol, 20000, ncom=nrow(eusilc), comp=TRUE)
str(icdf_eqIncome_Tyrol)
icdf_eqIncome_Tyrol$value
SE_lin(icdf_eqIncome_Tyrol, des_eusilc)
icdf1_eqIncome_Tyrol <-icdf1(~eqIncome, design=des_Tyrol, 20000, ncom=nrow(eusilc), comp=TRUE)
str(icdf1_eqIncome_Tyrol)
icdf1_eqIncome_Tyrol$value
SE_lin(icdf1_eqIncome_Tyrol, des_eusilc)

icdf_eqIncome_Tyrol <-icdf(~eqIncome, design=des_Tyrol, 20000, ncom=nrow(eusilc), comp=TRUE)

icdf_eqIncome_Tyrol$value

SE_lin(icdf_eqIncome_Tyrol,des_eusilc )
str(icdf_eqIncome1)
SE_lin(icdf_eqIncome1,des_eusilc )
all.equal()
all.equal(icdf_eqIncome,icdf_eqIncome1)

# use svy ratio
eusilc$poor<-(eusilc$eqIncome<=20000)*1
eusilc$one<-rep(1,nrow(eusilc))
eusilc$poor_Tyrol<-(eusilc$eqIncome<=20000)&(eusilc$db040=="Tyrol")*1
eusilc$one_tyrol<- (eusilc$db040=="Tyrol")*1



des_Tyrol<-subset(des_eusilc, db040=="Tyrol")

svyratio(~poor, ~one, des_Tyrol )

svyratio(~poor_Tyrol, ~one_tyrol, des_eusilc)
# svyratio by domains
svyby(~poor, by=~db040,denominator= ~one, design=des_eusilc,FUN=svyratio )
# convey by domains

lixo<- svyby(~eqIncome,by= ~db040, design=des_eusilc, FUN=icdf, x=20000,ncom=nrow(eusilc),
  comp=TRUE ,deff=FALSE, keep.var=FALSE )

unlist(lixo$statistic.value)
unlist(SE_lin(lixo,des_eusilc))
data.frame(db040=lixo$db040, value=unlist(lixo$statistic.value), se=unlist(SE_lin(lixo,des_eusilc)))

####################################################################################
# new iqalpha

iqalpha1 <- function(formula, design, alpha, h = NULL, ncom, comp, incvec = NULL,
  ...) {
  w <- weights(design)
  N<- sum(w)
  ind <- names(w)
  q_alpha <- svyquantile(x = formula, design = design, quantiles = alpha, method = "constant")
  q_alpha <- as.vector(q_alpha)
  qalphalin<-list(value=q_alpha, lin=NULL)
  objlinq <- fun_par_inf(qalphalin, "icdf", "densfun", formula=formula ,design= design,
    ncom=ncom ,  comp= TRUE, htot=NULL, fun="F",invec=invec)
  iq<-objlinq$lin
#   if (!is.null(incvec)) {
#     iq <- -(1/(N * Fprime)) * ((incvec <= q_alpha) - alpha)
#     comp <- FALSE
#   }
  if (comp) {
    names(iq) <- ind
    iq_comp <- complete(iq, ncom)
    res = iq_comp
  } else res <- iq
  list(value = q_alpha, lin = res)
}
###################################################################33

# Tests
htot <- h_fun(eusilc$eqIncome, eusilc$rb050)

iqalpha_old <- iqalpha(~eqIncome, des_eusilc, .50, h=htot, ncom=nrow(eusilc), comp=TRUE)
str(iqalpha_old)


iqalpha_new <- iqalpha1(~eqIncome, des_eusilc, .50, h=htot, ncom=nrow(eusilc), comp=TRUE)
str(iqalpha_new)

all.equal(iqalpha_old,iqalpha_new )

# with invec

iqalph_old <- iqalpha(~eqIncome, des_eusilc, .50, h=htot, ncom=nrow(eusilc), comp=TRUE, invec=eusilc$eqIncome)
str(iqalph_old )
iqalph_new <- iqalpha1(~eqIncome, des_eusilc, .50, h=htot, ncom=nrow(eusilc), comp=TRUE, invec=eusilc$eqIncome)
str(iqalph_new)
all.equal(iqalpha_old,iqalpha_new )
## svyapt

svyarpt1.survey.design <- function(formula, design, order = 0.5, percent = 0.6, h,
  ncom, comp, ...) {
  w <- weights(design)
  ind <- names(w)
  #quant_val <- svyquantile(x = formula, design = design, quantiles = order, method = "constant")
  #quant_val <- as.vector(quant_val)
  #ARPT <- percent * quant_val
  lin_arpt <- iqalpha1(formula = formula, design = design, alpha = order,
    h = h, ncom = ncom, comp = FALSE, incvec = NULL)
  arpt<- percent*(lin_arpt$value)
  arptlin<-percent*(lin_arpt$lin)
  names(arptlin) <- ind
  arptlin_comp <- complete(arptlin, ncom)
  if (comp)
    lin <- arptlin_comp else lin <- arptlin
  list(value = arpt, lin = lin)
}

# teste

arpt_eqIncome <-svyarpt(~eqIncome, design=des_eusilc, .5, .6, h = htot,
  ncom=nrow(eusilc), comp=TRUE)
str(arpt_eqIncome)


arpt_eqIncome1 <-svyarpt1.survey.design(~eqIncome, design=des_eusilc, .5, .6, h = htot,
  ncom=nrow(eusilc), comp=TRUE)
str(arpt_eqIncome1)

##############

isq <- function(formula, design, alpha, type = c("inf", "sup"), h = NULL, ncom, incvec,
  ...) {
  qalphalin <- iqalpha1(formula = formula, design = design, alpha, h = h, ncom = ncom,
    comp = TRUE, incvec = incvec)
  q_alpha<-qalphalin$value
  iq <-qalphalin$lin
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar <- df[[as.character(inc)]]
  w <- weights(design)
  ind <- names(w)
  #q_alpha <- svyquantile(x = formula, design = design, quantiles = alpha, method = "constant")
  #q_alpha <- as.vector(q_alpha)
  q_alpha<-qalphalin$value
  if (type == "inf") {
    inc_inf <- (incvar <= q_alpha) * incvar
    tot <- sum(inc_inf * w)
  } else {
    inc_sup <- (incvar > q_alpha) * incvar
    tot <- sum(inc_sup * w)
  }
  Fprime <- densfun(formula = formula, design = design, q_alpha, htot = h, fun = "S")
  isqalpha <- incvec * ((incvec <= q_alpha)) + Fprime * iq
  if (type == "inf")
    ires <- isqalpha else ires <- incvec - isqalpha
  list(value = tot, lin = ires)
}

####################

svygpg1 <- function(x, design, sex, ncom, comp=TRUE) {
  wage <- terms.formula(x)[[2]]
  df <- model.frame(design)
  wage <- df[[as.character(wage)]]
  w<- weights(design)
  ind<-names(w)
  # sex factor
  mf <- model.frame(sex, design$variables, na.action = na.pass)
  xx <- lapply(attr(terms(sex), "variables")[-1], function(tt) model.matrix(eval(bquote(~0 +
      .(tt))), mf))
  cols <- sapply(xx, NCOL)
  sex <- matrix(nrow = NROW(xx[[1]]), ncol = sum(cols))
  scols <- c(0, cumsum(cols))
  for (i in 1:length(xx)) {
    sex[, scols[i] + 1:cols[i]] <- xx[[i]]
  }
  colnames(sex) <- do.call("c", lapply(xx, colnames))
  sex <- as.matrix(sex)
  col_female <- grep("female", colnames(sex))
  col_male <- setdiff(1:2, col_female)
  # cria linearization objects of totals
  INDM <-list(value = sum(sex[, col_male]*w), lin=sex[, col_male])
  INDF <- list(value = sum(sex[, col_female]*w), lin=sex[, col_female])
  TM<- list(value = sum(wage*sex[, col_male]*w), lin=wage*sex[, col_male])
  TF<- list(value = sum(wage*sex[, col_female]*w), lin=wage*sex[, col_female])
  list_all_tot <-list(INDM=INDM,INDF=INDF,TM=TM,TF=TF)
  IGPG<-contrastinf(quote((TM/INDM-TF/INDF)/(TM/INDM)),list_all_tot)
  infun<-IGPG$lin
  names(infun) <- ind
  infuncomp <- complete(infun, ncom)
  if (comp) lin <- infuncomp else lin <- infun
  list(value = IGPG$value, lin = lin)
}

names(inf_fun) <- ind
inf_fun_comp <- complete(inf_fun, ncom)
if (comp)
  lin <- inf_fun_comp else lin <- inf_fun


# tests
library(survey)
library(vardpoor)
library(convey)
data(ses)
des_ses<- svydesign(id=~1, weights=~weights, data=ses,variables=~weights+sex+earningsHour+location)
ses_gpg<- svygpg(~earningsHour,des_ses, ~sex)
ses_gpg$value
SE_lin(ses_gpg,des_ses)

ses_gpg1<- svygpg1(~earningsHour, des_ses, ~sex, ncom = nrow(ses))
ses_gpg1$value
SE_lin(ses_gpg1,des_ses)

# test domain
gpgdom <- svyby(~earningsHour, by=~location, design = des_ses, FUN=svygpg1,
 sex=~sex, ncom = rownames(ses), comp=TRUE, deff=FALSE, keep.var=FALSE)

 table(ses$location)

 des_ses_AT1<- subset(des_ses, location=="AT1")
lixo<- svygpg1(~earningsHour, des_ses_AT1, ~sex, ncom = nrow(ses))
lixo$value
 SE_lin(lixo,des_ses)

  str(des_ses_AT1)
  str(des_ses)
 names(weights(des_ses_AT1))

 NROW(ses)

